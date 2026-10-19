(in-package "FSH")


(define-condition shell-error (simple-error) ())
(define-condition abort-fsh-command ()
  ((exit-code
    :initform 1
    :reader abort-fsh-command-exit-code
    :type (integer 0))))

(defun shell-error (control-string &rest args)
  (error 'shell-error :format-control control-string :format-arguments args))

(defun make-pipe ()
  (multiple-value-bind (in-fd out-fd) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream in-fd  :input t :buffering :none)
            (sb-sys:make-fd-stream out-fd :output t :buffering :none))))

(defun string-number (string)
  (multiple-value-bind (number length) (parse-integer string :radix 10 :junk-allowed t)
    (when (= length (length string))
      number)))

(defvar *shell-variables*
  (let ((tbl (make-hash-table :test 'equalp)))
    (setf (gethash "?" tbl) "0")
    (setf (gethash "$" tbl) (format nil "~d" (sb-posix:getpid)))
    (setf (gethash "IFS" tbl) (concatenate 'string '(#\Space #\Tab #\Newline)))
    tbl))

(defun shell-boundp (name)
  (declare (type string name))
  (setf name (coerce name 'simple-string))
  (or (gethash name *shell-variables*)
      (sb-posix:getenv name)))

(defun shell-variable (name)
  (declare (type string name))
  (or (shell-boundp name) ""))

(defun (setf shell-variable) (new name)
  (declare (type (or null string) new) (type string name))
  (setf name (coerce name 'simple-string))
  (if (sb-posix:getenv name)
      (progn
        (if new
            (sb-posix:setenv name new 1)
            (sb-posix:unsetenv name))
        new)
      (if new
          (setf (gethash name *shell-variables*) new)
          (progn (remhash name *shell-variables*) nil))))

(defvar *shell-functions* (make-hash-table :test 'equalp))

(defmacro define-fsh-command (name args &body body)
  (declare (type string name))
  (with-gensyms (block)
    `(setf (gethash ,name *shell-functions*)
           (lambda ,args
             (the (integer 0)
                  (block ,block
                    (handler-case
                        (progn ,@body)
                      (abort-fsh-command (c)
                        (return-from ,block (abort-fsh-command-exit-code c)))
                      (error (e)
                        (format t "~a~%" e)
                        (return-from ,block 1)))
                    0))))))

(defun probe-non-directory-file (pathspec)
  (let ((file (probe-file pathspec)))
    (when (and file (pathname-name file))
      file)))

(defun probe-directory-file (pathspec)
  (let ((dir (probe-file pathspec)))
    (unless (and dir (pathname-name dir))
      dir)))

(defun shell-fboundp (name)
  (declare (type string name))
  (let ((pathname (ignore-errors (pathname name))))
    (when (and pathname (not (wild-pathname-p pathname)))
      (if (let ((dir (pathname-directory pathname)))
            (or (eql :absolute (car dir))
                (and (eql :relative (car dir)) (or (equalp "." (cadr dir)) (eql :up (cadr dir))))))
          (probe-non-directory-file pathname)
          (or (gethash name *shell-functions*)
              (awhen (sb-posix:getenv "PATH")
                (loop for start = 0 then (1+ end)
                      for end = (or (position #\: it :start start) (length it))
                      do (let ((part (subseq it start end)))
                           (unless (string= "" part)
                             (awhen (probe-directory-file part)
                               (awhen (probe-non-directory-file
                                       (merge-pathnames pathname it))
                                 (return it)))))
                      until (eql end (length it)))))))))

(defun set-exit-code (code)
  (setf (shell-variable "?") (format nil "~d" code))
  code)

(defun compile-pattern (pattern)
  "Converts a PATTERN (as returned from PARSE-PATTERN) to one suitable
for MATCH-PATTERN."
  (apply #'concatenate 'vector
         (loop for elt in pattern
               collect (if (stringp elt) elt (list elt)))))

(defun match-pattern (string pattern &key (start 0) (end (length string)) (match-initial-dot t) from-end)
  "PATTERN must be an array of characters and the special symbols
MULTI-WILDCARD or SINGLE-WILDCARD"
  (if from-end
      (let ((length (length string)))
        (multiple-value-bind (matched end1 end2)
            (match-pattern (reverse string) (reverse pattern)
                           :start (- length end) :end (- length start)
                           :from-end nil :match-initial-dot match-initial-dot)
          (values matched (when end2 (- length end2)) (when end1 (- length end1)))))
      (loop with states = (list 0) and end1 = nil and end2 = nil
            for i from start
            do (loop until (eql states
                                (loop for state in states
                                      do (when (and (< state (length pattern))
                                                    (eql (aref pattern state) 'multi-wildcard))
                                           (pushnew (1+ state) states))
                                      finally (return states))))
               (when (member (length pattern) states)
                 (unless end1 (setf end1 i))
                 (setf end2 i))
               (when (<= end i)
                 (return (values (and (member (length pattern) states) t) end1 end2)))
               (let ((ch (aref string i)))
                 (setf states (loop for state in states
                                    nconc (when (< state (length pattern))
                                            (let ((elt (aref pattern state)))
                                              (if (characterp elt)
                                                  (when (char= elt ch) (list (1+ state)))
                                                  (when (or match-initial-dot (not (and (= i start) (eql ch #\.))))
                                                    (ecase elt
                                                      (multi-wildcard
                                                       (list state (1+ state)))
                                                      (single-wildcard
                                                       (list (1+ state))))))))))))))

(defun expand-file-pattern (file-pattern &aux (start :relative))
  "FILE-PATTERN represents a UNIX path decomposed into
components (separated by slashes).  It is a list, where each component
is either a string, matching a filename exactly, or a pattern suitable
for MATCH-PATTERN.

This function expands the given FILE-PATTERN and returns a list of
paths (strings) matching existing files.  The returned paths are in
the same format as is implied by the FILE-PATTERN, including extra
slashes and redundant dot-slashes."
  (when (equalp "" (car file-pattern))
    (pop file-pattern)
    (setf start :absolute))
  (labels ((directory-p (pathname)
             (and (null (pathname-name pathname)) (null (pathname-type pathname))))
           (pathname-head (pathname)
             (if (directory-p pathname)
                 (car (last (pathname-directory pathname)))
                 (format nil "~a~@[.~a~]" (pathname-name pathname) (pathname-type pathname))))
           (head-pathname (head)
             (let ((idx (position #\. head :from-end t)))
               (if (and idx (< 0 idx))
                   (make-pathname :name (subseq head 0 idx) :type (subseq head (1+ idx)))
                   (make-pathname :name head)))))
    (let ((paths (list (cons (make-pathname :directory (list start)) nil))))
      (loop for pattern in file-pattern
            do (setf paths (remove-if-not #'directory-p paths :key #'car))
               (if (equalp "" pattern)
                   (loop for cons in paths do (push "" (cdr cons)))
                   (setf paths
                         (loop for (path . components) in paths
                               append (if (stringp pattern)
                                          (let ((file (probe-file (merge-pathnames (head-pathname pattern) path))))
                                            (when file
                                              (list (cons file (cons pattern components)))))
                                          (let ((files (list* (merge-pathnames (make-pathname :directory '(:relative ".")) path)
                                                              (merge-pathnames (make-pathname :directory '(:relative "..")) path)
                                                              (directory (merge-pathnames path (make-pathname :name :wild :type :wild))
                                                                         :resolve-symlinks nil))))
                                            (loop for file in files for head = (pathname-head file)
                                                  when (match-pattern head pattern :match-initial-dot nil)
                                                    collect (cons file (cons head components)))))))))
      (loop for cons on paths do (setf (car cons) (format nil "~:[~;/~]~{~a~^/~}" (eql start :absolute) (reverse (cdar cons)))))
      paths)))

(defvar *dquote* nil)

(defun expand-parameter (part)
  "PART is a single component of a shell word, it can be a raw string
or a tagged list representing one of the following: ESCAPE, SQUOTE,
DQUOTE, PARAMETER[-OP], or BACKQUOTE-SUBSTITUTE.  Returns a PART which
is not a parameter or substitution.

DQUOTE parts contain another part which must be expanded
recursively (in the dquote context).

PARAMETER-OP parts may expand a word for substitution, in which case
the word should be expanded as a single string (without splitting or
pathname matching and in the same) and substituted directly, as if it
had been typed in the same dquote context."
  (if (consp part)
      (case (first part)
        (dquote `(dquote ,(let ((*dquote* t)) (expand-parameter (second part)))))
        (parameter
         (destructuring-bind (name) (cdr part)
           (if (numberp name)
               (or (nth name sb-ext:*posix-argv*) "")
               (shell-variable name))))
        (parameter-op
         (destructuring-bind (name op word) (cdr part)
           (let* ((variable (shell-variable name))
                  (op-char (aref op (1- (length op))))
                  (colon-p (eql #\: (aref op 0)))
                  (double-p (eql 2 (length op))))
             (ecase op-char
               (#\-
                (if (or (not (shell-boundp name)) (and colon-p (string= "" variable)))
                    (expand-word word :multi nil)
                    variable))
               (#\=
                (if (or (not (shell-boundp name)) (and colon-p (string= "" variable)))
                    (setf (shell-variable name) (expand-word word :multi nil))
                    variable))
               (#\?
                (if (or (not (shell-boundp name)) (and colon-p (string= "" variable)))
                    (shell-error "~a: ~a" name (expand-word word :multi nil))
                    variable))
               (#\+
                (if (or (not (shell-boundp name)) (and colon-p (string= "" variable)))
                    ""
                    (expand-word word :multi nil)))
               ((#\% #\#)
                (let ((string (shell-variable name)))
                  (multiple-value-bind (matched end1 end2)
                      (match-pattern string (expand-word-pattern word) :from-end (eql #\% op-char))
                    (declare (ignore matched))
                    (if end1
                        (ecase op-char
                          (#\% (if double-p (subseq string 0 end1) (subseq string 0 end2)))
                          (#\# (if double-p (subseq string end2) (subseq string end1))))
                        string))))))))
        (backquote-substitute
         (error "unimplemented BACKQUOTE-SUBSTITUTE"))
        (t part))
      part))

(defun expand-parameters (word)
  (loop for part in word collect (expand-parameter part)))

(defun split-fields (word)
  "WORD must be a list of parts as returned by EXPAND-PARAMETER.  This
function splits and trims the given word at any unquoted
whitespace (i.e. whitespace in any string part).  It returns a list of
words in the same format as WORD."
  (let ((tokens nil)
        (token nil))
    (flet ((end-token ()
             (when token
               (push (nreverse token) tokens)
               (setf token nil)))
           (push-elt (elt)
             (if (characterp elt)
                 (if (stringp (car token))
                     (vector-push-extend elt (car token))
                     (let ((string (make-dynamic-array :element-type 'character)))
                       (push string token)
                       (vector-push-extend elt string)))
                 (push elt token))))
      (loop for part in word
            do (if (stringp part)
                   (loop for char across part
                         do (if (member char '(#\Space #\Tab #\Newline))
                                (end-token)
                                (push-elt char)))
                   (push-elt part))
            finally (end-token))
      (nreverse tokens))))

(defun parse-pattern (string)
  "Parses string for shell pattern characters * and ?.  Returns a list of
segments, where each segment is a string not containing a pattern or
the symbols MULTI-WILDCARD and SINGLE-WILDCARD."
  (declare (type string string))
  (loop until (string= "" string)
        nconc (let ((idx (position-if (lambda (ch) (member ch '(#\* #\?))) string)))
                (if idx
                    (prog1 (list (subseq string 0 idx) (ecase (aref string idx) (#\* 'multi-wildcard) (#\? 'single-wildcard)))
                      (setf string (subseq string (1+ idx))))
                    (prog1 (list string) (setf string ""))))))

(defun pattern-file-pattern (pattern)
  "Transforms PATTERN (as returned by PARSE-PATTERN) into a
FILE-PATTERN (as defined in EXPAND-FILE-PATTERN) by splitting the raw
strings in PATTERN at the characters representing directory
separators (slash on UNIX)."
  (loop with token = nil
        while (or token (when pattern (setf token (pop pattern)) t))
        collect (loop with stop = nil until stop
                      collect (let ((idx (when (stringp token) (position #\/ token))))
                                (if idx
                                    (prog1 (subseq token 0 idx) (setf stop t token (subseq token (1+ idx))))
                                    (prog1 token (if pattern (setf token (pop pattern)) (setf stop t token nil))))))
          into ret
        finally (loop for cons on ret
                      do (if (loop for token in (car cons) always (stringp token))
                             (setf (car cons) (apply #'concatenate 'string (car cons)))
                             (setf (car cons) (compile-pattern (car cons)))))
                (return ret)))

(defun remove-quotes (word)
  "Flattens WORD (as returned by EXPAND-PARAMETER) into a string by
removing SQUOTE and DQUOTE elements, and expanding ESCAPE elements
according to the POSIX rules for escape characters (depends on if we
are in the context of a DQUOTE or not).  Returns a string."
  (if (stringp word)
      word
      (apply #'concatenate 'string
             (loop for part in word
                   collect (if (consp part)
                               (ecase (first part)
                                 (squote (second part))
                                 (dquote (let ((*dquote* t)) (remove-quotes (cdr part))))
                                 (escape
                                  (if *dquote*
                                      (case (second part)
                                        ((#\$ #\` #\" #\\) (string (second part)))
                                        (t (let ((ret (make-array 2 :element-type 'character)))
                                             (setf (aref ret 0) #\\ (aref ret 1) (second part))
                                             ret)))
                                      (string (second part)))))
                               part)))))

(defun remove-quotes-pattern (word)
  "Flattens WORD (as returned by EXPAND-PARAMETER) into a PATTERN
(as returned by PARSE-PATTERN).  Performs the same operations as
REMOVE-QUOTES except that unquoted wildcard patterns are extracted."
  (let ((tokens (loop for part in word
                      nconc (if (stringp part) (parse-pattern part) (list part)))))
    ;; Check if there is actually a wildcard to expand
    (loop while tokens
          collect (let ((chunk (loop for cons on tokens
                                     until (member (car cons) '(multi-wildcard single-wildcard))
                                     collect (car cons)
                                     finally (setf tokens cons))))
                    (if chunk
                        (remove-quotes chunk)
                        (prog1 (car tokens) (setf tokens (cdr tokens))))))))

(defun expand-pathname (word)
  "WORD must be a list of parts as returned by EXPAND-PARAMETER.  This
function attempts to transform WORD into a pattern containing
wildcards.  If there are no wildcards, or if the wildcards do not
match any files, the WORD is interpreted as a normal string (as per
POSIX specification for pathname expansion).  Returns a list of
strings."
  (assert (null *dquote*))
  (let ((pattern (remove-quotes-pattern word)))
    (if (loop for part in pattern thereis (not (stringp part)))
        (expand-file-pattern (pattern-file-pattern pattern))
        (list (apply #'concatenate 'string pattern)))))

(defun expand-pathnames (words)
  (loop for word in words nconc (expand-pathname word)))

(defun expand-tilde (word &aux idx)
  "Expand WORD (same format as input to EXPAND-PARAMETER) according to
the POSIX rules for Tilde Expansion: if WORD starts with an unescaped
tilde, it is expanded to the user's $HOME directory."
  (let ((first (first word)))
    (if (and (stringp first) (< 0 (length first)) (eql #\~ (aref first 0))
             (or (setf idx (position #\/ first)) (= 1 (length word))))
        (let* ((user (subseq first 1 (or idx (length first))))
               (expansion (if (equalp "" user) (shell-variable "HOME") (error "unimplemented: named home")))
               (concat (if idx (concatenate 'string expansion (subseq first idx)) expansion)))
          (if (equalp "" concat) (cdr word) (cons concat (cdr word))))
        word)))

(defun expand-word-pattern (word)
  (compile-pattern (remove-quotes-pattern (expand-parameters (expand-tilde word)))))

(defun expand-word (word &key (split-fields t) (expand-pathnames t) (multi t))
  (unless multi
    (setf split-fields nil) (setf expand-pathnames nil))
  (let ((ret (expand-parameters (expand-tilde word))))
    (if split-fields
        (setf ret (split-fields ret))
        (setf ret (list ret)))
    (if expand-pathnames
        (setf ret (expand-pathnames ret))
        (setf ret (mapcar #'remove-quotes ret)))
    (if multi
        ret
        (progn (assert (= 1 (length ret))) (first ret)))))

(defun expand-words (words)
  (loop for arg in words nconc (expand-word arg)))

(defun shell-escape (string)
  (with-output-to-string (s)
    (loop for ch across string
          do (cond ((member ch '(#\& #\| #\< #\> #\; #\Space #\Tab #\' #\" #\\ #\$ #\( #\* #\?))
                    (format s "\\~c" ch))
                   ((eql ch #\Newline)
                    (format s "'~%'"))
                   (t (write-char ch s))))))

;; TODO: aliases
;; TODO: $@ and $*, other special parameters
;; TODO: $(...) syntax, leave unimplemented
;; TODO: arithmetic expansion
;; TODO: here-document
;; TODO: [...] patterns

;; TODO: figure out how to let backgrounds echo nicely while in repl (ensure tostop?/SIGTTOU)? figure out SIGWINCH in repl

(sb-alien:define-alien-routine ("init_shell" %init-shell) sb-alien:void)
(sb-alien:define-alien-routine ("write_sigchld" write-sigchld) sb-alien:int)
(sb-alien:define-alien-routine ("handle_sigchld" %handle-sigchld) sb-alien:void)
(sb-alien:define-alien-routine ("hangup" hangup) sb-alien:void)
(sb-alien:define-alien-routine ("make_command" make-command)
    (* sb-alien:char) (pathname (* sb-alien:char)) (argv (* (* sb-alien:char))) (envp (* (* sb-alien:char))) (redirects (* (* sb-alien:char))))
(sb-alien:define-alien-routine ("make_redirect" %make-redirect)
    (* sb-alien:char) (type sb-alien:int) (fd sb-alien:int) (word (* sb-alien:char)))
(sb-alien:define-alien-routine ("run_pipeline" %run-pipeline) sb-alien:int (p (* (* sb-alien:char))) (wait sb-alien:int))
(sb-alien:define-alien-routine ("jobs_length" jobs-length) sb-alien:int)
(sb-alien:define-alien-routine ("job_pgid" job-%pgid) sb-alien:int (n sb-alien:int))
(sb-alien:define-alien-routine ("job_status" job-%status) sb-alien:int (n sb-alien:int))
(sb-alien:define-alien-routine ("job_remove" job-%remove) sb-alien:int (n sb-alien:int))
(sb-alien:define-alien-routine ("apply_redirect" %apply-redirect) sb-alien:int (redirect (* sb-alien:char)) (keep sb-alien:int))
(sb-alien:define-alien-routine ("free_redirect" %free-redirect) sb-alien:void (redirect (* sb-alien:char)))

(macrolet ((def (name %name args)
             `(defun ,name ,args
                (let ((ret (,%name n)))
                  (when (= -1 ret)
                    (error "invalid job"))
                  ret))))
  (def job-pgid job-%pgid (n))
  (def job-status job-%status (n))
  (def job-remove job-%remove (n)))

(defun init-shell ()
  (%init-shell)
  (sb-sys:enable-interrupt sb-posix:sigchld (lambda (&rest rest)
                                              (declare (ignore rest))
                                              (when (= (write-sigchld) -1)
                                                (fed::write-signal sb-posix:sigchld))))
  (push #'hangup sb-ext:*exit-hooks*)
  (sb-sys:enable-interrupt sb-posix:sighup (lambda (&rest rest)
                                             (declare (ignore rest))
                                             (sb-ext:exit :code 1))))

(defun make-ptr-array (ptrs)
  (let ((ret (sb-alien:make-alien (* sb-alien:char) (1+ (length ptrs)))))
    (loop for ptr in ptrs and i from 0
          do (setf (sb-alien:deref ret i) ptr))
    (setf (sb-alien:deref ret (length ptrs)) (sb-sys:int-sap 0))
    ret))

(defun make-alien-string-array (strings)
  (make-ptr-array (mapcar #'sb-alien:make-alien-string strings)))

(defun make-pipeline (commands)
  (make-ptr-array commands))

(defun make-redirect (type fd word)
  (let* ((type (or (position type #(input output output-clobber output-append
                                    input-output input-copy output-copy copy))
                   (error "invalid redirect type: ~s" type)))
         (word (sb-alien:make-alien-string (expand-word word :multi nil)))
         (ret (%make-redirect type fd word)))
    (if (sb-alien:null-alien ret)
        (error "make-redirect error")
        ret)))

(defun apply-redirect (type fd word &optional keep)
  (let ((redirect (make-redirect type fd word)))
    (unwind-protect
         (let ((ret (%apply-redirect redirect (if keep 1 0))))
           (if (<= 0 ret)
               ret
               (ecase ret
                 (-1 (sb-posix:syscall-error 'apply-redirect))
                 (-2 (error "~s is not a valid file descriptor" word))
                 (-3 (error "fd ~a is not open for ~:[writing~;reading~]" word (eql 'input-copy type))))))
      (%free-redirect redirect))))

(defun check-env-name (name)
  (assert (not (find #\= name)) nil "Name may not contain equals sign"))

(defmacro with-shell-opts (opts &body body)
  (let ((bindings (gensym "bindings"))
        (redirects (gensym "redirects")))
    `(let ((,bindings nil)
           (,redirects nil))
       (unwind-protect
            (progn
              (loop for (type . rest) in ,opts
                    collect (ecase type
                              (:env
                               (destructuring-bind (name word) rest
                                 (check-env-name name)
                                 (push (cons name (shell-variable name)) ,bindings)
                                 (setf (shell-variable name) (expand-word word :multi nil))))
                              (:redirect
                               (destructuring-bind (type fd word) rest
                                 (push (cons fd (apply-redirect type fd word t)) ,redirects)))))
              ,@body)
         (loop for (name . old-value) in ,bindings
               do (ignore-errors (setf (shell-variable name) old-value)))
         (loop for (fd . old-fd) in ,redirects
               do (ignore-errors (apply-redirect 'copy fd (list (format nil "~d" old-fd))))
                  (ignore-errors (sb-posix:close old-fd)))))))

(defun run-pipeline (commands &key background)
  "Each command in COMMANDS is a list of the form (OPTS . ARGS)."
  (unless commands
    (return-from run-pipeline nil))
  (setf commands (loop for (opts . args) in commands
                       collect (if args
                                   (list* (shell-fboundp (car args)) opts args)
                                   (list 'null-command opts))))
  (cond ((and (= 1 (length commands)) (functionp (caar commands)))
         (with-shell-opts (cadar commands)
           (set-exit-code (apply (caar commands) (cdddar commands)))))
        ((and (= 1 (length commands)) (eql 'null-command (caar commands)))
         (let ((opts (cadar commands)))
           (loop for (type . rest) in opts
                 do (ecase type
                      (:env
                       (destructuring-bind (name word) rest
                         (check-env-name name)
                         (setf (shell-variable name) (expand-word word :multi nil))))
                      (:redirect
                       (apply #'apply-redirect rest))))
           (set-exit-code 0)))
        (t
         (set-exit-code
          (%run-pipeline
           (make-pipeline
            (loop for (pathname opts cmd . args) in commands
                  collect (etypecase pathname
                            (null (shell-error "~a: command not found" cmd))
                            (function (unimplemented "shell functions in pipelines"))
                            ((eql null-command) (unimplemented "null commands in pipelines"))
                            (pathname
                             (loop for (type . rest) in opts
                                   if (eql :redirect type)
                                     collect (apply #'make-redirect rest) into redirects
                                   else
                                     if (eql :env type)
                                       collect (destructuring-bind (name word) rest
                                                 (check-env-name name)
                                                 (concatenate 'string name "=" (expand-word word :multi nil)))
                                         into bindings
                                   else do (unreachable)
                                   finally (return
                                             (make-command (sb-alien:make-alien-string (namestring pathname))
                                                           (make-alien-string-array (list* cmd args))
                                                           (make-alien-string-array bindings)
                                                           (make-ptr-array redirects))))))))
           (if background 0 1))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-background (list)
    (let ((background nil))
      (values (loop while list
                    if (eql :background (car list))
                      do (pop list) (setf background (pop list))
                    else collect (pop list))
              background))))

(defmacro run-command (&rest words-and-options)
  "A WORD is either a string or a list, while an OPTION is a symbol/value
pair like a keyword argument."
  (multiple-value-bind (words-and-options background) (parse-background words-and-options)
    `(shell-pipe (run-command ,@words-and-options) ,@(when background '(:background t)))))

(defmacro shell-pipe (&rest commands-and-options)
  (multiple-value-bind (commands background) (parse-background commands-and-options)
    `(run-pipeline
      (list ,@(loop for command in commands
                    do (assert (and (listp command) (listp (cdr command)) (eql 'run-command (first command))) nil
                               "Invalid command in pipeline: ~s" command)
                    collect (let ((words-and-options (cdr command)))
                              (loop while words-and-options
                                    if (or (listp (car words-and-options))
                                           (stringp (car words-and-options)))
                                      collect (pop words-and-options) into words
                                    else
                                      collect (let ((key (pop words-and-options)))
                                                (assert (member key '(:redirect :env)) nil "invalid option ~s" key)
                                                (cons key (pop words-and-options)))
                                        into opts
                                    finally (return `(list* ',opts ,(when words `(expand-words ',words))))))))
      ,@(when background '(:background t)))))

(defmacro shell-progn (&body commands)
  (if commands
      `(progn ,@commands)
      (set-exit-code 0)))

(defmacro shell-and (&rest commands)
  (case (length commands)
    (0 0)
    (1 (first commands))
    (t (let ((code (gensym "CODE")))
         `(let ((,code (shell-and ,@(butlast commands))))
            (if (eql 0 ,code)
                ,(car (last commands))
                ,code))))))

(defmacro shell-or (&rest commands)
  (case (length commands)
    (0 1)
    (1 (first commands))
    (t `(if (eql 0 ,(first commands))
            0
            (shell-or ,@(rest commands))))))

(defmacro shell-not (command)
  `(set-exit-code (if (eql 0 ,command) 1 0)))

(defmacro shell-if (condition then else)
  `(if (eql 0 ,condition) ,then ,else))

(defmacro shell-case (word &body cases)
  (let ((word-sym (gensym "WORD")))
    (labels ((expand-cases (cases)
               (if cases
                   (destructuring-bind (patterns &rest body) (first cases)
                     `(if (or ,@(loop for pattern in patterns
                                      collect `(match-pattern ,word-sym (expand-word-pattern ',pattern))))
                          (shell-progn ,@body)
                          ,(expand-cases (cdr cases))))
                   `(set-exit-code 0))))
      `(let ((,word-sym (expand-word ',word :multi nil)))
         ,(expand-cases cases)))))

(defmacro shell-for (name words &body body)
  (let ((code (gensym "CODE"))
        (word (gensym "WORD"))
        (name-sym (gensym "NAME")))
    `(let ((,name-sym ,name))
       (loop :with ,code := 0
             :for ,word :in ,(when words `(expand-words ',words))
             :do (setf (shell-variable ,name-sym) ,word)
                 (setf ,code (shell-progn ,@body))
             :finally (return (set-exit-code ,code))))))

(defmacro shell-while (condition &body body)
  (let ((code (gensym "CODE")))
    `(loop :with ,code := 0
           :while (eql 0 ,condition)
           :do (setf ,code (shell-progn ,@body))
           :finally (return (set-exit-code ,code)))))

(defmacro shell-until (condition &body body)
  (let ((code (gensym "CODE")))
    `(loop :with ,code := 0
           :until (eql 0 ,condition)
           :do (setf ,code (shell-progn ,@body))
           :finally (return (set-exit-code ,code)))))

(defmacro subshell (command)
  (declare (ignore command))
  (unimplemented))

(defmacro shell-defun (&rest args)
  (declare (ignore args))
  (unimplemented))

(define-fsh-command "jobs" ()
  (let ((done nil))
    (loop for i below (jobs-length)
          do (let ((status (job-status i)))
               (format t "[~d] (~d) ~{~a~^/~}~%" i (job-pgid i)
                       (append (unless (= 0 (logand status #b100))
                                 (list "Running"))
                               (unless (= 0 (logand status #b010))
                                 (list "Stopped"))
                               (unless (= 0 (logand status #b001))
                                 (list "Done"))))
               (when (= status #b001)
                 (push i done))))
    ;; TODO: There may be a memory leak issue here since jobs are not
    ;; removed until they are printed, if the user backgrounds many
    ;; jobs without calling "jobs" then they will remain forever
    ;; (print changes after before every prompt?).
    
    ;; NOTE: This needs to be in reverse order.
    (loop for i in done
          do (job-remove i))))

(define-fsh-command "cd" (arg)
  (aif (probe-directory-file arg)
       (progn
         (sb-posix:chdir it)
         (setf *default-pathname-defaults* it))
       (shell-error "fsh: ~a: No such file or directory" arg)))

;; TODO: change name to "edit"
(define-fsh-command "e" (&rest args)
  (loop for arg in args do (fed:find-file arg)))
