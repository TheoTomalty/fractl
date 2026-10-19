(in-package "FED")


(define-default-grammar "lisp" 'ftx::lambda-main 'ftx::lambda-read)

(define-group-attributes ftx::lambda-line-comment '(:foreground :bright-black))
(define-group-attributes ftx::lambda-block-comment '(:foreground :bright-black))
(define-group-attributes ftx::lambda-unmatched-paren '(:background :red))
(define-rule-attributes ftx::lambda-number '(:foreground :magenta))
(define-rule-attributes ftx::lambda-string '(:foreground :green))
(define-rule-attributes ftx::lambda-character '(:foreground :magenta))
(define-rule-attributes ftx::lambda-bitvector '(:foreground :magenta))
(define-rule-attributes ftx::lambda-uninterned '(:foreground :orange))
(define-rule-attributes ftx::lambda-radix '(:foreground :magenta))
(define-rule-attributes ftx::lambda-binary '(:foreground :magenta))
(define-rule-attributes ftx::lambda-octal '(:foreground :magenta))
(define-rule-attributes ftx::lambda-hex '(:foreground :magenta))

(define-rule-evaluator ftx::lambda-symbol paint
  (let ((tok (ignore-errors (eval-ast ftx::token))))
    (when tok
      (cond ((and (= 1 (length tok))
                  (stringp (first tok))
                  (or (string= (string-upcase (first tok)) "T")
                      (string= (string-upcase (first tok)) "NIL")))
             '(:foreground :magenta))
            ((and (stringp (first tok)) (char= #\& (aref (first tok) 0)))
             '(:foreground :yellow))
            ((and (consp (first tok)) (eql 'ftx::colons (first (first tok))))
             '(:foreground :orange))))))

(define-rule-evaluator ftx::lambda-symbol nointern-symbol
  (ignore-errors (ftx::token-symbol (ignore-errors (eval-ast ftx::token)) t)))

(define-rule-evaluator ftx::lambda-list paint
  (when (and (boundp 'ftx::sexprs) ftx::sexprs)
    (let* ((node (first ftx::sexprs))
           (symbol (eval-ast node 'nointern-symbol))
           (name (second ftx::sexprs)))
      (when (or (special-operator-p symbol) (macro-function symbol))
        (lambda (paint)
          (funcall paint node '(:foreground :red))
          (when (and name (eql (ast-rule name) 'ftx::lambda-symbol))
            (case symbol
              ((defun defmacro)
               (funcall paint name '(:foreground :blue :bold t)))
              ((defvar defparameter defconstant)
               (funcall paint name '(:foreground :cyan)))
              ((deftype defstruct defclass defcondition)
               (funcall paint name '(:foreground :blue :italic t)))))
          )))))

(define-rule-evaluator ftx::lambda-list extract-package
  (when (and (boundp 'ftx::sexprs)
             ftx::sexprs
             (eql 'in-package (eval-ast (first ftx::sexprs) 'nointern-symbol))
             (cdr ftx::sexprs))
    (let ((*read-eval* nil))
      (ignore-errors (find-package (eval-ast (second ftx::sexprs)))))))

(define-rule-evaluator ftx::lambda-main extract-package
  (when (boundp 'ftx::sexprs)
    (loop for node in ftx::sexprs
          do (awhen (eval-ast node 'extract-package)
               (return it)))))

(define-rule-evaluator ftx::lambda-symbol complete-function #'lambda-complete)

(define-rule-evaluator ftx::lambda-list comment-prefix ";;")
(define-rule-evaluator ftx::lambda-main comment-prefix ";;")

(define-rule-evaluator ftx::lambda-number number-formatter #'princ-to-string)
(define-rule-evaluator ftx::lambda-binary number-formatter (lambda (n) (format nil "#b~b" n)))
(define-rule-evaluator ftx::lambda-octal number-formatter (lambda (n) (format nil "#o~o" n)))
(define-rule-evaluator ftx::lambda-hex number-formatter (lambda (n) (format nil "#x~x" n)))
(define-rule-evaluator ftx::lambda-radix number-formatter
  (let ((radix (or (eval-ast ftx::prefix) 10)))
    (lambda (n) (format nil "#~dr~v,r" radix radix n))))

(define-rule-evaluator ftx::lambda-string formatter
  (lambda (node buffer)
    (let ((str (handler-case (eval-ast node) (error (e) (editor-error "~a" e)))))
      (format nil "~s" (format-paragraph-aux str (buffer-variable buffer 'fill-column))))))

(defvar *fcl-keymap* (copy-keymap *global-keymap*))
(define-key *fcl-keymap* "RET" (:insert :normal :visual) fcl-accept)
(define-key *fcl-keymap* "C-d" (:insert) fcl-ctrl-d)
(define-key *fcl-keymap* "M-p" (:insert :normal) repl-prev-history)
(define-key *fcl-keymap* "M-n" (:insert :normal) repl-next-history)

(defun fcl-prompt (buffer)
  (list (format nil "𝜆~@[[~d]~] " (buffer-variable buffer 'debug-level))
        :foreground
        (if (buffer-variable buffer 'fcl-eval-failed-p) :red :green)))

(defun fcl-repl (&optional (switch-p t))
  (let* ((frame (repl-frame))
         (window (repl-frame-window frame))
         (buffer-name (format nil "*fcl~@[[~d]~]*" *debug-level*)))
    (aif (find buffer-name (window-buffers window) :key #'buffer-name :test #'equal)
         (if switch-p (values (push-buffer it window)) it)
         (let ((buffer (create-buffer buffer-name :window window :default-insert t)))
           (setf (buffer-variable buffer 'debug-level) *debug-level*
                 (buffer-variable buffer 'fcl-eval-failed-p) nil
                 (buffer-grammar buffer) (default-grammar "lisp")
                 (buffer-keymap buffer) *fcl-keymap*)
           (init-repl buffer :prompt #'fcl-prompt)
           (when switch-p
             (switch-to-frame frame))
           buffer))))

(defun fcl-repl-name-p (str)
  (cond ((string= "*fcl*" str)
         (values t nil))
        ((and (< 7 (length str))
              (string= "*fcl[" (subseq str 0 5))
              (string= "]*" (subseq str (- (length str) 2))))
         (multiple-value-bind (n idx)
             (parse-integer str :start 5 :end (- (length str) 2)
                                :radix 10 :junk-allowed t)
           (if (= idx (- (length str) 2))
               (values t n)
               (values nil nil))))
        (t (values nil nil))))

(defun fcl-clear-debug-repls-hook ()
  (let ((buffers (loop for b in (buffers)
                       when (multiple-value-bind (fcl-p debug-level) (fcl-repl-name-p (buffer-name b))
                              (and fcl-p (if (integerp *debug-level*)
                                             (and debug-level (< *debug-level* debug-level))
                                             debug-level)))
                         collect b)))
    (loop for buffer in buffers
          do (delete-buffer buffer))))

(add-default-editor-hook 'post-command 'fcl-clear-debug-repls-hook)

(define-editor-command fcl-accept
    (:cursor cursor)
  (repl-accept-eval (string cursor)
    (let* ((buffer (cursor-buffer cursor))
           (reader (buffer-reader-strict buffer))
           (failed t))
      (unwind-protect
          (aif (and *debug-level* (ignore-errors (parse-integer string :radix 10)))
               (awhen (nth it *debug-restarts*)
                      (invoke-restart-interactively it))
               (let ((values (multiple-value-list (evm-eval string reader))))
                 (format t "~&~a~{~s~%~}~a" (csi-text-foreground :magenta nil) values (csi-text-reset nil))
                 (setf failed nil)))
        (setf (buffer-variable buffer 'fcl-eval-failed-p) failed)))))

(define-editor-command fcl-ctrl-d
    (:cursor cursor :modes (:insert) :n n :register r :record (:insert :record)
     :keys ((merge (and (toplevel-p) (eql 'fcl-ctrl-d *previous-binding*)))))
  (if (and (= 0 (cursor-pos cursor)) (point-eob-p (cursor-point cursor)))
      (aif *debug-default-restart*
           (invoke-restart-interactively it)
           (editor-error "No default restart"))
      (delete-forward-char :n n :register r :merge merge)))

(defun enclosing-sexpr-list (node point)
  (point-enclosing-node-in point node (lambda (node) (member (ast-rule node) '(ftx::lambda-list ftx::lambda-string)))
                           :inclusive nil
                           :trace t))

(defvar *indent-symbols* (make-hash-table))

(macrolet ((def (type &rest symbols)
             `(loop for symbol in ',symbols
                    do (setf (gethash symbol *indent-symbols*) ',type))))
  (def (:body 0)
    locally return with-standard-io-syntax)
  (def (:body 1)
    typecase etypecase ctypecase case ecase ccase handler-bind handler-case restart-bind restart-case unwind-protect
    with-input-from-string with-output-to-string with-hash-table-iterator with-package-iterator
    with-compilation-unit with-open-file with-open-stream with-simple-restart
    dolist dotimes do-symbols do-external-symbols do-all-symbols
    defvar defparameter defconstant defpackage define-symbol-macro defstruct
    prog prog1 block catch throw lambda eval-when when unless return-from
    let let* flet labels macrolet symbol-macrolet
    multiple-value-prog1 multiple-value-call
    print-unreadable-object pprint-logical-block)
  (def (:body 2)
    progv prog2 do do*
    defun defmacro defgeneric deftype defclass define-condition
    define-compiler-macro define-setf-expander define-modify-macro
    destructuring-bind multiple-value-bind
    with-accessors with-slots with-condition-restarts)
  (def (:tagbody 0) tagbody)
  (def (:tagbody 1) prog*))

(defun indent-function (n sexprs)
  (declare (type (integer 1) n))
  (values (if (= n 1) (first sexprs) (second sexprs)) 0))

(defun indent-progn (n sexprs)
  (declare (type (integer 1) n))
  (if (= n 1)
      (values nil 2)
      (values (second sexprs) 0)))

(defun indent-body (n-args n)
  (declare (type (integer 1) n))
  (values nil (if (<= n n-args) 4 2)))

(defun indent-tagbody (n-args n sexprs)
  (declare (type (integer 1) n))
  (if (<= n n-args)
      (values nil 4)
      (let ((node (nth n sexprs)))
        (if (and node (eql 'ftx::lambda-symbol (ast-rule node)))
            (values nil 1)
            (values nil 3)))))

(defun indent-loop (n sexprs)
  (declare (type (integer 1) n))
  (cond ((= 1 n)
         (values nil 2))
        ((eql 'ftx::lambda-list (ast-rule (second sexprs)))
         ;; Simple loop
         (values (second sexprs) 0))
        (t
         (let ((body-node nil)
               (body-offset nil)
               (body-node-is-word-p nil))
           (loop for i from 1 for sexpr in (rest sexprs)
                 do (multiple-value-bind (rule foundp) (ast-rule sexpr)
                      (unless (eql 'ftx::lambda-list rule)
                        (setf body-node nil
                              body-offset nil))
                      (when (eql n i)
                        (return))
                      (when foundp
                        (case rule
                          (ftx::lambda-symbol
                           (let* ((token (eval-ast (ast-variable sexpr 'ftx::token)))
                                  (start (aif (position-if #'ftx::colons-part-p token :from-end t)
                                              (1+ it)
                                              0))
                                  (string (or (ignore-errors (ftx::token-string (subseq token start))) "")))
                             (when (member string '("DO" "INITIALLY" "FINALLY") :test #'string=)
                               (setf body-node sexpr
                                     body-offset 2
                                     body-node-is-word-p t))))
                          (ftx::lambda-list
                           (when body-node-is-word-p
                             (setf body-node sexpr
                                   body-offset 0
                                   body-node-is-word-p nil)))))))
           (if body-node
               (values body-node body-offset)
               (values (second sexprs) 0))))))

(defun indent-defmethod (n sexprs)
  (declare (type (integer 1) n))
  (if (loop for sexpr in (rest sexprs) and i from 1 below n
            always (eql 'ftx::lambda-symbol (ast-rule sexpr)))
      (values nil 4)
      (values nil 2)))

(defun indent-defsetf (n sexprs)
  (declare (type (integer 1) n))
  (if (and (cddr sexprs) (eql 'ftx::lambda-list (ast-rule (third sexprs))))
      (indent-body 3 n)
      (indent-body 2 n)))

(defun indent-define-method-combination (n sexprs)
  (declare (type (integer 1) n))
  (if (and (cddr sexprs) (eql 'ftx::lambda-list (ast-rule (third sexprs))))
      (indent-body 3 n)
      (indent-body 1 n)))

(defun split-sexprs (point sexprs)
  (loop for node in sexprs and i from 0
        do (when (point< point (ast-offset node))
             (return
               (values i (if (= (point-line point)
                                (point-line (ast-origin node)))
                             sexprs
                             (subseq sexprs 0 i)))))
        finally (return
                  (values (length sexprs) sexprs))))

(defun lambda-indent (buffer point &aux (ast (buffer-ast buffer)))
  ;; TODO: avoid parsing buffer on each line changed by returning node-relative indentation and applying indents in bulk
  ;; TODO: keyword arguments (if function accepts &key) should be aligned with each other
  ;; TODO: lambda-list indentation
  ;; TODO: handle lexical definitions
  (multiple-value-bind (list-node parents) (awhen ast (enclosing-sexpr-list it point))
    (if (and list-node (eql 'ftx::lambda-list (ast-rule list-node)))
        (multiple-value-bind (n sexprs) (split-sexprs point (ast-variable list-node 'ftx::sexprs))
          (multiple-value-bind (node offset)
              (if (= 0 n)
                  (values nil 1)
                  (let* ((symbol (with-buffer-package buffer
                                   (eval-ast (first sexprs) 'nointern-symbol)))
                         (lambda-list (when symbol
                                        (handler-case (sb-introspect:function-lambda-list symbol)
                                          (undefined-function ()))))
                         (indent-symbols-val (gethash symbol *indent-symbols*)))
                    (cond
                      ((and (cdr parents)
                            (eql 'ftx::lambda-list (ast-rule (first parents)))
                            (eql 'ftx::lambda-list (ast-rule (second parents)))
                            (awhen (ast-variable (second parents) 'ftx::sexprs)
                              (and (member (eval-ast (first it) 'nointern-symbol) '(flet labels macrolet))
                                   (eql 1 (position (first parents) it)))))
                       (indent-body 1 n))
                      ((and (cdr parents)
                            (eql 'ftx::lambda-list (ast-rule (first parents)))
                            (eql 'ftx::lambda-list (ast-rule (second parents)))
                            (awhen (ast-variable (second parents) 'ftx::sexprs)
                              (and (member (eval-ast (first it) 'nointern-symbol) '(let let* symbol-macrolet))
                                   (eql 1 (position (first parents) it)))))
                       (indent-body 0 n))
                      ((and parents
                            (eql 'ftx::lambda-list (ast-rule (first parents)))
                            (awhen (ast-variable (first parents) 'ftx::sexprs)
                              (and (member (eval-ast (first it) 'nointern-symbol) '(handler-case restart-case))
                                   (awhen (position list-node it) (<= 2 it)))))
                       (indent-body 1 n))
                      ((not (eql 'ftx::lambda-symbol (ast-rule (first sexprs))))
                       (if (member (ast-rule (first sexprs)) '(ftx::lambda-unquote-1 ftx::lambda-unquote-2 ftx::lambda-unquote-3))
                           (indent-function n sexprs)
                           (values nil 1)))
                      ((awhen (ignore-errors (eval-ast (ast-variable (first sexprs) 'ftx::token)))
                         (or (and (stringp (first it)) (char= #\& (aref (first it) 0)))
                             (and (consp (first it)) (eql 'ftx::colons (first (first it))))))
                       (values nil 1))
                      ((or (eql 'progn symbol) (eql 'cond symbol))
                       (indent-progn n sexprs))
                      ((eql 'loop symbol)
                       (indent-loop n sexprs))
                      ((eql 'defmethod symbol)
                       (indent-defmethod n sexprs))
                      ((eql 'defsetf symbol)
                       (indent-defsetf n sexprs))
                      ((eql 'define-method-combination symbol)
                       (indent-define-method-combination n sexprs))
                      (indent-symbols-val
                       (destructuring-bind (type n-args) indent-symbols-val
                         (ecase type
                           (:body (indent-body n-args n))
                           (:tagbody (indent-tagbody n-args n sexprs)))))
                      ((member '&body lambda-list)
                       (indent-body (position-if (lambda (entry) (member entry lambda-list-keywords)) lambda-list) n))
                      (t
                       (indent-function n sexprs)))))
            (+ (coord-x (point-vcoord (ast-origin (or node list-node))))
               offset)))
        (when (and ast (eql 'ftx::lambda-main (ast-symbol ast)))
          0))))

(fsh:define-fsh-command "indent-test" (&rest files)
  (loop for file in files
        do (format t "~a~%" file)
           (let ((buffer (create-file-buffer file)))
             (unwind-protect
                 (loop with point = (text-pos-point (buffer-text buffer) 0)
                       for i from 1
                       repeat (buffer-nlines buffer)
                       do (let ((actual (lambda-indent buffer point))
                                (expected (loop with point = (copy-point point)
                                                while (eql #\Space (point-peek point))
                                                count (point-next point))))
                            (unless (eql expected actual)
                              (format t "~d: expected ~d, got ~d~%" i expected actual)))
                          (setf point (point-beginning-of-line (point-end-of-line point 2))))
               (delete-buffer buffer)))
           (finish-output)))

(defun lambda-complete-internal (package string externalp)
  (let* ((parts (apply #'vector
                       (loop for start = 0 then (1+ end)
                             until (< (length string) start)
                             for end = (or (position #\- string :start start) (length string))
                             collect (subseq string start end))))
         (min-parts nil)
         (symbols
           (loop for symbol being the symbols of package
                 for name = (symbol-name symbol)
                 when (and (or (not externalp) (eql :external (nth-value 1 (find-symbol (symbol-name symbol) package))))
                           (<= (1- (length parts)) (count #\- name))
                           (loop for i from 0
                                 for start = 0 then (1+ end)
                                 until (< (length name) start)
                                 for end = (or (position #\- name :start start) (length name))
                                 always (or (not (< i (length parts)))
                                            (let ((part (aref parts i)))
                                              (and (<= (length part) (- end start))
                                                   (loop for i from start below end and j below (length part)
                                                         always (char= (aref name i) (aref part j))))))
                                 finally (setf min-parts (if min-parts (min i min-parts) i))))
                   collect symbol)))
    (when min-parts
      (loop with prefixes = (make-array min-parts :initial-element nil)
            and non-unique = (make-array min-parts :initial-element nil)
            for symbol in symbols
            for name = (symbol-name symbol)
            do (loop for i from 0 below min-parts
                     for start = 0 then (1+ end)
                     until (< (length name) start)
                     for end = (or (position #\- name :start start) (length name))
                     do (aif (aref prefixes i)
                             (loop for j from start below (min end (+ (length it) start))
                                   do (unless (eql (aref name j) (aref it (- j start)))
                                        (setf (aref prefixes i) (subseq name start j)
                                              (aref non-unique i) t)
                                        (return))
                                   finally (when (< (- end start) (length it))
                                             (setf (aref non-unique i) t)))
                             (setf (aref prefixes i) (subseq name start end))))
            finally (return (values (format nil "~{~a~^-~}" (coerce prefixes 'list))
                                    (aif (position t non-unique)
                                         (- (1- (length non-unique)) it)
                                         0)
                                    symbols))))))

(defun parse-symbol-token (token)
  ;; TODO: merge this with token-symbol logic in lambda.lisp
  (unless (and (<= (count-if #'ftx::colons-part-p token) 1)
               (aif (find-if #'ftx::colons-part-p token)
                    (<= (second it) 2)
                    t))
    (editor-error "Malformed package designator"))
  (let* ((colons-pos (position-if #'ftx::colons-part-p token))
         (ncolons (when colons-pos (second (nth colons-pos token))))
         (externalp (when colons-pos (= 1 ncolons)))
         (package-string (when colons-pos (ftx::token-string (subseq token 0 colons-pos))))
         (package (if colons-pos
                      (if (eql 0 (length package-string))
                          (find-package "KEYWORD")
                          (find-package package-string))
                      *package*))
         (string (ftx::token-string (if colons-pos (subseq token (1+ colons-pos)) token))))
    (unless package
      (editor-error "Package does not exist"))
    (values package string externalp package-string ncolons)))

(defun lambda-complete (cursor node)
  (lambda-complete-token cursor (eval-ast (ast-variable node 'ftx::token))))

(defun lambda-complete-token (cursor token)
  (let ((buffer (cursor-buffer cursor)))
    (flet ((string-symbol-repr (string)
             (if (string= "" string)
                 ""
                 (subseq (format nil "~s" (make-symbol string)) 2))))
      (multiple-value-bind (package string externalp package-string ncolons)
          (with-buffer-package buffer
            (parse-symbol-token token))
        (multiple-value-bind (symbol-string position) (lambda-complete-internal package string externalp)
          (unless symbol-string
            (editor-error "Could not complete symbol"))
          (let ((ret (if package-string
                         (format nil "~a~a~a"
                                 (string-symbol-repr package-string)
                                 (make-string ncolons :initial-element #\:)
                                 (string-symbol-repr symbol-string))
                         (string-symbol-repr symbol-string))))
            (values t
                    ret
                    (loop for end = (length ret) then (position #\- ret :from-end t :end end)
                          repeat position
                          finally (return end)))))))))

(define-group-evaluator ftx::lambda-main toplevel-p t)
(define-group-evaluator ftx::lambda-sexpr sexpr-p t)

(define-rule-evaluator ftx::lambda-list delimited-p t)
(define-rule-evaluator ftx::lambda-list missing-close-delim-p (boundp 'ftx::eof))
(define-rule-evaluator ftx::lambda-list list-p t)

(define-rule-evaluator ftx::lambda-string delimited-p t)
(define-rule-evaluator ftx::lambda-string missing-close-delim-p (boundp 'ftx::eof2))
