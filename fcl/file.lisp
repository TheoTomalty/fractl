;; namestring: a string that represents a file name (e.g. "test/file.txt")
;; pathname: a structure used to represent paths implementation-independently (e.g. #p"test/file.txt")
;; logical pathname namestring: implementation-independent string representation of pathnames?

(defstruct (pathname (:constructor %make-pathname)
                     (:conc-name %pathname-)
                     (:predicate pathnamep))
  (host nil :type (or string list (eql :unspecific)))
  (device nil :type (or string null (eql :unspecific)))
  (directory nil :type list)
  (name nil :type (or string null (member :wild :unspecific)))
  (type nil :type (or string null (member :wild :unspecific)))
  (version nil :type (or (integer 0 *) null (member :newest :wild :unspecific))))

(defstruct (logical-pathname (:include pathname)))

(defvar *logical-pathname-translations* (make-hash-table :test #'equal))
(defvar *logical-pathname-translations-internal* (make-hash-table :test #'equal))

(macrolet ((def (name)
             `(defun ,name (pathname)
                (,(conc-sym "%" name) (pathname pathname)))))
  (def pathname-host)
  (def pathname-device)
  (def pathname-directory)
  (def pathname-name)
  (def pathname-type)
  (def pathname-version))

;; Initialize it here?
(defvar *default-pathname-defaults*)

(defun make-pathname (&key host device directory name type version
                        (defaults (make-pathname :host (pathname-host *default-pathname-defaults*)))
                        (case :local))
  (when (stringp directory)
    (setf directory (list :absolute directory)))
  (when (eql :wild directory)
    (setf directory (list :absolute :wild-inferiors)))
  (let ((ret (if (logical-host-p host)
                 (%make-logical-pathname :host host :device device :directory directory
                                         :name name :type type :version version)
                 (%make-pathname :host host :device device :directory directory
                                 :name name :type type :version version))))
    (merge-pathnames ret defaults)
    case))

(defun pathname (pathname)
  (etypecase pathname
    (string
     (parse-namestring pathname))
    (pathname pathname)
    (stream ...)))

(defun remove-back (directories)
  (push nil directories)
  (loop for dirs on directories
        do (when (and (or (stringp (cadr dirs)) (eql :wild (cadr dirs)))
                      (eql :back (caddr dirs)))
             (setf (cdr dirs) (cdddr dirs)))
        finally (return (cdr directories))))

(defun merge-pathnames (pathname &optional (default-pathname *default-pathname-defaults*) (default-version :newest))
  (setf pathname (pathname pathname)
        default-pathname (pathname default-pathname))
  ;; TODO: handle logical namestring
  (let ((ret (copy-pathname pathname)))
    (if (and (pathname-host ret) (not (pathname-device ret))
             (eql (pathname-host ret) (pathname-host default-pathname)))
        (setf (pathname-device ret) (pathname-device default-pathname))
        (setf (pathname-device ret) nil))
    (macrolet ((merge (name)
                 `(when (not (,name pathname))
                    (setf (,name ret) (,name default-pathname)))))
      (merge pathname-host)
      ;; (merge pathname-device)
      (let ((pathname-directory (pathname-directory pathname))
            (default-pathname-directory (pathname-directory default-pathname)))
        (if (and (listp pathname-directory)
                 (eql :relative (car pathname-directory))
                 (listp default-pathname-directory))
            (remove-back
             (append default-pathname-directory
                     (cdr pathname-directory)))
            (or pathname-directory default-pathname-directory)))
      (merge pathname-name)
      (merge pathname-type))
    (if (not (pathname-name pathname))
        (merge pathname-version)
        (unless (pathname-version ret)
          (setf (pathname-version ret) default-version)))))

(define-condition parse-namestring-error (parse-error)
  ((index
    :initarg :index
    :reader parse-namestring-error-index)
   (reason
    :initarg :reason
    :reader parse-namestring-error-reason)))

(defun parse-unix-namestring (string start end defaults)
  (setf end (or end (length string)))
  (let ((directory-type :relative)
        components
        component
        (i start))
    (flet ((directory-component (component)
             (etypecase component
               ((cons (eql #\*) null)
                :wild)
               ((cons (eql #\.) (cons (eql #\.) null))
                :back)
               (list
                (awhen (position #\* component)
                  (error 'parse-namestring-error
                         :index (- i (- (length component) it))
                         :reason "Wildcard must be the only element of directory components."))
                (concatenate 'string component))))
           (name-component (component)
             (etypecase
               ((cons (eql #\*) null) :wild)
               (null nil)
               (list
                (awhen (member #\* component)
                  (error 'parse-namestring-error
                         :index (- i (- (length component) it))
                         :reason "Wildcard must be the only element of name components."))
                (concatenate 'string component))))
           (type-component (component)
             (etypecase
               ((cons (eql #\*) null) :wild)
               (list
                (awhen (member #\* component)
                  (error 'parse-namestring-error
                         :index (- i (- (length component) it))
                         :reason "Wildcard must be the only element of type components."))
                (concatenate 'string component))))
           (type-component (component)
             ))
      (loop while (< i end)
            do (let ((ch (aref string i)))
                 (case ch
                   (#\/
                    (if component
                        (push (directory-component component)
                              components)
                        (setf directory-type :absolute))
                    (loop while (and (< (1+ i) end) (eql #\/ (aref string (1+ i))))
                          do (incf i)))
                   (t (push ch component))))
               (incf i))
      (let* ((pos (position #\. component :from-end t))
             (full-component-p (or (null pos) (= pos 0)))
             (name-component (if full-component-p component (subseq component 0 pos)))
             (type-component (if full-component-p nil (subseq component (1+ pos)))))
        (values (make-pathname
                 :directory (when components
                              (list* directory-type components))
                 :name (name-component name-component)
                 :type (unless full-component-p
                         (type-component type-component))
                 :defaults defaults)
                i)))))

(defun unix-namestring (pathname)
  (let ((dirs (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (apply concatenate 'string
           (append (ecase (car dirs)
                     (:absolute (list "/"))
                     (:relative))
                   (loop for dir in (cdr dirs)
                         append (list (etypecase dir
                                        (string dir)
                                        ((eql :back) "..")
                                        ((eql :wild) "*"))
                                      "/"))
                   (when name
                     (list (etypecase name
                             (string name)
                             ((eql :wild) "*"))))
                   (when type (list "." type))))))

(defmacro check-host (host)
  `(unless (equal "UNIX" ,host)
     (error "Unknown host ~s." ,host)))

(defun parse-namestring (thing &optional host (default-pathname *default-pathname-defaults*)
                         &key (start 0) end junk-allowed)
  (if junk-allowed
      (handler-case (parse-namestring thing host default-pathname :start start :end end)
        (parse-namestring-error (e)
          (values nil (parse-namestring-error-index e))))
      (etypecase thing
        (stream ...)
        (pathname
         (if (equal (pathname-host thing) host)
             (values thing start)
             (error "Pathname host does not match :HOST parameter.")))
        (string
         (cond ((or (logical-host-p host)
                    (and (null host)
                         (or (logical-pathname-namestring-p thing)
                             (logical-host-p (pathname-host default-pathname)))))
                (merge-pathnames
                 (parse-logical-pathname-namestring thing)
                 default-pathname))
               (t
                (when host (check-host host))
                (parse-unix-namestring thing start end default-pathname)))))))

(defun namestring (pathname)
  (setf pathname (pathname pathname))
  (check-host (pathname-host pathname))
  (unix-namestring pathname))

(defun file-namestring (pathname)
  (setf pathname (pathname pathname))
  (check-host (pathname-host pathname))
  (let ((tmp (copy-pathname pathname)))
    (setf (pathname-directory tmp) nil)
    (unix-namestring tmp)))

(defun directory-namestring (pathname)
  (setf pathname (pathname pathname))
  (check-host (pathname-host pathname))
  (let ((tmp (copy-pathname pathname)))
    (setf (pathname-name tmp) nil)
    (setf (pathname-type tmp) nil)
    (setf (pathname-version tmp) nil)
    (unix-namestring tmp)))

(defun host-namestring (pathname)
  (declare (ignorable pathname))
  (setf pathname (pathname pathname))
  "")

(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
  (setf pathname (pathname pathname)
        defaults (pathname defaults))
  (check-host (pathname-host pathname))
  (let ((tmp (copy-pathname pathname)))
    (let ((dirs (pathname-directory pathname))
          (default-dirs (pathname-directory defaults)))
      (when (and (eql :absolute (car dirs))
                 (eql :absolute (car default-dirs)))
        (loop with subdirs = (cdr dirs)
              for dir in (cdr default-dirs)
              do (unless (equal dir (pop subdirs))
                   (return))
              finally (setf (pathname-directory tmp)
                            (if subdirs (list* :relative subdirs) nil)))))
    (when (equal (pathname-name pathname) (pathname-name defaults))
      (setf (pathname-name tmp) nil))
    (when (equal (pathname-type pathname) (pathname-type defaults))
      (setf (pathname-type tmp) nil))
    (when (equal (pathname-version pathname) (pathname-version defaults))
      (setf (pathname-version tmp) nil))
    (unix-namestring tmp)))

(defun wild-pathname-p (pathname &optional field-key)
  (setf pathname (pathname pathname))
  (flet ((host-p () nil)
         (device-p () nil)
         (directory-p ()
           (member :wild (pathname-directory pathname)))
         (name-p ()
           (eql :wild (pathname-name pathname)))
         (type-p () (eql :wild (pathname-name pathname)))
         (version-p () (eql :wild (pathname-version pathname))))
    (ecase field-key
      ((:host) (host-p))
      ((:device) (device-p))
      ((:directory) (directory-p))
      ((:name) (name-p))
      ((:type) (type-p))
      ((:version) (version-p))
      ((nil) (or (host-p)
                 (device-p)
                 (directory-p)
                 (name-p)
                 (type-p)
                 (version-p))))))

(defun pathname-match-p (pathname wildcard)
  (setf pathname (pathname pathname)
        wildcard (pathname wildcard))
  (macrolet ((wild-match (name)
               (or (eql :wild (,name wildcard))
                   (equal (,name pathname) (,name wildcard)))))
    (and (equal (pathname-host pathname) (pathname-host wildcard))
         (equal (pathname-device pathname) (pathname-device wildcard))
         (loop for dir in (pathname-directory pathname) and wdir in (pathname-directory wildcard)
               always (or (eql :wild wdir) (equal dir wdir)))
         (wild-match pathname-name)
         (wild-match pathname-type)
         (wild-match pathname-version))))

(defun translate-pathname (source from-wildcard to-wildcard)
  (declare (ignore source from-wildcard to-wildcard))
  (error "unimiplemented"))

(defun parse-logical-pathname-namestring (string)
  (let ((i 0))
    (labels ((word-char-p (&key with-asterisk)
               (let ((ch (aref string i)))
                 (or (char<= #\A ch #\Z)
                     (char<= #\0 ch #\9)
                     (eql #\- ch)
                     (and with-asterisk (eql #\* ch)))))
             (parse-marker-strict (ch)
               (aif (parse-marker ch)
                    it
                    (error "Expected marker ~s at position ~s." ch i)))
             (parse-marker (ch)
               (when (eql ch (aref string i))
                 (incf i)
                 ch))
             (parse-word-strict (&key with-asterisk)
               (loop with start = i
                     while (word-char-p :with-asterisk with-asterisk)
                     do (incf i)
                     finally (unless (/= i start)
                               (error "Word components must have at least one character."))
                             (return (subseq string start i))))
             (parse-host ()
               (when (word-char-p)
                 (prog1 (parse-word-strict)
                   (parse-marker-strict #\:))))
             (parse-wild-word-strict (&key wild-inferiors)
               (let ((word (parse-word-strict :with-asterisk t)))
                 (cond ((and wild-inferiors (equal "**" word))
                        :wild-inferiors)
                       ((search "**" word)
                        (error "Components may not have adjacent wildcard symbols."))
                       ((search "*" word)
                        (if (equal "*" word)
                            :wild
                            (error "unimplemented")))
                       (t word))
                 (parse-marker-strict #\;)))
             (parse-directory ()
               (when (word-char-p :with-asterisk t)
                 (prog1 (parse-wild-word-strict :wild-inferiors t)
                   (parse-marker-strict #\;))))
             (parse-type-and-version ()
               (when (parse-marker #\.)
                 (values (parse-wild-word-strict)
                         (parse-version))))
             (parse-version ()
               (when (parse-marker #\.)
                 (let ((word (parse-wild-word-strict))
                       (int 0))
                   (cond ((loop for ch across word
                                always (digit-char-p ch)
                                do (setf int (+ (* int 10) (- (char-code ch) #.(char-code #\0)))))
                          int)
                         ((or (equal word "newest") (equal word "NEWEST"))
                          :newest)
                         ((equal word "*")
                          :wild)
                         (t (error "Invalid version word ~s." word))))
                 (parse-version))))
      (let (type version)
        (prog1
            (make-logical-pathname :host (parse-host)
                                   :device nil
                                   :directory (let ((relative-directory-p (parse-marker #\;)))
                                                (list* (if relative-directory-p :relative :absolute)
                                                       (loop for dir = (parse-directory)
                                                             while dir
                                                             collect dir)))
                                   :name (when (word-char-p :with-asterisk t)
                                           (parse-wild-word-strict))
                                   :type (setf (values type version) (parse-type-and-version))
                                   :version version)
          (unless (= i (length string))
            (error "Unexpected characters in logical pathname parsing ~s." (subseq string i))))))))

(defun logical-host-p (obj)
  (and (stringp obj)
       (nth-value 1 (gethash (string-upcase obj) *logical-pathname-translations*))))

(defun valid-logical-hostname-p (obj)
  (and (stringp obj)
       (loop for ch across obj
             always (or (char<= #\A ch #\Z)
                        (char<= #\0 ch #\9)
                        (eql #\- ch)))))

(defmacro check-valid-logical-hostname (obj)
  `(unless (valid-logical-hostname-p ,obj)
     (error "Invalid logical hostname ~s." ,obj)))

(defun logical-pathname-namestring-p (obj)
  (and (stringp obj)
       (awhen (position #\: obj)
         (let ((host (subseq obj 0 it)))
           (and (valid-logical-hostname-p host)
                (logical-host-p host))))))

(defun logical-pathname (pathspec)
  (etypecase pathspec
    (string
     (unless (logical-pathname-namestring-p pathspec)
       (error "~s is not a logical pathname namestring" pathspec))
     (parse-logical-pathname-namestring pathspec))
    (logical-pathname
     pathspec)
    (stream ...)))

(defun logical-pathname-translations (host)
  (nth-value 0 (gethash (string-upcase host) *logical-pathname-translations*)))

(defun (setf logical-pathname-translations) (new host)
  (setf host (string-upcase host))
  (check-valid-logical-hostname host)
  (setf (gethash host *logical-pathname-translations*) nil
        (gethash host *logical-pathname-translations-internal*) nil)
  (setf (gethash host *logical-pathname-translations-internal*)
        (loop for translation in new
              collect (destructuring-bind (from-wildcard to-wildcard) translation
                        (cons (parse-namestring from-wildcard host)
                              (pathname to-wildcard)))))
  (setf (gethash host *logical-pathname-translations*) new))

(defun load-logical-pathname-translations (host)
  (setf host (string-upcase host))
  (check-valid-logical-hostname host)
  (unless (logical-host-p host)
    (error "unimplemented")))

(defun translate-logical-pathname (pathname)
  (setf pathname (pathname pathname))
  (etypecase pathname
    (logical-pathname
     (multiple-value-bind (translations found)
         (gethash (pathname-host pathname) *logical-pathname-translations-internal*)
       (assert found)
       (translate-logical-pathname
        (aif (assoc pathname translations :test #'pathname-match-p)
             (translate-pathname pathname (car it) (cdr it))
             (error "No pathname translation matched.")))))
    (pathname pathname)))
