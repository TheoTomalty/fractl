(defmacro define-condition (name parent-types slot-specifiers &rest options)
  ;; TODO: ensure that parent-types inherit from condition
  (let (report)
    (setf options
          (loop for option in options
                when (and (consp option) (eql :report (car option)))
                  do (setf report (list `(report :initform ,(cadr option)
                                                 :reader condition-report)))
                else collect option))
    `(defclass ,name ,(or parent-types '(condition))
       ,(append slot-specifiers report)
       ,@(append options '((:metaclass condition-class))))))

(defclass condition-class (slot-class) ())
(defclass condition (slot-object) ())
(define-condition serious-condition () ())
(define-condition simple-condition ()
  ((format-control
    :initarg :format-control
    :initform nil
    :reader simple-condition-format-control)
   (format-arguments
    :initarg :format-arguments
    :initform nil
    :reader simple-condition-format-arguments)))

(define-condition storage-condition (serious-condition) ())

(define-condition error (serious-condition) ())
(define-condition simple-error (simple-condition error) ())
(define-condition cell-error (error)
  ((name
    :initarg :name
    :reader cell-error-name)))
(define-condition parse-error (error) ())
(define-condition type-error (error)
  ((datum
    :initarg :datum
    :reader type-error-datum)
   (expected-type
    :initarg :expected-type
    :reader type-error-expected-type)))
(define-condition simple-type-error (simple-condition error) ())
(define-condition control-error (error) ())
(define-condition simple-control-error (simple-condition error) ())
(define-condition invalid-method-error (simple-condition error) ())
(define-condition method-combination-error (simple-condition error) ())

(define-condition warning () ())
(define-condition simple-warning (simple-condition warning) ())
(define-condition style-warning (warning) ())

(defstruct (restart
            (:constructor make-restart
                (name function
                 &optional
                   report-function
                   interactive-function
                   test-function)))
  (name nil :type proper-symbol :read-only t)
  (function nil :type function :read-only t)
  (report-function nil :type (or function null) :read-only t)
  (interactive-function nil :type (or function null) :read-only t)
  (test-function (constantly t) :type function :read-only t))

(defvar *restarts* nil)
(defvar *applicable-handlers* nil)
(defvar *break-on-signals* nil)

(defun make-condition (name &rest args)
  (let ((class (find-class name)))
    (unless (typep class 'condition-class)
      (error "~s is not a condition class" class))
    (apply #'make-instance class args)))

(defmacro condition-from-datum (default-type datum args)
  `(etypecase ,datum
     (condition
      (when ,args
        (error "expected no arguments when a condition is passed to ~a"
               ,(ecase default-type
                  ((simple-condition) "signal")
                  ((simple-error) "error")
                  ((simple-warning) "warn"))))
      ,datum)
     (string
      (make-condition ',default-type :format-control ,datum :format-arguments ,args))
     (symbol
      (apply #'make-condition ,datum ,args))))

(defun signal (datum &rest args)
  (let ((condition (condition-from-datum simple-condition datum args))
        (saved-handlers *applicable-handlers*))
    (when (typep condition *break-on-signals*)
      (break condition))
    (loop while *applicable-handlers*
          do (let ((handlers (pop *applicable-handlers*)))
               (loop for (type . handler) in handlers
                     do (when (typep condition type)
                          (funcall handler condition))))
          finally (setf *applicable-handlers* saved-handlers))))

(defun error (datum &rest args)
  (let ((condition (condition-from-datum simple-error datum args)))
    (signal condition)
    (invoke-debugger condition)))

(defun invalid-method-error (method format-control &rest args)
  (error 'invalid-method-error
         :format-control format-control
         :format-arguments args))

(defun method-combination-error (method format-control &rest args)
  (error 'method-combination-error
         :format-control format-control
         :format-arguments args))

(defun simple-type-error (datum type)
  (error 'simple-type-error
         :datum datum
         :expected-type type
         :format-control "The value ~s is not of type ~s."
         :format-arguments (list datum type)))

(defmacro handler-bind (bindings &body body)
  `(let ((*applicable-handlers*
           (cons (list ,@(loop for (type . handler-forms) in bindings
                               do `(cons ',type ,(prognify handler-forms))))
                 *applicable-handlers*)))
     ,@body))

(defmacro handler-case (expression &optional error-clauses)
  (let ((no-error-clause (last error-clauses))
        (block-name (make-symbol "BLOCK")))
    (if (typep no-error-clause '(cons (cons (eql :no-error))))
        (setf no-error-clause (car no-error-clause)
              error-clauses (butlast error-clauses))
        (setf no-error-clause nil))
    `(block ,block-name
       `(handler-bind
            ,(loop for clause in error-clauses
                   do (destructuring-bind (type args &rest forms) clause
                        (let (decls)
                          (case (length args)
                            (0 (let ((sym (make-symbol "CONDITION")))
                                 (setf args (list sym))
                                 (push `(declare (ignore ,sym)) decls)))
                            (1 (unless (symbolp (car args))
                                 (error "argument must be a symbol")))
                            (t (error "too many arguments to HANDLER-CASE clause ~s" args)))
                          `(,type (lambda ,args
                                    ,@decls
                                    (return-from ,block-name ,(prognify forms)))))))
          ,(if no-error-clause
               (destructuring-bind (lambda-list . forms) (cdr no-error-clause)
                 `(multiple-value-call #'(lambda ,lambda-list ,@forms) ,expression))
               expression)))))

(defmacro ignore-errors (&body body)
  `(handler-case ,(prognify body)
     (error (condition) (values nil condition))))

(defmacro restart-bind (clauses &body body)
  `(let ((*restarts*
           (cons (list ,@(loop for clause in clause
                               collect (destructuring-bind (name function &rest args) clause
                                         `(cons ',name (make-restart ',name ,function ,@args)))))
                 *restarts*)))
     ,@body))

(defun ignore-t (c)
  (declare (ignore c))
  t)

(defmacro restart-case (form &rest clauses)
  ;; TODO: special case for signal, error, cerror, warn form
  (flet ((simple-string-report (string)
           (lambda (stream) (write-string string stream))))
    (let ((block-name (make-symbol "BLOCK")))
      `(block ,block-name
         (restart-bind
             ,(loop for clause in clauses
                    collect (destructuring-bind (name lambda-list &rest args) clause
                              (with-ext-lists ((kwargs kwargs-end))
                                (let ((test `(function ignore-t)))
                                  (loop while (and (consp args) (consp (cdr args)))
                                        do (case (car args)
                                             (:interactive
                                              (append-tail `(:interactive-function (function ,(cadr args))) kwargs-end))
                                             (:report
                                              (append-tail `(:report-function ,(if (stringp (cadr args))
                                                                                   (simple-string-report (cadr args))
                                                                                   `(function ,(cadr args))))
                                                           kwargs-end))
                                             (:test
                                              (setf test `(function ,(cadr args))))
                                             (t (return))))
                                  (append-tail `(:test-function ,test) kwargs-end))
                                (setf args (cddr args)))
                              `(,name #'(lambda ,lambda-list (return-from ,block-name ,(prognify args))) ,@kwargs)))
           ,form)))))

(defmacro with-condition-restarts (condition restarts &body body)
  (let ((condition-sym (make-symbol "CONDITION"))
        (restarts-sym (make-symbol "RESTARTS"))
        (restart-sym (make-symbol "RESTART")))
    `(let* ((,condition-sym ,condition)
            (,restarts-sym ,restarts)
            (*restarts* (cons (loop ,restart-sym in ,restarts-sym
                                    do (setf (rustart-test-function ,restart-sym)
                                             (lambda (condition) (eql ,condition-sym condition)))
                                    collect (cons (restart-name ,restart-sym) ,restart-sym))
                              *restarts*)))
       ,@body)))

(defun compute-restarts (&optional condition)
  (loop for restarts in *restarts*
        nconc (loop for (name . restart) in restarts
                    when (or (null condition)
                             (funcall (restart-test-function restart) condition))
                      collect restart)))

(defun find-restart (identifier &optional condition)
  (declare (type identifier (or proper-symbol restart))
           (type condition (or condition null)))
  (loop for restarts in *restarts*
        do (loop for (name . restart) in restarts
                 do (when (and (if (symbolp identifier)
                                   (eql name identifier)
                                   (eql restart identifier))
                               (or (not condition)
                                   (funcall (report-test-function restart) condition)))
                      (return restart)))))

(defun find-restart-strict (identifier &optional condition)
  (let ((restart (find-restart identifier condition)))
    (unless restart
      (error 'simple-control-error
             :format-control "The restart ~S is not active."
             :format-arguments (list identifier)))
    restart))

(defun invoke-restart (restart &rest args)
  (apply (restart-function (find-restart-strict restart)) args))

(defun invoke-restart-interactively (restart)
  (let* ((restart (find-restart-strict restart))
         (function (restart-function restart))
         (interactive (restart-interactive-function restart)))
    ;; TODO: should the interactive functions return arguments in lists or multiple values?
    (multiple-value-call function (if interactive
                                      (funcall interactive)
                                      (values)))))

(defun abort (&optional condition)
  (invoke-restart (find-restart-strict 'abort condition)))

(defun continue (&optional condition)
  (awhen (find-restart 'continue condition)
    (invoke-restart it)))

(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart-strict 'muffle-warning condition)))

(defun store-value (value &optional condition)
  (awhen (find-restart 'store-value condition)
    (invoke-restart it value)))

(defun use-value (value &optional condition)
  (awhen (find-restart 'use-value condition)
    (invoke-restart it value)))

(defmacro with-simple-restart ((name format-control &rest format-arguments) &body body)
  `(restart-case ,(prognify body)
     (,name ()
       :report (lambda (stream)
                 (format stream ,format-control ,@format-arguments))
       (values nil t))))

(defun cerror (continue-format-control datum &rest args)
  (with-simple-restart (continue "~a" (apply #'format nil continue-format-control args))
    (apply #'error datum args)))

(defun warn (datum &rest args)
  (let ((condition (condition-from-datum simple-warning datum args)))
    (unless (typep condition 'warning)
      (error 'type-error :datum condition :expected-type 'warning))
    (with-simple-restart (muffle-warning "Muffle warning.")
      (signal condition)
      (format *error-output* ";; warning: ~s~%" condition))
    nil))

(defun break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger
       (make-condition 'simple-condition
                       :format-control format-control
                       :format-arguments format-arguments))))
  nil)

(defun assert-error (places datum &rest args)
  (with-simple-restart (continue "Retry assertion with new values for ~{~s~^, ~}" places)
    (apply #'error datum args)))

(defun query-new-value ()
  (format *query-io* "Type a form to evaluate and use: ")
  (eval (read *query-io*)))

(defmacro assert (test-form
                  &optional places (datum-form "The assertion ~s failed." datum-form-p)
                  &rest argument-forms)
  (unless datum-form-p
    (setf argument-forms `(',test-form)))
  `(loop until ,test-form
         do (progn
              (assert-error ',places ,datum-form ,@argument-forms)
              ,@(loop for place in places
                      collect `(setf ,place (query-new-value ',place))))))

(defun check-type-error (place datum type &optional string)
  (restart-case
      (error 'simple-type-error
             :datum datum
             :expected-type type
             :format-control "The value of ~s, ~s, is not ~s."
             :format-arguments (list place datum (or string (format nil "of type ~s" type))))
    (store-value (value)
      :report "Specify a value to use instead."
      :interactive query-new-value
      value)))

(defmacro check-type (place type &optional string)
  (let ((datum (make-symbol "DATUM")))
    `(loop for ,datum = ,place
           until (typep ,datum ',type)
           do (setf ,place (check-type-error ',place ,datum ',type ,@(when string (list string)))))))

;; (defmacro invoke-debugger ()
;;   (error "unimplemented"))


