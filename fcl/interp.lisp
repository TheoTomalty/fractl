(defpackage #:fr-int
  (:use #:cl)
  (:import-from #:sb-impl #:comma #:comma-p #:comma-expr #:comma-kind)
  (:import-from #:sb-int #:quasiquote #:unquote))

(defpackage #:fr-i
  (:use #:cl #:fr-int)
  (:export #:interp-eval #:interp-load))

(in-package #:fr-i)

;; From utils.lisp

(defun gethash-strict (key hash-table)
  (multiple-value-bind (ret found) (gethash key hash-table)
    (when (not found)
      (error "key not found in hash-table: ~s" key))
    ret))

(defun (setf gethash-strict) (val key hash-table)
  (when (nth-value 1 (gethash key hash-table))
    (error "key already in hash-table: ~s" key))
  (setf (gethash key hash-table) val))

(defun make-dynamic-array (&rest keys)
  (apply #'make-array 16 :adjustable t :fill-pointer 0 keys))


(declaim (optimize (debug 3)))

(defstruct lexenv
  (variables nil :type list)
  (functions nil :type list)
  (blocks nil :type list)
  (tags nil :type list)
  (parent nil :type (or lexenv null)))


;; Convert these to objects
(defstruct interp-macro-function
  (function nil :type function))

(defstruct interp-special-form
  (name nil :type symbol))

(defstruct interp-object
  class
  (slots nil :type simple-vector))

(defparameter *keyword-package* (find-package "KEYWORD"))
(defparameter *symbol-functions* (make-hash-table))
(defparameter *symbol-macros* (make-hash-table))
(defparameter *symbol-values* (make-hash-table))
(defparameter *symbol-plists* (make-hash-table))
(defparameter *info* (make-hash-table))

(defun info (category sym)
  (when (and (eql category :variable) (keywordp sym))
    (return-from info :constant))
  (multiple-value-bind (plist found) (gethash sym *info*)
    (when found
      (getf plist category))))

(defun (setf info) (val category sym)
  (setf (getf (gethash sym *info*) category) val))

(defun get-symbol-value (sym)
  (if (keywordp sym)
      (values sym t)
      (gethash sym *symbol-values*)))

(defmacro define-constant (name val)
  `(progn
     (setf (info :variable ',name) :constant)
     (setf (gethash ',name *symbol-values*) ,val)))

(defmacro define-parameter (name val)
  `(progn
     (setf (info :variable ',name) :special)
     (setf (gethash ',name *symbol-values*) ,val)))

(defmacro define-builtin-function (name)
  `(progn
     (setf (info :function ',name) :function)
     (setf (gethash ',name *symbol-functions*) #',name)))

(defmacro define-builtin-functions (&body names)
  `(progn
     ,@(loop for name in names collect `(define-builtin-function ,name))))

(defmacro define-special-form (name)
  `(progn
     (setf (info :function ',name) :special-form)
     (setf (gethash ',name *symbol-functions*) (make-interp-special-form :name ',name))))

(defmacro define-special-forms (&body names)
  `(progn
     ,@(loop for name in names collect `(define-special-form ,name))))

(defmacro define-function (name args &body body)
  `(progn
     (setf (info :function ',name) :function)
     (setf (gethash ',name *symbol-functions*)
           (lambda ,args ,@body))))

(defmacro define-setf-function (name args &body body)
  `(setf (info :setf-function ',name)
         (lambda ,args ,@body)))

(defmacro define-macro (name args &body body)
  (let ((form-sym (make-symbol "FORM"))
        (env-sym (make-symbol "ENV")))
    `(progn
       (setf (info :function ',name) :macro)
       (setf (gethash ',name *symbol-functions*)
             (make-interp-macro-function
              :function
              (lambda (,form-sym ,env-sym)
                (declare (ignore ,env-sym))
                (destructuring-bind ,args (cdr ,form-sym)
                  ,@body)))))))

(defun constant-p (name)
  (eql :constant (info :variable name)))

(defun lookup-variable-in-lexenv (sym env &key (fail t))
  (if (null env)
      (multiple-value-bind (symbol-macro found) (gethash sym *symbol-macros*)
        (if found
            (values symbol-macro :macro)
            (multiple-value-bind (val found) (get-symbol-value sym)
              (when (and (not found) fail)
                (error 'unbound-variable :name sym))
              (values val :variable))))
      (let ((assoc (assoc sym (lexenv-variables env))))
        (if assoc
            (values-list (cdr assoc))
            (lookup-variable-in-lexenv sym (lexenv-parent env) :fail fail)))))

(defun set-variable-in-lexenv (sym val env)
  (if (null env)
      (if (constant-p sym)
          (error "can not set constant value ~s" sym)
          (setf (gethash sym *symbol-values*) val))
      (let ((assoc (assoc sym (lexenv-variables env))))
        (if assoc
            (setf (cadr assoc) val)
            (set-variable-in-lexenv sym val (lexenv-parent env))))))

(defun variable-special-in-lexenv-p (sym env)
  (if (null env)
      (eql :special (info :variable sym))
      (let ((assoc (assoc sym (lexenv-variables env))))
        (if assoc
            (eql :special (caddr assoc))
            (variable-special-in-lexenv-p sym (lexenv-parent env))))))

(defun lookup-function-in-lexenv (sym env &key (fail t))
  (if (null env)
      (multiple-value-bind (val found) (gethash sym *symbol-functions*)
        (when (and (not found) fail)
          (error 'undefined-function :name sym))
        (typecase val
          (interp-macro-function
           (values (interp-macro-function-function val) :macro))
          (t (values val :function))))
      (let ((assoc (assoc sym (lexenv-functions env))))
        (if assoc
            (values-list (cdr assoc))
            (lookup-function-in-lexenv sym (lexenv-parent env) :fail fail)))))

(defun local-function-in-lexenv-p (sym env)
  (if (null env)
      nil
      (let ((assoc (assoc sym (lexenv-functions env))))
        (if assoc
            t
            (local-function-in-lexenv-p sym (lexenv-parent env))))))

(defun lookup-block-in-lexenv (sym env)
  (if (null env)
      (error "return from unknown block: ~s" sym)
      (let ((assoc (assoc sym (lexenv-blocks env))))
        (if assoc
            (cdr assoc)
            (lookup-block-in-lexenv sym (lexenv-parent env))))))

(defun lookup-tag-in-lexenv (sym env)
  (if (null env)
      (error "go to unknown tag: ~s" sym)
      (let ((assoc (assoc sym (lexenv-tags env))))
        (if assoc
            (cdr assoc)
            (lookup-tag-in-lexenv sym (lexenv-parent env))))))

(defun eval-block (exprs env)
  (loop while (not (null (cdr exprs)))
        for expr = (pop exprs)
        do (eval-in-lexenv expr env)
        finally (return (eval-in-lexenv (car exprs) env))))

(defmacro invalid-form (name expr)
  `(error ,(format nil "invalid ~a form:~~% ~~s" name) ,expr))

(defun silent-length (list)
  (handler-case (length list)
    (type-error () -1)))

(defmacro check-not-constant (symbol)
  (let ((symbol-sym (gensym)))
    `(let ((,symbol-sym ,symbol))
       (when (constant-p ,symbol-sym)
         (error "~s names a constant and can not be bound lexically" ,symbol-sym)))))

(defmacro check-nargs= (nargs target name expr)
  `(unless (= ,nargs ,target)
     (error ,(format nil "incorrect number of arguments for ~s:~~% ~~s" name) ,expr)))

(defmacro check-nargs>= (nargs target name expr)
  `(unless (>= ,nargs ,target)
     (error ,(format nil "not enough arguments for ~s:~~% ~~s" name) ,expr)))


(defmacro with-saved-specials (saved &body body)
  (declare (type symbol saved))
  `(let ((,saved nil))
     (unwind-protect (progn ,@body)
       (loop for (var old-value . found) in saved
             do (if found
                    (setf (gethash var *symbol-values*) old-value)
                    (remhash var *symbol-values*))))))

(defmacro save-and-bind-special (saved var val)
  (let ((var-sym (gensym))
        (val-sym (gensym)))
    `(let ((,var-sym ,var)
           (,val-sym ,val))
       (multiple-value-bind (old-value found) (gethash ,var-sym *symbol-values*)
         (push (list* ,var-sym old-value found) ,saved)
         (setf (gethash ,var-sym *symbol-values*) ,val-sym)))))

(defun special-declarations (exprs env &key force)
  (let (specials)
    (let ((expr (car exprs)))
      (when (and (consp expr) (eql 'declare (car expr)))
        (loop for declaration in (cdr expr)
              when (and (consp declaration) (eql 'special (car declaration)))
                do (loop for var in (cdr declaration)
                         do (unless (symbolp var)
                              (invalid-form "DECLARE" expr))
                         do (push var specials)))))
    (if (or specials force)
        (make-lexenv :parent env :variables (loop for var in specials
                                                  collect (list var nil :special)))
        env)))

(defun remove-declarations (exprs)
  (loop while (and exprs (consp (car exprs)) (eql 'declare (caar exprs)))
        do (setf exprs (cdr exprs)))
  exprs)

(defun generic-function-p (obj)
  (and (interp-object-p obj)
       (eql (info :class 'standard-generic-function)
            (interp-object-class obj))))

(defun interp-apply (function arguments)
  (if (generic-function-p function)
      (apply (or (gethash 'fr-int::funcall-generic-function *symbol-functions*)
                 (error "generic functions not initialized yet"))
             function
             arguments)
      (apply function arguments)))

(defun eval-in-lexenv (expr env)
  (typecase expr
    (symbol
     (multiple-value-bind (val kind) (lookup-variable-in-lexenv expr env)
       (case kind
         ((:macro) (eval-in-lexenv val env))
         (t val))))
    (cons
     (let ((name (first expr)))
       (case name
         ((let)
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 variables
                 (new-env (special-declarations body env :force t)))
            (with-saved-specials saved
              (loop for (binding init-form) in bindings
                    do (let ((val (when init-form
                                    (eval-in-lexenv init-form env))))
                         (if (variable-special-in-lexenv-p binding new-env)
                             (save-and-bind-special saved binding val)
                             (push (list binding val :variable)
                                   variables))))
              (setf (lexenv-variables new-env)
                    (append (lexenv-variables new-env) variables))
              (eval-block (remove-declarations body) new-env))))
         ((let*)
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 variables
                 (new-env (special-declarations body env :force t)))
            (with-saved-specials saved
              (loop for (binding init-form) in bindings
                    do (let ((val (when init-form
                                    (eval-in-lexenv
                                     init-form
                                     (make-lexenv :parent env :variables variables)))))
                         (if (variable-special-in-lexenv-p binding new-env)
                             (save-and-bind-special saved binding val)
                             (push (list binding val :variable)
                                   variables))))
              (setf (lexenv-variables new-env)
                    (append (lexenv-variables new-env) variables))
              (eval-block (remove-declarations body) new-env))))
         ((symbol-macrolet)
          (let ((bindings (cadr expr))
                (body (cddr expr)))
            (eval-block
             body
             (make-lexenv
              :parent env
              :variables (loop for binding in bindings
                               collect (list (car binding) (cadr binding) :macro))))))
         ((setq)
          (loop with args = (cdr expr)
                while args
                for name = (pop args)
                for form = (pop args)
                with ret = nil
                do (let ((val (eval-in-lexenv form env)))
                     (set-variable-in-lexenv name val env)
                     (setf ret val))
                finally (return ret)))
         ((block)
          (let ((name (cadr expr))
                (exprs (cddr expr))
                (catch-sym (make-symbol "RETURN")))
            (catch catch-sym
              (eval-block
               exprs
               (make-lexenv
                :parent env
                :blocks (list (cons name catch-sym)))))))
         ((return-from)
          (let ((name (cadr expr))
                (expr (caddr expr)))
            (throw (lookup-block-in-lexenv name env)
              (eval-in-lexenv expr env))))
         ((tagbody)
          (let ((exprs (make-dynamic-array))
                tags
                (i 0)
                (catch-sym (make-symbol "GO")))
            (loop for expr in (cdr expr)
                  with i = 0
                  do (if (symbolp expr)
                         (push (list* expr catch-sym i) tags)
                         (progn
                           (vector-push-extend expr exprs)
                           (incf i))))
            (setf exprs (coerce exprs 'simple-vector))
            (locally (declare (type simple-vector exprs))
              (loop with new-env = (make-lexenv :parent env
                                                :tags tags)
                    while (< i (length exprs))
                    for expr = (aref exprs i)
                    do (block nil
                         (setf i (catch catch-sym
                                   (progn
                                     (eval-in-lexenv expr new-env)
                                     (incf i)
                                     (return)))))))))
         ((go)
          (let ((name (cadr expr)))
            (destructuring-bind (catch-sym . i) (lookup-tag-in-lexenv name env)
              (throw catch-sym i))))
         ((catch)
          (let ((sym (cadr expr))
                (form (caddr expr)))
            (catch (eval-in-lexenv sym env)
              (eval-in-lexenv form env))))
         ((throw)
          (let ((sym (cadr expr))
                (form (caddr expr)))
            (throw (eval-in-lexenv sym env)
              (eval-in-lexenv form env))))
         ((load-time-value)
          (eval-in-lexenv (cadr expr) env))
         ((eval-when)
          (let ((when (cadr expr))
                (body (cddr expr)))
            (when (member :execute when)
              (eval-block body env))))
         ((locally)
          (let* ((body (cdr expr))
                 (new-env (special-declarations body env)))
            (eval-block (remove-declarations body) new-env)))
         ((flet)
          (let ((bindings (cadr expr))
                (body (cddr expr)))
            (eval-block
             body
             (make-lexenv
              :parent env
              :functions (loop for binding in bindings
                               collect (destructuring-bind (name arg-sym &rest lambda-body) binding
                                         (list name
                                               (lambda (&rest rest)
                                                 (let ((catch-sym (make-symbol "RETURN")))
                                                   (catch catch-sym
                                                     (eval-block
                                                      lambda-body
                                                      (make-lexenv :parent env
                                                                   :variables (list (list arg-sym rest :variable))
                                                                   :blocks (list (cons name catch-sym)))))))
                                               :function)))))))
         ((labels)
          (let ((bindings (cadr expr))
                (body (cddr expr))
                (new-env (make-lexenv :parent env)))
            (setf (lexenv-functions new-env)
                  (loop for binding in bindings
                        collect (destructuring-bind (name arg-sym &rest lambda-body) binding
                                  (list name
                                        (lambda (&rest rest)
                                          (let ((catch-sym (make-symbol "RETURN")))
                                            (catch catch-sym
                                              (eval-block
                                               lambda-body
                                               (make-lexenv :parent new-env
                                                            :variables (list (list arg-sym rest :variable))
                                                            :blocks (list (cons name catch-sym)))))))
                                        :function))))
            (eval-block body new-env)))
         ((macrolet)
          (let ((bindings (cadr expr))
                (body (cddr expr)))
            (eval-block
             body
             (make-lexenv
              :parent env
              :functions (loop for (name lambda-list . lambda-body) in bindings
                               collect (list name nil :macro))))))
         ((function)
          (let ((arg (cadr expr)))
            (if (symbolp arg)
                (lookup-function-in-lexenv arg env)
                (ecase (car arg)
                  ((lambda)
                   (let ((arg-sym (cadr arg))
                         (lambda-body (cddr arg)))
                     (lambda (&rest rest)
                       (eval-block
                        lambda-body
                        (make-lexenv :parent env
                                     :variables (list (list arg-sym rest :variable)))))))
                  ((setf)
                   (or (info :setf-function (cadr arg))
                       (error 'undefined-function :name arg)))))))
         ((multiple-value-call)
          (let ((function (cadr expr))
                (form (caddr expr)))
            (interp-apply (eval-in-lexenv function env)
                          (multiple-value-list (eval-in-lexenv form env)))))
         ((multiple-value-prog1)
          (let ((form (cadr expr))
                (body (cddr expr)))
            (let ((vals (multiple-value-list (eval-in-lexenv form env))))
              (eval-block body env)
              (values-list vals))))
         ((the)
          (let ((type (cadr expr))
                (form (caddr expr)))
            (declare (ignore type))
            (eval-in-lexenv form env)))
         ((if)
          (let ((cond (cadr expr))
                (then (caddr expr))
                (else (cadddr expr)))
            (if (eval-in-lexenv cond env)
                (eval-in-lexenv then env)
                (eval-in-lexenv else env))))
         ((progn)
          (eval-block (cdr expr) env))
         ((progv)
          (let ((vars (cadr expr))
                (vals (caddr expr))
                (body (cdddr expr)))
            (let ((saved (loop for var in (eval-in-lexenv vars env)
                               for val in (eval-in-lexenv vals env)
                               do (unless (symbolp var)
                                    (error "~s is not a symbol when binding" var))
                               collect (multiple-value-bind (old-value found) (gethash var *symbol-values*)
                                         (list* var old-value found)))))
              (unwind-protect
                   (eval-block body env)
                (loop for (var old-value . found) in saved
                      do (if found
                             (setf (gethash var *symbol-values*) old-value)
                             (remhash var *symbol-values*)))))))
         ((unwind-protect)
          (let ((protected (cadr expr))
                (cleanup (cddr expr)))
            (unwind-protect
                 (eval-in-lexenv protected env)
              (eval-block cleanup env))))
         ((quote)
          (cadr expr))
         (t
          (let ((form (car expr))
                (args (cdr expr)))
            (if (symbolp form)
                (multiple-value-bind (func kind) (lookup-function-in-lexenv form env)
                  (unless (eql kind :function)
                    (error 'undefined-function :name form))
                  (interp-apply func (loop for arg in args collect (eval-in-lexenv arg env))))
                (let ((arg-sym (cadr form))
                      (lambda-body (cddr form)))
                  (assert (eql 'lambda (car form)))
                  (lambda (&rest rest)
                    (eval-block
                     lambda-body
                     (make-lexenv :parent env
                                  :variables (list (list arg-sym rest :variable))))))))))))
    (t expr)))

(defun macroexpand-block (body env &key ignore-declare)
  (loop for expr in body
        with first = t
        collect (if (and first ignore-declare (consp expr) (eql 'declare (car expr)))
                    expr
                    (macroexpand-all expr env))
        do (setf first nil)))

(defun coerce-to-function (val)
  (etypecase val
    (symbol
     (let ((ret (or (gethash val *symbol-functions*)
                    (error 'undefined-function :name val))))
       (when (interp-macro-function-p ret)
         (error "~s is a macro, not a function" val))
       (when (interp-special-operator-p ret)
         (error "~s is a special form, not a function" val))
       ret))
    (function
     val)
    ((satisfies generic-function-p)
     val)))

(defun simple-expand-lambda-list (lambda-list list)
  (let* ((key nil) (reqend 0) (optend 0)
         (list-sym (make-symbol "LIST"))
         (bindings (loop for form in lambda-list
                         if (member form lambda-list-keywords)
                           do (setf key form)
                         else
                           collect (ecase key
                                     ((nil)
                                      (incf reqend) (incf optend)
                                      (list form (let ((x (make-symbol "X")))
                                                   `(let ((,x (car ,list-sym)))
                                                      (setq ,list-sym (cdr ,list-sym))
                                                      ,x))))
                                     ((&optional)
                                      (incf optend)
                                      (list form (let ((x (make-symbol "X")))
                                                   `(let ((,x (car ,list-sym)))
                                                      (setq ,list-sym (cdr ,list-sym))
                                                      ,x))))
                                     ((&rest &body)
                                      (setf optend nil)
                                      (list form list-sym))))))
    (list* (list list-sym (if optend
                              `(fr-int::check-lambda-list ,list ,reqend ,optend ',lambda-list)
                              `(fr-int::check-lambda-list-rest ,list ,reqend ',lambda-list)))
           bindings)))

(defun simple-expand-macro-lambda-list (lambda-list form env)
  (declare (ignore env))
  (simple-expand-lambda-list lambda-list `(cdr ,form)))

(defun macroexpand-lambda-list (lambda-list body env)
  (let ((args-sym (make-symbol "ARGS"))
        (fn (or (gethash 'fr-int::expand-lambda-list *symbol-functions*)
                #'simple-expand-lambda-list)))
    (list args-sym
          (macroexpand-all
           `(let* ,(funcall fn lambda-list args-sym)
              ,@body)
           env))))

(defun macroexpand-macro-lambda-list (lambda-list body env)
  (let ((form-sym (make-symbol "FORM"))
        (env-sym (make-symbol "ENV"))
        (fn (or (gethash 'fr-int::expand-macro-lambda-list *symbol-functions*)
                #'simple-expand-macro-lambda-list)))
    (list form-sym env-sym
          (macroexpand-all
           `(let* ,(funcall fn lambda-list form-sym env-sym)
              ,@body)
           env))))

(defun macroexpand-all (expr &optional env)
  (setq expr (interp-macroexpand expr env))
  (typecase expr
    (symbol expr)
    (cons
     (let ((name (first expr))
           (nargs (silent-length (rest expr))))
       (case name
         ((let)
          (check-nargs>= nargs 1 "LET" expr)
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 variables
                 new-bindings
                 (new-env (special-declarations body env :force t)))
            (unless (listp bindings)
              (invalid-form "LET" expr))
            (loop for binding in bindings
                  for init-form = nil
                  do (unless (or (symbolp binding)
                                 (and (consp binding)
                                      (= 2 (silent-length binding))
                                      (symbolp (setf init-form (cadr binding)
                                                     binding (car binding)))))
                       (invalid-form "LET" expr))
                  do (progn
                       (check-not-constant binding)
                       (push (list binding (macroexpand-all init-form env)) new-bindings)
                       (unless (variable-special-in-lexenv-p binding new-env)
                         (push (list binding nil :variable)
                               variables))))
            (setf (lexenv-variables new-env)
                  (append (lexenv-variables new-env) variables))
            `(let ,(reverse new-bindings) ,@(macroexpand-block body new-env :ignore-declare t))))
         ((let*)
          (check-nargs>= nargs 1 "LET*" expr)
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 variables
                 new-bindings
                 (new-env (special-declarations body env :force t)))
            (unless (listp bindings)
              (invalid-form "LET*" expr))
            (loop for binding in bindings
                  for init-form = nil
                  do (unless (or (symbolp binding)
                                 (and (consp binding)
                                      (= 2 (silent-length binding))
                                      (symbolp (setf init-form (cadr binding)
                                                     binding (car binding)))))
                       (invalid-form "LET*" expr))
                  do (progn
                       (check-not-constant binding)
                       (push (list binding (macroexpand-all
                                            init-form
                                            (make-lexenv :parent env :variables variables)))
                             new-bindings)
                       (unless (variable-special-in-lexenv-p binding new-env)
                         (push (list binding nil :variable)
                               variables))))
            (setf (lexenv-variables new-env)
                  (append (lexenv-variables new-env) variables))
            `(let* ,(reverse new-bindings) ,@(macroexpand-block body new-env :ignore-declare t))))
         ((symbol-macrolet)
          (check-nargs>= nargs 1 "SYMBOL-MACROLET" expr)
          (let ((bindings (cadr expr))
                (body (cddr expr)))
            ;; TODO: handle declare
            `(symbol-macrolet
                 ,bindings
               ,@(macroexpand-block
                  body
                  (make-lexenv
                   :parent env
                   :variables (loop for binding in bindings
                                    do (unless (and (consp binding)
                                                    (= 2 (silent-length binding))
                                                    (symbolp (car binding)))
                                         (invalid-form "SYMBOL-MACROLET" expr))
                                    collect (list (car binding) (cadr binding) :macro)))))))
         ((setq)
          (unless (evenp nargs)
            (error "odd number of arguments for SETQ:~% ~s" expr))
          `(setq ,@(loop with args = (cdr expr)
                         while args
                         for name = (pop args)
                         for form = (pop args)
                         do (unless (symbolp name)
                              (invalid-form "SETQ" expr))
                         append (list name (macroexpand-all form env)))))
         ((block)
          (check-nargs>= nargs 1 "BLOCK" expr)
          (let ((name (cadr expr))
                (body (cddr expr)))
            (unless (symbolp name)
              (invalid-form "BLOCK" expr))
            `(block ,name
               ,@(macroexpand-block
                  body
                  (make-lexenv
                   :parent env
                   :blocks (list (cons name nil)))))))
         ((return-from)
          (check-nargs= nargs 2 "RETURN-FROM" expr)
          (let ((name (cadr expr))
                (expr (caddr expr)))
            (unless (symbolp name)
              (invalid-form "RETURN-FROM" expr))
            `(return-from ,name ,(macroexpand-all expr env))))
         ((tagbody)
          (let (tags)
            (loop for expr in (cdr expr)
                  with i = 0
                  do (if (symbolp expr)
                         (push (list* expr nil i) tags)
                         (incf i)))
            `(tagbody
                ,@(loop for expr in (cdr expr)
                        with new-env = (make-lexenv :parent env :tags tags)
                        collect (if (symbolp expr)
                              expr
                              (macroexpand-all expr new-env))))))
         ((go)
          (check-nargs= nargs 1 "GO" expr)
          (let ((name (cadr expr)))
            (unless (symbolp name)
              (invalid-form "GO" expr))
            expr))
         ((catch)
          (check-nargs= nargs 2 "CATCH" expr)
          (let ((sym (cadr expr))
                (form (caddr expr)))
            `(catch ,(macroexpand-all sym env)
               ,(macroexpand-all form env))))
         ((throw)
          (check-nargs= nargs 2 "THROW" expr)
          (let ((sym (cadr expr))
                (form (caddr expr)))
            `(throw ,(macroexpand-all sym env)
               ,(macroexpand-all form env))))
         ((load-time-value)
          (check-nargs= nargs 1 "LOAD-TIME-VALUE" expr)
          (macroexpand-all (cadr expr) env))
         ((eval-when)
          (check-nargs>= nargs 1 "EVAL-WHEN" expr)
          (let ((when (cadr expr))
                (body (cddr expr)))
            (unless (and (listp when)
                         (loop for time in when
                               always (member time '(:compile-toplevel :load-toplevel :execute))))
              (invalid-form "EVAL-WHEN" expr))
            `(eval-when ,when
              ,@(macroexpand-block body env))))
         ((locally)
          (let* ((body (cdr expr))
                 (new-env (special-declarations body env)))
            `(locally ,@(macroexpand-block body new-env))))
         ((flet)
          (check-nargs>= nargs 1 "FLET" expr)
          (let ((bindings (cadr expr))
                new-bindings
                (body (cddr expr))
                (new-env (make-lexenv :parent env)))
            (setf (lexenv-functions new-env)
                  (loop for binding in bindings
                        for name = nil
                        for lambda-list = nil
                        for lambda-body = nil
                        do (unless (and (consp binding)
                                        (>= (silent-length binding) 2)
                                        (symbolp (setf name (car binding)))
                                        (listp (setf lambda-body (cddr binding)
                                                     lambda-list (cadr binding))))
                             (invalid-form "FLET" expr))
                        collect (list name nil :function)
                        do (push (list name lambda-list lambda-body) new-bindings)))
            `(flet ,(loop for (name lambda-list lambda-body) in new-bindings
                          collect (list* name (macroexpand-lambda-list lambda-list lambda-body new-env)))
               ,@(macroexpand-block body new-env))))
         ((labels)
          (check-nargs>= nargs 1 "LABELS" expr)
          (let ((bindings (cadr expr))
                new-bindings
                (body (cddr expr))
                (new-env (make-lexenv :parent env)))
            (setf (lexenv-functions new-env)
                  (loop for binding in bindings
                        for name = nil
                        for lambda-list = nil
                        for lambda-body = nil
                        do (unless (and (consp binding)
                                        (>= (silent-length binding) 2)
                                        (symbolp (setf name (car binding)))
                                        (listp (setf lambda-body (cddr binding)
                                                     lambda-list (cadr binding))))
                             (invalid-form "LABELS" expr))
                        collect (list name nil :function)
                        do (push (list name lambda-list lambda-body) new-bindings)))
            `(labels ,(loop for (name lambda-list lambda-body) in new-bindings
                            collect (list* name (macroexpand-lambda-list lambda-list lambda-body new-env)))
               ,@(macroexpand-block body new-env))))
         ((macrolet)
          (check-nargs>= nargs 1 "MACROLET" expr)
          (let ((bindings (cadr expr))
                (body (cddr expr)))
            `(macrolet ,bindings
               ,@(macroexpand-block
                  body
                  (make-lexenv
                   :parent env
                   :functions
                   (loop for binding in bindings
                         collect (let (name lambda-list lambda-body)
                                   (if (and (consp binding)
                                            (>= (silent-length binding) 2)
                                            (symbolp (setf name (car binding)))
                                            (listp (setf lambda-body (cddr binding)
                                                         lambda-list (cadr binding))))
                                       (list name
                                             (destructuring-bind (form-sym env-sym . expanded-body)
                                                 (macroexpand-macro-lambda-list lambda-list lambda-body nil)
                                               (lambda (form env)
                                                 (let ((catch-sym (make-symbol "RETURN")))
                                                   (catch catch-sym
                                                     (eval-block
                                                      expanded-body
                                                      (make-lexenv :parent nil
                                                                   :variables (list (list form-sym form)
                                                                                    (list env-sym env))
                                                                   :blocks (list (cons name catch-sym))))))))
                                             :macro)
                                       (invalid-form "MACROLET" expr)))))))))
         ((function)
          (check-nargs= nargs 1 "FUNCTION" expr)
          (let ((arg (cadr expr)))
            (cond
              ((symbolp arg)
               (if (eql :macro (nth-value 1 (lookup-function-in-lexenv arg env :fail nil)))
                   (error "~s is a macro, not a function" arg)
                   expr))
              ((consp arg)
               (case (car arg)
                 ((lambda)
                  (let (lambda-list lambda-body)
                    (unless (and (>= (silent-length arg) 2)
                                 (listp (setf lambda-body (cddr arg)
                                              lambda-list (cadr arg))))
                      (invalid-form "FUNCTION" expr))
                    `#'(lambda ,@(macroexpand-lambda-list lambda-list lambda-body env))))
                 ((setf)
                  (unless (and (= (silent-length arg) 2)
                               (symbolp (cadr arg)))
                    (invalid-form "FUNCTION" expr))
                  expr)
                 (t (invalid-form "FUNCTION" expr))))
              (t (invalid-form "FUNCTION" expr)))))
         ((multiple-value-call)
          (check-nargs= nargs 2 "MULTIPLE-VALUE-CALL" expr)
          (let ((function (cadr expr))
                (form (caddr expr)))
            `(multiple-value-call ,(macroexpand-all function env)
               ,(macroexpand-all form env))))
         ((multiple-value-prog1)
          (check-nargs>= nargs 1 "MULTIPLE-VALUE-PROG1" expr)
          (let ((form (cadr expr))
                (body (cddr expr)))
            `(multiple-value-prog1 ,(macroexpand-all form env)
               ,@(macroexpand-block body env))))
         ((the)
          (check-nargs= nargs 2 "THE" expr)
          (let ((type (cadr expr))
                (form (caddr expr)))
            `(the ,type ,(macroexpand-all form env))))
         ((if)
          (unless (or (eql nargs 2) (= nargs 3))
            (error "incorrect number of arguments for IF:~% ~s" expr))
          (let ((cond (cadr expr))
                (then (caddr expr))
                (else (cadddr expr)))
            `(if ,(macroexpand-all cond env)
                 ,(macroexpand-all then env)
                 ,(macroexpand-all else env))))
         ((progn)
          (check-nargs>= nargs 0 "PROGN" expr)
          `(progn ,@(macroexpand-block (cdr expr) env)))
         ((progv)
          (check-nargs>= nargs 2 "PROGN" expr)
          (let ((vars (cadr expr))
                (vals (caddr expr))
                (body (cdddr expr)))
            `(progv ,(macroexpand-all vars env) ,(macroexpand-all vals env)
               ,@(macroexpand-block body env))))
         ((unwind-protect)
          (check-nargs>= nargs 1 "UNWIND-PROTECT" expr)
          (let ((protected (cadr expr))
                (cleanup (cddr expr)))
            `(unwind-protect
                 ,(macroexpand-all protected env)
              ,@(macroexpand-block cleanup env))))
         ((quote)
          (check-nargs= nargs 1 "QUOTE" expr)
          expr)
         (t
          (let ((form (car expr)))
            (list*
             (if (symbolp form)
                 form
                 (let (lambda-list lambda-body)
                   (if (and (consp form)
                            (eql 'lambda (car form))
                            (>= (silent-length form) 2)
                            (listp (setf lambda-body (cddr form)
                                         lambda-list (cadr form))))
                       `(lambda ,@(macroexpand-lambda-list lambda-list lambda-body env))
                       (invalid-form "FUNCTION" expr))))
             (loop for arg in (cdr expr) collect (macroexpand-all arg env))))))))
    (t expr)))

(defun interp-macroexpand-1 (expr &optional env)
  (cond
    ((symbolp expr)
     (multiple-value-bind (val kind) (lookup-variable-in-lexenv expr env :fail nil)
       (case kind
         ((:macro) (values val t))
         (t (values expr nil)))))
    ((consp expr)
     (let ((form (car expr)))
       (if (symbolp form)
           (multiple-value-bind (func kind) (lookup-function-in-lexenv form env :fail nil)
             (case kind
               ((:macro)
                (values
                 (funcall (coerce-to-function (gethash-strict '*macroexpand-hook* *symbol-values*))
                           func expr env)
                 t))
               (t (values expr nil))))
           (values expr nil))))
    (t (values expr nil))))

(defun interp-macroexpand (expr &optional env)
  (loop with expanded-once
        do (multiple-value-bind (new-expr expanded) (interp-macroexpand-1 expr env)
             (when expanded
               (setq expanded-once t))
             (if expanded
                 (setq expr new-expr)
                 (return (values new-expr expanded-once))))))

(defvar *print-forms* nil)

(defun interp-eval (form)
  (let ((expanded-form (macroexpand-all form nil)))
    (when *print-forms*
      (format t "~&~s" expanded-form))
    (eval-in-lexenv expanded-form nil)))

(defun interp-load (file)
  (with-open-file (stream file)
    (let ((eof (make-symbol "EOF"))
          (*package* (find-package "FN-INT")))
      (loop for form = (read stream nil eof)
            until (eql form eof)
            do (interp-eval form)
               (setq *package* (gethash-strict '*package* *symbol-values*))))))

(defparameter eof (make-symbol "EOF"))

(defun repl ()
  (loop (format t "~&> ")
        (let ((string (read-line)))
          (when (equal string "quit")
            (return))
          (with-input-from-string (s string)
            (loop with form
                  until (eql eof (setf form (read s nil eof)))
                  do (format t "~s~%" (interp-eval form)))))))

(define-constant fr-int::t t)
(define-constant fr-int::nil nil)
(define-constant fr-int::call-arguments-limit (expt 2 62))
(define-constant fr-int::lambda-parameters-limit (expt 2 62))
(define-constant fr-int::multiple-values-limit (expt 2 62))
(define-parameter fr-int::*macroexpand-hook* 'funcall)
(define-parameter fr-int::*package* (find-package "FN-INT"))
(define-parameter fr-int::*keyword-package* (find-package "KEYWORD"))

(define-special-forms
  let let* symbol-macrolet setq
  block return-from tagbody go
  catch throw
  load-time-value eval-when
  locally function quote
  flet labels macrolet
  multiple-value-call
  multiple-value-prog1
  the if progn progv
  unwind-protect)

(define-builtin-functions
  values error print eq eql equal
  + - = < > <= >= evenp oddp integerp
  list list* cons consp null endp listp nconc append not
  car cdr rplaca rplacd
  symbolp make-symbol symbol-name symbol-package keywordp intern
  stringp string= char code-char char-code characterp
  sb-impl::comma-p sb-impl::comma-expr sb-impl::comma-kind sb-impl::unquote)

(define-function fr-int::info (category sym)
  (info category sym))

(define-setf-function fr-int::info (new category sym)
  (setf (info category sym) new))

(define-function fr-int::funcall (function &rest arguments)
  (interp-apply (coerce-to-function function) arguments))

(define-function fr-int::apply (function arg &rest arguments)
  (interp-apply (coerce-to-function function) (apply #'list* arg arguments)))

(define-function fr-int::%defmacro (name function)
  (setf (info :function name) :macro)
  (setf (gethash name *symbol-functions*)
        (make-interp-macro-function :function function))
  name)

(defun function-name-p (name)
  (or (symbolp name)
      (and (consp name)
           (eql 'setf (car name))
           (eql 2 (silent-length name))
           (symbolp (cadr name)))))

(define-function fr-int::function-name-p (name)
  (function-name-p name))

(define-function fr-int::function-name-block-name (name)
  (etypecase name
    (symbol name)
    ((cons (eql setf) (cons symbol null))
     (cadr name))))

(define-function fr-int::%defun (name function)
  (unless (function-name-p name)
    (error "invalid function name ~s" name))
  (cond
    ((symbolp name)
     (setf (info :function name) :function)
     (setf (gethash name *symbol-functions*) function))
    ((and (consp name) (eql (car name) 'setf))
     (setf (info :setf-function (cadr name)) function)))
  name)

(define-function fr-int::%defvar (name &optional (value nil value-p))
  (setf (info :variable name) :special)
  (when value-p
    (unless (nth-value 1 (gethash name *symbol-values*))
      (setf (gethash name *symbol-values*) value)))
  name)

(define-function fr-int::%defparameter (name value)
  (setf (info :variable name) :special)
  (setf (gethash name *symbol-values*) value)
  name)

(define-function fr-int::%defconstant (name value)
  (setf (info :variable name) :constant)
  (setf (gethash name *symbol-values*) value)
  name)

(define-function fr-int::proclaim (forms)
  (loop for form in forms
        do (when (and (consp form) (eql 'special (car form)))
             (loop for name in (cdr form)
                   do (setf (info :variable name) :special)))))

(define-function fr-int::find-package-strict (package-designator)
  (or (find-package package-designator)
      (error "package not found ~s" package-designator)))

(define-function fr-int::boundp (name)
  (unless (symbolp name)
    (error "the argument to BOUNDP is not of type symbol: ~s" name))
  (nth-value 1 (get-symbol-value name)))

(define-function fr-int::makunbound (name)
  (unless (symbolp name)
    (error "the argument to BOUNDP is not of type symbol: ~s" name))
  (when (constant-p name)
    (error "can not modify the symbol-value of the constant ~s" name))
  (remhash name *symbol-values*)
  name)

(defun interp-fboundp (name)
  (unless (function-name-p name)
    (error "invalid function name ~s" name))
  (cond
    ((symbolp name)
     (multiple-value-bind (func found) (gethash name *symbol-functions*)
       (when found func)))
    ((and (consp name) (eql 'setf (car name)))
     (info :setf-function (cadr name)))))

(define-function fr-int::fboundp (name)
  (interp-fboundp name))

(define-function fr-int::fdefinition (name)
  (unless (interp-fboundp name)
    (error 'undefined-function :name name))
  (cond
    ((symbolp name)
     (nth-value 0 (gethash name *symbol-functions*)))
    ((and (consp name) (eql 'setf (car name)))
     (info :setf-function (cadr name)))))

(define-setf-function fr-int::fdefinition (new name)
  (unless (function-name-p name)
    (error "invalid function name ~s" name))
  (cond
    ((symbolp name)
     (setf (info :function name) :function)
     (setf (gethash name *symbol-functions*) new))
    ((and (consp name) (eql 'setf (car name)))
     (setf (info :setf-function (cadr name)) new))))

(define-function fr-int::fmakunbound (name)
  (unless (function-name-p name)
    (error "invalid function name ~s" name))
  (when (symbolp name)
    (if (eql :special-form (info :function name))
        (error "can not FMAKUNBOUND special form ~s" name)
        (setf (info :function name) nil)))
  (cond
    ((symbolp name)
     (remhash name *symbol-functions*))
    ((and (consp name) (eql 'setf (car name)))
     (setf (info :setf-function (cadr name)) nil))))

(define-function fr-int::macro-p (object)
  (interp-macro-function-p object))

(define-function special-operator-p (object)
  (interp-special-operator-p object))

(define-function fr-int::function-lambda-expression (function)
  (declare (ignore function))
  (error "unimplemented"))

(define-function fr-int::functionp (object)
  (or (functionp object)
      (interp-macro-function-p object)
      (interp-special-operator-p object)))

(define-function fr-int::compiled-function-p (object)
  (declare (ignore object))
  (error "unimplemented"))

(define-function fr-int::constantp (name)
  (cond
    ((symbolp name)
     (constant-p name))
    ((atom name) t)))

(define-function fr-int::check-lambda-list (list end1 end2 form)
  (let ((length (length list)))
    (when (< length end1)
      (error "not enough arguments to lambda-list ~s~% ~s" form list))
    (when (> length end2)
      (error "too many arguments to lambda-list ~s~% ~s" form list)))
  list)

(define-function fr-int::check-lambda-list-rest (list end1 form)
  (let ((length (length list)))
    (when (< length end1)
      (error "not enough arguments to lambda-list ~s~% ~s" form list)))
  list)

(define-macro fr-int::macro-lambda (lambda-list &body body)
  (let ((form-sym (make-symbol "FORM"))
        (env-sym (make-symbol "ENV")))
    `#'(lambda (,form-sym ,env-sym)
         (declare (ignore ,env-sym))
         (let* ,(simple-expand-lambda-list lambda-list `(cdr ,form-sym))
           ,@body))))

(define-function fr-int::symbol-value (symbol)
  (multiple-value-bind (val found)
      (get-symbol-value symbol)
    (unless found
      (error 'unbound-variable :name symbol))
    val))

(define-setf-function fr-int::symbol-value (new symbol)
  (if (keywordp symbol)
      (error "can not set the value of a keyword")
      (setf (gethash symbol *symbol-values*) new)))

(define-function fr-int::symbol-function (symbol)
  (multiple-value-bind (val found)
      (gethash symbol *symbol-functions*)
    (unless found
      (error 'unbound-variable :name symbol))
    val))

(define-setf-function fr-int::symbol-function (new symbol)
  (setf (gethash symbol *symbol-functions*) new))

(define-function fr-int::symbol-plist (symbol)
  (gethash symbol *symbol-plists*))

(define-setf-function fr-int::symbol-plist (new symbol)
  (setf (gethash symbol *symbol-plists*) new))

(define-function fr-int::macroexpand (expr &optional env)
  (interp-macroexpand expr env))

(define-function fr-int::macroexpand-1 (expr &optional env)
  (interp-macroexpand-1 expr env))

(define-function fr-int::%define-setf-expander (access-fn fn)
  (setf (info :setf-expander access-fn) fn))

(define-function fr-int::get-setf-expander (name &optional env)
  (if (local-function-in-lexenv-p name env)
      nil
      (info :setf-expander name)))

(define-function fr-int::coerce-to-function (val)
  (coerce-to-function val))

(define-function fr-int::equalp (x y)
  (declare (ignore x y))
  (error "unimplemented"))

(define-function fr-int::%make-simple-vector (n)
  (make-array n))

(define-function fr-int::%make-simple-string (n)
  (make-array n :element-type 'character))
(define-function fr-int::%make-simple-vector-base-char (n)
  (make-array n :element-type 'base-char))
(define-function fr-int::%make-simple-vector-bit (n)
  (make-array n :element-type 'bit))
(define-function fr-int::%make-simple-vector-u8 (n)
  (make-array n :element-type '(unsigned-byte 8)))
(define-function fr-int::%make-simple-vector-u16 (n)
  (make-array n :element-type '(unsigned-byte 16)))
(define-function fr-int::%make-simple-vector-u32 (n)
  (make-array n :element-type '(unsigned-byte 32)))
(define-function fr-int::%make-simple-vector-i8 (n)
  (make-array n :element-type '(signed-byte 8)))
(define-function fr-int::%make-simple-vector-i16 (n)
  (make-array n :element-type '(signed-byte 16)))
(define-function fr-int::%make-simple-vector-i32 (n)
  (make-array n :element-type '(signed-byte 32)))

(define-function fr-int::%simple-vector-p (obj)
  (simple-vector-p obj))

(define-function fr-int::%simple-vector-length (obj)
  (declare (type simple-vector obj))
  (length obj))

(defun double-float-infinity-p (obj)
  (and (typep obj 'double-float)
       (sb-ext:float-infinity-p obj)))
(defun double-float-nan-p (obj)
  (and (typep obj 'double-float)
       (sb-ext:float-nan-p obj)))

(define-function fr-int::%normal-double-float-p (obj)
  (and (typep obj 'double-float)
       (not (sb-kernel:float-infinity-or-nan-p obj))))

(deftype ptr-type ()
  `(or symbol cons simple-vector compiled-function interp-object))

(define-function fr-int::%boxed-tag (obj)
  (etypecase obj
    ((satisfies double-float-infinity-p) 0)
    ((satisfies double-float-nan-p) 4)
    ((or single-float character) 5)
    ((signed-byte 49) 6)
    (ptr-type 7)))

(define-function fr-int::%boxed-sector-32-tag (obj)
  (etypecase obj
    (single-float 0)
    (character 1)))

(define-function fr-int::%boxed-ptr-tag (obj)
  (etypecase obj
    (null 0)
    (cons 1)
    (symbol 2)
    (simple-vector 3)
    (compiled-function 4)
    (interp-object 7)))

(define-function fr-int::%simple-vector-type-tag (obj)
  (declare (type simple-vector obj))
  (etypecase (array-element-type obj)
      ((eql t) 0)
      ((eql bit) 3)
      ((eql base-char) 4)
      ((eql character) 5)
      ((eql single-float) 6)
      ((cons (eql unsigned-byte) (cons (eql 8) nil)) 8)
      ((cons (eql unsigned-byte) (cons (eql 16) nil)) 9)
      ((cons (eql unsigned-byte) (cons (eql 32) nil)) 10)
      ((cons (eql signed-byte) (cons (eql 8) nil)) 12)
      ((cons (eql signed-byte) (cons (eql 16) nil)) 13)
      ((cons (eql signed-byte) (cons (eql 32) nil)) 14)))

(define-function fr-int::%simple-vector-aref (array n)
  (aref (the simple-vector array) n))

(define-setf-function fr-int::%simple-vector-aref (new array n)
  (setf (aref (the simple-vector array) n) new))

(define-function fr-int::%make-object (class n)
  (make-interp-object :class class :slots (make-array n)))

(define-function fr-int::%object-slot (object n)
  (aref (interp-object-slots object) n))

(define-setf-function fr-int::%object-slot (new object n)
  (setf (aref (interp-object-slots object) n) new))

(define-function fr-int::%object-class (object)
  (interp-object-class object))

(define-setf-function fr-int::%object-class (new object)
  (setf (interp-object-class object) new))

(define-function fr-int::%object-length (object)
  (length (interp-object-slots object)))

(define-function fr-int::%string-append-2 (str1 str2)
  (cond ((= 0 (length str1)) str2)
        ((= 0 (length str2)) str1)
        (t (let ((ret (make-array (+ (length str1) (length str2)) :element-type 'character)))
             (replace ret str1)
             (replace ret str2 :start1 (length str1))))))

(define-function fr-int::%value-id (val)
  ;; This is the equivalent of obj-to-sap but returns an integer
  (declare (ignore val))
  (error "unimplemented"))

(define-function fr-int::%eq-hash (val)
  (declare (ignore val))
  (error "unimplemented"))

(define-function fr-int::%eql-hash (val)
  (declare (ignore val))
  (error "unimplemented"))

(define-function fr-int::%equal-hash (val)
  (declare (ignore val))
  (error "unimplemented"))

(define-function fr-int::%equalp-hash (val)
  (declare (ignore val))
  (error "unimplemented"))

(define-function fr-int::%read-char (fd)
  (ecase fd
    (0 (read-char *standard-input* nil nil))
    (1 (error "Can not read from stdout."))
    (2 (error "Can not read from stderr."))))

(define-function fr-int::%write-char (fd ch)
  (ecase fd
    (0 (error "Can not write to stdin."))
    (1 (write-char ch *standard-output*))
    (2 (write-char ch *error-output*))))

(define-function fr-int::eval (form)
  (interp-eval form))

;; ORDER:
;; base.lisp
;; iter.lisp
;; lambda-list.lisp
;; setf.lisp
;; list.lisp
;; type.lisp
;; condition.lisp
;; symbol.lisp
;; character.lisp
;; array.lisp
;; seq.lisp
;; hash-table.lisp
;; file.lisp
;; stream.lisp
;; print.lisp

