(defmacro define-setf-expander (access-fn lambda-list &body body)
  (let ((form-sym (make-symbol "FORM"))
        (env-sym (make-symbol "ENV")))
    `(%define-setf-expander
      ',access-fn
      (lambda (,form-sym ,env-sym)
        (declare (ignorable ,env-sym))
        (block ,access-fn
          (let* ,(expand-macro-lambda-list lambda-list form-sym env-sym)
            ,@body))))))

(defun get-setf-args (args)
  (with-ext-lists ((vars vars-end)
                   (forms forms-end)
                   (new-args new-args-end))
    (loop for arg in args
          for sym = (make-symbol "TMP")
          do (if (constantp arg)
                 (push-tail arg new-args-end)
                 (progn
                   (push-tail sym vars-end)
                   (push-tail arg forms-end)
                   (push-tail sym new-args-end))))
    (values vars forms new-args)))

(defun get-setf-expansion (form &optional env)
  (block nil
    (if (atom form)
        (let ((new (make-symbol "NEW")))
          (multiple-value-bind (form expanded) (macroexpand-1 form env)
            (when expanded
              (return (get-setf-expansion form env))))
          (values nil nil (list new) `(setq ,form ,new) form))
        (let ((functor (car form)))
          (let ((expander (get-setf-expander functor env)))
            (when expander
              (return (funcall expander form env))))
          (multiple-value-bind (form expanded) (macroexpand-1 form env)
            (when expanded
              (return (get-setf-expansion form env))))
          (let ((new (make-symbol "NEW")))
            (multiple-value-bind (vars forms args) (get-setf-args (cdr form))
              (values vars forms (list new)
                      `(funcall #'(setf ,functor) ,new ,@args)
                      `(,functor ,@args))))))))

(defmacro simple-defsetf (access-fn update-fn)
  `(%define-setf-expander
    ',access-fn
    (lambda (form env)
      (declare (ignorable env))
      (let ((new (make-symbol "NEW")))
        (multiple-value-bind (vars forms args) (get-setf-args (cdr form))
          (values vars forms (list new)
                  (list* ',update-fn (append args (list new)))
                  (list* ',access-fn args)))))))

(defmacro expanded-defsetf (access-fn lambda-list new-args &body body)
  (let ((form-sym (make-symbol "FORM"))
        (env-sym (make-symbol "ENV"))
        (vars-sym (make-symbol "VARS"))
        (forms-sym (make-symbol "FORMS"))
        (args-sym (make-symbol "ARGS"))
        (new (loop for new-arg in new-args
                   collect (make-symbol (symbol-name new-arg)))))
    `(%define-setf-expander
      ',access-fn
      (lambda (,form-sym ,env-sym)
        (declare (ignorable ,env-sym))
        (block ,access-fn
          (multiple-value-bind (,vars-sym ,forms-sym ,args-sym) (get-setf-args (cdr ,form-sym))
            (setq ,form-sym (list* ',access-fn ,args-sym))
            (let* (,@(loop for new-arg in new-args
                           for sym in new
                           collect (list new-arg sym))
                   ,@(expand-macro-lambda-list lambda-list form-sym env-sym))
              (values ,vars-sym ,forms-sym ',new (progn ,@body) ,form-sym))))))))

(defmacro defsetf (access-fn first-arg &rest args)
  (if (symbolp first-arg)
      `(simple-defsetf ,access-fn ,first-arg ,@args)
      `(expanded-defsetf ,access-fn ,first-arg ,@args)))

(defmacro setf (&rest args &environment env)
  (unless (evenp (list-length args))
    (error "odd number of arguments to SETF: ~s" args))
  (loop while args
        collect (multiple-value-bind (vars forms new setter) (get-setf-expansion (car args) env)
                  (let ((form setter))
                    (when (cdr new)
                      (setq form `(multiple-value-bind ,new ,(cadr args) ,form)))
                    (when (or vars (not (cdr new)))
                      (setq form `(let (,@(loop for var in vars and form in forms collect (list var form))
                                        ,@(unless (cdr new) `((,(car new) ,(cadr args)))))
                                    ,form)))
                    form))
          into ret
        do (setq args (cddr args))
        finally (return (prognify ret))))

(defmacro psetf (&rest args &environment env)
  (unless (evenp (list-length args))
    (error "odd number of arguments to PSETF: ~s" args))
  (let (headers)
    (with-ext-lists ((setters setters-end))
      (loop while args
            do (multiple-value-bind (vars forms new setter) (get-setf-expansion (car args) env)
                 (let ((let-bindings (append (loop for var in vars and form in forms collect (list var form))
                                             (unless (cdr new)
                                               `((,(car new) ,(cadr args))))))
                       (mv-bind (when (cdr new)
                                  (list new (cadr args)))))
                   
                   (when let-bindings
                     (unless (eql 'let* (caar headers))
                       (setq headers (cons (list 'let* nil) headers)))
                     (rplaca (cdar headers)
                             (append (cadar headers) let-bindings)))
                   (when mv-bind
                     (setq headers (cons (list* 'multiple-value-bind mv-bind) headers)))
                   (push-tail setter setters-end)))
               (setq args (cddr args)))
      (push-tail nil setters-end)
      (let ((forms setters))
        (loop for header in headers
              do (setq forms (list (append header forms)))
              finally (return (prognify forms)))))))

(defmacro shiftf (&rest args &environment env)
  (unless (cddr args)
    (error "SHIFTF needs at least two arguments but got: ~s" args))
  (let (headers out)
    (with-ext-lists ((setters setters-end))
      (loop while args
            with first = t
            with prev-new
            do (let (let-bindings mv-bind)
                 (if (cdr args)
                     (multiple-value-bind (vars forms new setter getter) (get-setf-expansion (car args) env)
                       (when first
                         (setq out (loop for var in new collect (make-symbol "OUT"))
                               prev-new out))
                       (setq let-bindings (append (loop for var in vars and form in forms collect (list var form))
                                                  (unless (cdr prev-new)
                                                    `((,(car prev-new) ,getter))))
                             mv-bind (when (cdr prev-new)
                                       (list prev-new getter))
                             prev-new new)
                       (push-tail setter setters-end))
                     (setq let-bindings (unless (cdr prev-new)
                                          (list (list (car prev-new) (car args))))
                           mv-bind (when (cdr prev-new)
                                     (list prev-new (car args)))))
                 (when let-bindings
                   (unless (eql 'let* (caar headers))
                     (setq headers (cons (list 'let* nil) headers)))
                   (rplaca (cdar headers)
                           (append (cadar headers) let-bindings)))
                 (when mv-bind
                   (setq headers (cons (list* 'multiple-value-bind mv-bind) headers))))
               (setq args (cdr args)
                     first nil))
      (push-tail `(values ,@out) setters-end)
      (let ((forms setters))
        (loop for header in headers
              do (setq forms (list (append header forms)))
              finally (return (prognify forms)))))))

(defmacro rotatef (&rest args &environment env)
  (unless (cddr args)
    (error "ROTATEF needs at least two arguments but got: ~s" args))
  (with-ext-lists ((varss varss-end)
                   (formss formss-end)
                   (news news-end)
                   (setters setters-end)
                   (getters getters-end))
    (loop for arg in args
          do (multiple-value-bind (vars forms new setter getter) (get-setf-expansion args env)
               (push-tail vars varss-end)
               (push-tail forms formss-end)
               (push-tail new news-end)
               (push-tail setter setters-end)
               (push-tail getter getters-end)))
    (let (headers (news (cons (car news-end) news)))
      (loop for vars in varss and forms in formss and new in news and setter in setters and getter in getters
            do (let* ((let-bindings (append (loop for var in vars and form in forms collect (list var form))
                                            (unless (cdr new)
                                              `((,(car new) ,getter)))))
                      (mv-bind (when (cdr new)
                                 (list new getter))))
                 (when let-bindings
                   (unless (eql 'let* (caar headers))
                     (setq headers (cons (list 'let* nil) headers)))
                   (rplaca (cdar headers)
                           (append (cadar headers) let-bindings)))
                 (when mv-bind
                   (setq headers (cons (list* 'multiple-value-bind mv-bind) headers)))))
      (push-tail nil setters-end)
      (let ((forms setters))
        (loop for header in headers
              do (setq forms (list (append header forms)))
              finally (return (prognify forms)))))))

(define-setf-expander values (&rest places &environment env)
  (with-ext-lists ((vars vars-end)
                   (forms forms-end)
                   (new new-end)
                   (setters setters-end)
                   (getters getters-end))
    (loop for place in places
          do (multiple-value-bind (local-vars local-forms local-new setter getter) (get-setf-expansion place env)
               (append-tail local-vars vars-end)
               (append-tail (cdr local-new) vars-end)
               (append-tail local-forms forms-end)
               (append-tail (loop for sym in (cdr local-new) collect nil) forms-end)
               (when local-new
                 (push-tail (car local-new) new-end))
               (push-tail setter setters-end)
               (push-tail getter getters-end)))
    (values vars forms new `(values ,@setters) `(values ,@getters))))

(define-setf-expander the (value-type form &environment env)
  (multiple-value-bind (vars forms new setter getter) (get-setf-expansion form env)
    (values vars forms new
            `(multiple-value-bind ,new (the ,value-type (values ,@new))
               ,setter)
            `(the ,value-type ,getter))))

(define-setf-expander apply (function &rest args)
  (unless (and (consp function)
               (= 2 (list-length function))
               (eql 'function (car function))
               (symbolp (cadr function)))
    (error "SETF expander of APPLY expected a function of the form #'<symbol> but got: ~s" function))
  (let ((new (make-symbol "NEW")))
    (multiple-value-bind (vars forms args) (get-setf-args args)
      (values vars forms (list new)
              `(apply #'(setf ,(cadr function)) ,new ,@args)
              `(apply ,function ,@args)))))

(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (vars forms old-new setter getter) (get-setf-expansion place env)
    (let ((new (make-symbol "NEW"))
          (indicator-sym (make-symbol "INDICATOR"))
          (default-sym (make-symbol "DEFAULT")))
      (unless (constantp indicator)
        (setq vars (append vars (list indicator-sym))
              forms (append forms (list indicator))
              indicator indicator-sym))
      (unless (constantp default)
        (setq vars (append vars (list default-sym))
              forms (append forms (list default))
              default default-sym))
      (values vars forms (list new)
              `(let ((,(car old-new) (%putf ,getter ,indicator ,new))
                     ,@(cdr old-new))
                 ,@(when (eq default default-sym)
                     (list default))
                 ,setter
                 ,new)
              `(getf ,getter ,indicator ,default)))))

(defmacro remf (place indicator &environment env)
  (multiple-value-bind (vars forms new setter getter) (get-setf-expansion place env)
    (let* ((flag (make-symbol "FLAG"))
           (form `(multiple-value-bind (,(car new) ,flag)
                      (%remf ,getter ,indicator)
                    ,(if (cdr new) `(let ,(cdr new) ,setter) setter)
                    ,flag)))
      (if vars
          `(let ,(loop for var in vars for form in forms collect (list var form))
             ,form)
          form))))

(defun read-write-modify (preceding-forms place following-forms fn env)
  (multiple-value-bind (pre-vars pre-forms pre-args) (get-setf-args preceding-forms)
    (multiple-value-bind (vars forms new setter getter) (get-setf-expansion place env)
      (multiple-value-bind (post-vars post-forms post-args) (get-setf-args following-forms)
        (let ((read (make-symbol "READ")))
          (multiple-value-call
              (lambda (new-form &optional (return nil return-p))
                `(let* (,@(loop for var in pre-vars and form in pre-forms collect (list var form))
                        ,@(loop for var in vars and form in forms collect (list var form))
                        ,@(loop for var in post-vars and form in post-forms collect (list var form))
                        (,read ,getter)
                        (,(car new) ,new-form)
                        ,@(loop for store in (cdr new) collect (list store nil)))
                   ,setter
                   ,@(when return-p
                       (list return))))
            (apply fn (append pre-args (list read) post-args))))))))

(defmacro define-modify-macro (name lambda-list function &optional documentation)
  (let ((place (make-symbol "PLACE"))
        (env (make-symbol "ENV")))
    `(defmacro ,name (,place ,@lambda-list &environment ,env)
       (read-write-modify nil ,place ,(expand-modify-macro-lambda-list lambda-list)
                          (lambda (&rest rest)
                            (list* ',function rest))
                          ,env))))

(defmacro incf (place &optional (delta 1) &environment env)
  (read-write-modify () place (list delta) (lambda (place delta) `(+ ,place ,delta)) env))

(defmacro incf* (place &optional (delta 1) &environment env)
  (read-write-modify () place (list delta) (lambda (place delta) (values `(+ ,place ,delta) place)) env))

(defmacro decf (place &optional (delta 1) &environment env)
  (read-write-modify () place (list delta) (lambda (place delta) `(- ,place ,delta)) env))

(defmacro decf* (place &optional (delta 1) &environment env)
  (read-write-modify () place (list delta) (lambda (place delta) (values `(- ,place ,delta) place)) env))

(defmacro pop (place &environment env)
  (read-write-modify () place () (lambda (place) (values `(cdr ,place) `(car ,place))) env))

(defmacro push (obj place &environment env)
  (read-write-modify (list obj) place () (lambda (obj place) `(cons ,obj ,place)) env))

(defmacro pushnew (obj place &key key (test nil test-p) (test-not nil test-not-p)  &environment env)
  (read-write-modify (list obj) place (list key test test-not)
                     (lambda (obj place key test test-not)
                       `(adjoin ,obj ,place
                                ,@(when test-p `(:test ,test))
                                ,@(when test-not-p `(:test-not ,test-not))))
                     env))
