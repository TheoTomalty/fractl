#+boot
(eval-when (:compile-toplevel :load-toplevel :execute)
  (%defmacro 'comptime
             (macro-lambda (&rest body)
                           (list* 'eval-when '(:compile-toplevel :load-toplevel :execute) body))))

#+boot
(comptime
 (%defun 'split-declare
         #'(lambda (forms)
             (labels ((aux ()
                        (if (consp forms)
                            (let ((form (car forms)))
                              (if (consp form)
                                  (if (eql 'declare (car form))
                                      (progn
                                        (setq forms (cdr forms))
                                        (list* form (aux)))))))))
               (values (aux) forms))))

 (%defmacro 'defmacro
            (macro-lambda (name args &rest body)
                          (multiple-value-call
                              #'(lambda (declarations forms)
                                  (list 'comptime
                                        (list '%defmacro (list 'quote name)
                                              (append
                                               (list 'macro-lambda args)
                                               declarations
                                               (list (list* 'block (function-name-block-name name) forms))))))
                            (split-declare body)))))

#+boot
(defmacro in-package (package-designator)
  (list 'comptime (list 'setq '*package* (list 'find-package-strict (list 'quote package-designator)))))

(in-package "FCL")


#+boot
(defmacro lambda (&rest rest)
  (list 'function (list* 'lambda rest)))

#+boot
(defmacro defun (name args &rest body)
  (multiple-value-call
      (lambda (declarations forms)
        (list '%defun (list 'quote name)
              (append (list 'lambda args)
                      declarations
                      (list (list* 'block (function-name-block-name name) forms)))))
    (split-declare body)))

#+boot
(defmacro defconstant (name val)
  (list 'comptime (list '%defconstant (list 'quote name) val)))

#+boot
(defmacro defparameter (name val)
  (list '%defparameter (list 'quote name) val))

#+boot
(defmacro defvar (name &rest args)
  (if args
      (list '%defvar (list 'quote name) (list 'if (list 'boundp (list 'quote name)) nil (car args)))
      (list '%defvar (list 'quote name))))

#+boot
(defmacro define-ca/dr (name &rest ops)
  (labels ((aux (ops)
             (if (null ops)
                 'list
                 (list (car ops) (aux (cdr ops))))))
    (list 'defun name (list 'list) (aux ops))))

#+boot
(progn
  (define-ca/dr caar car car)
  (define-ca/dr cadr car cdr)
  (define-ca/dr cdar cdr car)
  (define-ca/dr cddr cdr cdr)
  (define-ca/dr caaar car car car)
  (define-ca/dr caadr car car cdr)
  (define-ca/dr cadar car cdr car)
  (define-ca/dr caddr car cdr cdr)
  (define-ca/dr cdaar cdr car car)
  (define-ca/dr cdadr cdr car cdr)
  (define-ca/dr cddar cdr cdr car)
  (define-ca/dr cdddr cdr cdr cdr)
  (define-ca/dr caaaar car car car car)
  (define-ca/dr caaadr car car car cdr)
  (define-ca/dr caadar car car cdr car)
  (define-ca/dr caaddr car car cdr cdr)
  (define-ca/dr cadaar car cdr car car)
  (define-ca/dr cadadr car cdr car cdr)
  (define-ca/dr caddar car cdr cdr car)
  (define-ca/dr cadddr car cdr cdr cdr)
  (define-ca/dr cdaaar cdr car car car)
  (define-ca/dr cdaadr cdr car car cdr)
  (define-ca/dr cdadar cdr car cdr car)
  (define-ca/dr cdaddr cdr car cdr cdr)
  (define-ca/dr cddaar cdr cdr car car)
  (define-ca/dr cddadr cdr cdr car cdr)
  (define-ca/dr cdddar cdr cdr cdr car)
  (define-ca/dr cddddr cdr cdr cdr cdr))

#+boot
(defun 1+ (n)
  (+ n 1))

#+boot
(defun 1- (n)
  (- n 1))

#+boot
(defun atom (x)
  (not (consp x)))

(defmacro push-tail (val tail)
  (list 'rplacd tail (list 'setq tail (list 'list val))))

(defun %append-tail (list tail)
  (tagbody
   begin
     (if (consp list) nil (go end))
     (push-tail (car list) tail)
     (setq list (cdr list))
     (go begin)
   end
     (return-from %append-tail (rplacd tail list))))

(defmacro append-tail (val tail)
  (list 'setq tail (list '%append-tail val tail)))

(defmacro nconc-tail (val tail)
  (list 'setq tail (list 'last (list 'rplacd tail val))))

(defmacro with-ext-lists (pairs &rest body)
  (let* ((bindings-head (list nil))
         (bindings-tail bindings-head)
         (symbol-macros-head (list nil))
         (symbol-macros-tail symbol-macros-head)
         (ignorable-head (list nil))
         (ignorable-tail ignorable-head))
    (tagbody
     begin
       (if (null pairs) (go end))
       (let* ((pair (car pairs))
              (head-sym (make-symbol "HEAD"))
              (head-user-sym (car pair))
              (tail-sym (cadr pair)))
         (push-tail (list head-sym '(list nil)) bindings-tail)
         (push-tail (list tail-sym head-sym) bindings-tail)
         (push-tail (list head-user-sym (list 'cdr head-sym))
                    symbol-macros-tail)
         (push-tail head-sym ignorable-tail)
         (push-tail tail-sym ignorable-tail))
       (setq pairs (cdr pairs))
       (go begin)
     end)
    (list 'let* (cdr bindings-head)
          (list 'declare (list* 'ignorable (cdr ignorable-head)))
          (list* 'symbol-macrolet (cdr symbol-macros-head)
                 body))))

#+boot
(defmacro multiple-value-list (value-form)
  (list 'multiple-value-call (list 'function 'list) value-form))

#+boot
(defmacro multiple-value-bind (vars value-form &rest body)
  (let ((ignore (make-symbol "IGNORE")))
    (list 'multiple-value-call
          (list 'function (list* 'lambda (list* '&optional (append vars (list '&rest ignore)))
                                 (list 'declare (list 'ignore ignore))
                                 body))
          value-form)))

#+boot
(defmacro multiple-value-setq (vars value-form)
  (if vars
      (with-ext-lists ((syms syms-end)
                       (update update-end))
        (tagbody
         begin
           (if (null vars) (go end))
           (let ((sym (make-symbol "ARG")))
             (push-tail sym syms-end)
             (push-tail (car vars) update-end)
             (push-tail sym update-end))
           (setq vars (cdr vars))
           (go begin)
         end)
        (let ((ignore (make-symbol "IGNORE")))
          (list 'multiple-value-call
                (list 'function (list 'lambda (list* '&optional (append syms (list '&rest ignore)))
                                      (list 'declare (list 'ignore ignore))
                                      (list* 'setq update)
                                      (car syms)))
                value-form)))
      (list 'values value-form)))

#+boot
(defmacro nth-value (n form)
  (with-ext-lists ((syms syms-end))
    (tagbody
     begin
       (if (= n 0)
           (go end))
       (push-tail (make-symbol "IGNORE") syms-end)
       (setq n (- n 1))
       (go begin)
     end)
    (let ((keep (make-symbol "KEEP")))
      (list 'multiple-value-bind (append syms (list keep)) form
            (list 'declare (list* 'ignore syms))
            keep))))

#+boot
(defun values-list (list)
  (apply #'values list))

#+boot
(defmacro when (condition &rest body)
  (list 'if condition (list* 'progn body)))

#+boot
(defmacro unless (condition &rest body)
  (list 'if condition nil (list* 'progn body)))

#+boot
(defmacro cond (&rest clauses)
  (when clauses
    (let ((clause (car clauses)))
      (unless (consp (car clauses))
        (error "COND clause is not a cons ~s" (car clauses)))
      (list 'if (car clause) (list* 'progn (cdr clause)) (list* 'cond (cdr clauses))))))

#+boot
(defmacro or (&rest forms)
  (cond
    ((null forms) nil)
    ((cdr forms)
     (let ((sym (make-symbol "OR")))
       (list 'let (list (list sym (car forms)))
          (list 'if sym sym (list* 'or (cdr forms))))))
    (t (car forms))))

#+boot
(defmacro and (&rest forms)
  (labels ((aux (forms)
             (if (cddr forms)
                 (multiple-value-bind (cond last) (aux (cdr forms))
                   (values (list 'if (car forms) cond) last))
                 (values (car forms) (cadr forms)))))
    (multiple-value-bind (cond last) (aux forms)
      (list 'if cond last))))

(defun self-evaluating-p (form)
  (cond
    ((consp form) nil)
    ((eql t form) t)
    ((null form) t)
    ((keywordp form) t)
    ((symbolp form) nil)
    (t t)))

(defun maybe-quote (form)
  (if (self-evaluating-p form)
      form
      (list 'quote form)))

(defun orify (forms)
  (cond ((null forms) nil)
        ((cdr forms) (list* 'or forms))
        (t (car forms))))

(defun case-to-condition (arg-sym form &optional special-t-p)
  (labels ((condition (symbol)
             (list 'eql arg-sym (maybe-quote symbol)))
           (conditions (symbols)
             (cond
               ((null symbols) nil)
               ((consp symbols)
                (cons (condition (car symbols))
                      (conditions (cdr symbols))))
               (t (error "not a proper list")))))
    (cond
      ((consp form)
       (orify (conditions form)))
      ((null form) nil)
      ((and special-t-p (eql t form)) t)
      (t (condition form)))))

#+boot
(defmacro ecase (arg &rest clauses)
  (let ((arg-sym (make-symbol "ARG")))
    (labels ((aux (clauses)
               (if (consp clauses)
                   (let ((clause (car clauses)))
                     (list* (list*
                             (case-to-condition arg-sym (car clause))
                             (cdr clause))
                            (aux (cdr clauses))))
                   (list (list t (list 'error "~s fell through the ecase expression" arg-sym))))))
      (list 'let (list (list arg-sym arg)) (list* 'cond (aux clauses))))))

#+boot
(defmacro ccase (arg &rest clauses)
  (error "unimplemented"))

#+boot
(defmacro case (arg &rest clauses)
  (let ((arg-sym (make-symbol "ARG")))
    (labels ((aux (clauses)
               (when (consp clauses)
                 (let ((clause (car clauses)))
                   (list* (list*
                           (case-to-condition arg-sym (car clause) t)
                           (cdr clause))
                          (aux (cdr clauses)))))))
      (list 'let (list (list arg-sym arg)) (list* 'cond (aux clauses))))))

#+boot
(defmacro quasiquote (arg)
  (labels ((aux (arg level)
             (cond
               ((consp arg)
                (let ((car (car arg))
                      (cdr (cdr arg)))
                  (cond
                    ((and (sb-impl::comma-p car) (eql 0 level))
                     (let ((kind (sb-impl::comma-kind car))
                           (expr (sb-impl::comma-expr car)))
                       (ecase kind
                         (0
                          (list 'cons expr (aux cdr level)))
                         (1
                          (let ((rest (aux cdr level)))
                            (if rest
                                (list 'nconc expr rest)
                                expr)))
                         (2
                          (let ((rest (aux cdr level)))
                            (if rest
                                (list 'append expr rest)
                                expr))))))
                    ((eql 'quasiquote car)
                     (list 'cons '(quote quasiquote) (aux cdr (1+ level))))
                    (t
                     (list 'cons (aux car level) (aux cdr level))))))
               ((sb-impl::comma-p arg)
                (if (eql 0 level)
                    (progn
                      (unless (eql 0 (sb-impl::comma-kind arg))
                        (error "can not splice toplevel unquote"))
                      (sb-impl::comma-expr arg))
                    (list 'unquote (aux (sb-impl::comma-expr arg) (1- level)) (sb-impl::comma-kind arg))))
               (t (maybe-quote arg)))))
    (aux arg 0)))

#+boot
(defmacro psetq (&rest pairs)
  (with-ext-lists ((bindings bindings-end)
                   (body body-end))
    (let (name val)
      (tagbody
       begin
         (if (consp pairs)
             (setq name (car pairs))
             (go end))
         (setq pairs (cdr pairs))
         (if (consp pairs)
             (setq val (car pairs))
             (go error))
         (setq pairs (cdr pairs))
         (let ((sym (make-symbol "TMP")))
           (push-tail (list sym val) bindings-end)
           (push-tail (list 'setq name sym) body-end))
         (go begin)
       error
         (error "invalid PSETQ form:~% ~s" (list* 'psetq pairs))
       end))
    `(let ,bindings ,@body nil)))

#+boot
(defmacro return (&optional val)
  `(return-from nil ,val))

#+boot
(defmacro prog (bindings &body body)
  (list 'block nil (list 'let bindings (list* 'tagbody body))))

#+boot
(defmacro prog* (bindings &body body)
  (list 'block nil (list 'let* bindings (list* 'tagbody body))))

#+boot
(defmacro prog1 (result &rest body)
  (let ((var (make-symbol "VAR")))
    `(let ((,var ,result))
       ,@body
       ,var)))

#+boot
(defmacro prog2 (form1 result &rest body)
  (let ((var (make-symbol "VAR")))
    `(progn
       ,form1
       (let ((,var ,result))
         ,@body
         ,var))))

(defmacro awhen (form &body body)
  `(let ((it ,form))
     (when it ,@body)))

(defmacro aif (form then &optional else)
  `(let ((it ,form))
     (if it ,then ,else)))

(defmacro defconstant-eqx (name val &optional (eqx ''equal))
  (let ((new-val-sym (make-symbol "VAL"))
        (old-val-sym (make-symbol "OLD-VAL")))
    `(defconstant ,name (let ((,new-val-sym ,val))
                          (if (boundp ',name)
                              (let ((,old-val-sym (symbol-value ',name)))
                                (if (funcall ,eqx ,old-val-sym ,new-val-sym)
                                    ,old-val-sym
                                    ,new-val-sym))
                              ,new-val-sym)))))

(defmacro define-constant (name val)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,val)))
  
#+boot
(defconstant-eqx lambda-list-keywords
    '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)
  #'equal)

(define-constant unbound-marker (make-symbol "UNBOUND-MARKER"))

#-boot
(define-constant keyword-package (find-package "KEYWORD"))

#+boot
(defun identity (object)
  object)

#+boot
(defun complement (function)
  (lambda (x) (not (funcall function x))))

#+boot
(defun constantly (value)
  (lambda (&rest rest) value))

#+boot
(defmacro typecase (arg &rest clauses)
  (error "unimplemented"))

#+boot
(defmacro ctypecase (arg &rest clauses)
  (error "unimplemented"))

#+boot
(defmacro etypecase (arg &rest clauses)
  (error "unimplemented"))

#+boot
(progn
  (defvar *standard-input*)
  (defvar *standard-output*)
  (defvar *error-output*)
  (defvar *debug-io*)
  (defvar *query-io*)
  (defvar *trace-output*)
  (defvar *terminal-io*))
