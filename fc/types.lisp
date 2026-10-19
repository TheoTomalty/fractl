(defpackage #:types
  (:use #:cl #:utils #:chi-symbols #:wam)
  (:export #:ctype-equal #:basic-ctype-p #:integer-ctype-p
           #:define-cfunction-typed #:*new-types*
           #:unalias-ctype #:field-offset #:field-index
           #:widep #:signedp #:size #:align
           #:peano-integer #:integer-peano))


(in-package #:types)

(deftype void () nil)
(deftype bool () 'boolean)
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype u64 () '(unsigned-byte 64))
(deftype i8 () '(signed-byte 8))
(deftype i16 () '(signed-byte 16))
(deftype i32 () '(signed-byte 32))
(deftype i64 () '(signed-byte 64))

(defparameter *new-types* nil)

(defun ctype-equal (t1 t2)
  (equalp t1 t2))

(defun basic-ctype-p (type)
  (or (and (consp type)
           (eql 'ptr (first type)))
      (case type
        ((u8 u16 u32 u64 i8 i16 i32 i64 bool)
         t))))

(defun integer-ctype-p (type)
  (case type
    ((u8 u16 u32 u64 i8 i16 i32 i64) t)))

(defun widep (type)
  (or (and (consp type)
           (eql 'ptr (first type)))
      (ecase type
        ((bool i8 u8 i16 u16 i32 u32 ptr) nil)
        ((u64 i64) t))))

(defun signedp (type)
  (and (not (and (consp type)
                 (eql 'ptr (first type))))
       (ecase type
         ((i8 i16 i32 i64) t)
         ((bool u8 u16 u32 u64) nil))))

(defun unalias-ctype (type &optional (seen (make-hash-table)))
  (when (null *new-types*)
    (return-from unalias-ctype type))
  (when (nth-value 1 (gethash type seen))
    (error "type recursion detected for ~s" type))
  (multiple-value-bind (new-type found) (gethash type *new-types*)
    (if found
        (progn
          (setf (gethash type seen) t)
          (unalias-ctype new-type seen))
        type)))

(defmacro recursion-protect (fname type seen)
  `(when (and (symbolp ,type) *new-types*)
     (multiple-value-bind (type-1 found) (gethash ,type *new-types*)
       (when found
         (let ((saved (gethash ,type seen)))
           (typecase saved
             ((eql t) (error "type recursion detected for ~s" ,type))
             (integer (return-from ,fname saved))))
         (setf (gethash ,type ,seen) t)
         (return-from ,fname (let ((ret (size type-1 ,seen)))
                               (setf (gethash ,type ,seen) ret)
                               ret))))))

(defun size (type &optional (seen (make-hash-table)))
  (recursion-protect size type seen)
  (case type
    ((void) 0)
    ((u8 i8 bool) 1)
    ((u16 i16) 2)
    ((u32 i32) 4)
    ((u64 i64) 8)
    (t
     (ecase (first type)
       (ptr 8)
       (array
        (* (size (second type) seen)
                 (third type)))
       (tuple
        (loop for type in (rest type)
              with offset = 0
              do (setf offset (+ (align-extend offset (align type)) (size type seen)))
              finally (return offset)))
       (struct
        (loop for (field type) in (rest type)
              with offset = 0
              do (setf offset (+ (align-extend offset (align type)) (size type seen)))
              finally (return offset)))
       ;; Handle other compound types
       ))))

(defun align (type &optional (seen (make-hash-table)))
  (recursion-protect align type seen)
  (case type
    ((void) 0)
    ((u8 i8 bool) 1)
    ((u16 i16) 2)
    ((u32 i32) 4)
    ((u64 i64) 8)
    (t (ecase (first type)
         (ptr 8)
         (array (align (second type) seen))
         (tuple (loop for type in (rest type) maximize (align type seen)))
         (struct (loop for (field type) in (rest type) maximize (align type seen)))
         ;; Handle other compound types
         ))))

(defun field-offset (type field)
  (loop for (fname type) in (ecase (first type)
                              (struct (rest type))
                              (tuple (loop for type in (rest type)
                                           for i from 0
                                           collect (list i type))))
        with offset = 0
        do (progn
             (setf offset (align-extend offset (align type)))
             (when (eql fname field)
               (return offset))
             (incf offset (size type)))
        finally (error "field ~s not in type ~s" field type)))

(defun field-index (type field)
  (ecase (first type)
    (struct
     (position-if (lambda (pair) (eql field (first pair))) (rest type)))
    (tuple (peano-integer field))))

(defparameter *type-logic* nil)
(defun make-type-logic (&aux (ret (make-dynamic-array)))
  (loop for rule in `((map #?x #?(#?(#?x . #?y) . #?l) #?y)
                      (:- (map #?x #?(#?h . #?l) #?y)
                          (map #?x #?l #?y))

                      (nth zero #?(#?x . #?l) #?x)
                      (:- (nth (succ #?n) #?(#?x . #?l) #?y)
                          (nth #?n #?l #?y))

                      (peano zero)
                      (peano (succ #?n))

                      (types #?ctx #?() #?())
                      (:- (types #?ctx #?(#?x . #?xs) #?(#?t . #?ts))
                          (type #?ctx #?x #?t)
                          (types #?ctx #?xs #?ts))

                      (:- (typeclass #?ctx #?x #?t)
                          (map #?x #?ctx #?t))

                      (:- (type #?ctx #?x #?t)
                          (wam::var #?x)
                          wam::!
                          (typeclass #?ctx #?x #?t))
                      (:- (type #?ctx #?x i32)
                          (wam::integer #?x))
                      (type #?ctx true bool)
                      (type #?ctx false bool)
                      (:- (type #?ctx #?x i8)
                          (wam::character #?x))
                      (:- (type #?ctx #?x #?t)
                          (wam::symbol #?x)
                          (map #?x #?ctx #?t))
                      (:- (type #?ctx (funcall #?x #?a #?t) #?t)
                          (type #?ctx #?x (fn #?ts #?t))
                          (types #?ctx #?a #?ts))
                      (:- (type #?ctx (set #?x #?y) void)
                          (type #?ctx #?x #?t)
                          (type #?ctx #?y #?t))
                      (:- (type #?ctx (field struct #?x #?f) #?t)
                          (wam::symbol #?f)
                          (type #?ctx #?x #?s)
                          (type-resolve #?s (struct #?fs))
                          (map #?f #?fs #?t))
                      (:- (type #?ctx (field tuple #?x #?f) #?t)
                          (peano #?f)
                          (type #?ctx #?x #?s)
                          (type-resolve #?s (tuple #?fs))
                          (nth #?f #?fs #?t))
                      (:- (type #?ctx (addr #?x) (ptr #?t))
                          (type #?ctx #?x #?t))
                      (:- (type #?ctx (deref #?x) #?t)
                          (type #?ctx #?x (ptr #?t)))

                      (:- (type-resolve #?t #?t)
                          (wam::structure #?t))
                      (:- (type-resolve #?t #?u)
                          (wam::symbol #?t)
                          (new-type #?t #?u))
                      (:- (type-resolve #?t #?u)
                          (wam::symbol #?t)
                          (new-type #?t #?v)
                          (type-resolve #?v #?u))

                      (type-block #?ctx #?ret #?())
                      (:- (type-block #?ctx #?ret #?((let #?v #?t #?x) . #?l))
                          (wam::symbol #?v)
                          (type #?ctx #?x #?t)
                          (type-block #?(#?(#?v . #?t) . #?ctx) #?ret #?l))
                      (:- (type-block #?ctx #?ret #?((return #?x) . #?l))
                          (type #?ctx #?x #?ret)
                          (type-block #?ctx #?ret #?l))
                      (:- (type-block #?ctx #?ret #?((while #?c #?b) . #?l))
                          (type #?ctx #?c bool)
                          (type-block #?ctx #?ret #?b)
                          (type-block #?ctx #?ret #?l))
                      (:- (type-block #?ctx #?ret #?((when #?c #?b) . #?l))
                          (type #?ctx #?c bool)
                          (type-block #?ctx #?ret #?b)
                          (type-block #?ctx #?ret #?l))
                      (:- (type-block #?ctx #?ret #?(#?x . #?l))
                          (type #?ctx #?x #?t)
                          (type-block #?ctx #?ret #?l)))
        do (vector-push-extend rule ret))
  ret)

(defun push-type-logic (rule &rest rules)
  (vector-push-extend rule *type-logic*)
  (loop for rule in rules
        do (vector-push-extend rule *type-logic*)))

(defun wam-mapcar (func list)
  (etypecase list
    (null #?())
    (cons (wam-cons (funcall func (car list)) (wam-mapcar func (cdr list))))))

(defun wam-list (&rest args)
  (etypecase args
    (null #?())
    (cons (wam-cons (car args) (apply #'wam-list (cdr args))))))

(defun wam-list* (arg &rest args)
  (if (null args)
      arg
      (wam-cons arg (apply #'wam-list* (car args) (cdr args)))))

(defun integer-peano (n)
  (declare (type (integer 0) n))
  (if (eql 0 n)
      'zero
      `(succ ,(integer-peano (1- n)))))

(defun peano-integer (p)
  (etypecase p
    ((eql zero) 0)
    (cons
     (assert (eql 'succ (first p)))
     (1+ (peano-integer (second p))))))

(defun expand-expr (expr)
  (if (consp expr)
      (case (first expr)
        ((set addr deref)
         (list* (first expr) (mapcar #'expand-expr (rest expr))))
        ((field)
         (list 'field (wam::make-var :name (gensym))
               (expand-expr (second expr))
               (etypecase (third expr)
                 (symbol (third expr))
                 ((integer 0) (integer-peano (third expr))))))
        ((= /= + - * / mod index coerce)
         (list* (first expr) (wam::make-var :name (gensym)) (mapcar #'expand-expr (rest expr))))
        (t (list 'funcall (first expr)
                 (wam-mapcar #'expand-expr (rest expr))
                 (wam::make-var :name (gensym)))))
      expr))

(defun expand-stmt (stmt)
  (if (consp stmt)
      (case (first stmt)
        ((let)
         `(let ,(second stmt) ,(wam::make-var :name (gensym)) ,(expand-expr (third stmt))))
        ((while)
         `(while ,(expand-expr (second stmt)) ,(wam-mapcar #'expand-stmt (cddr stmt))))
        ((when)
         `(when ,(expand-expr (second stmt)) ,(wam-mapcar #'expand-stmt (cddr stmt))))
        ((return)
         (ecase (length stmt)
           (1 `(return))
           (2 `(return ,(expand-expr (second stmt))))))
        (t (expand-expr stmt)))
      (expand-expr stmt)))

(defun uncons-lists (l)
  (typecase l
    (cons (cons (uncons-lists (car l)) (uncons-lists (cdr l))))
    (wam-cons (cons (uncons-lists (wam-car l)) (uncons-lists (wam-cdr l))))
    (wam-null nil)
    (t l)))

(defun function-type-rule (name arg-types ret &key builtin)
  (if builtin
      (let* ((arg-vars (loop for type in arg-types
                             for i from 1
                             collect (wam::make-var :name (make-symbol (format nil "ARG~d" i))))))
        `(:- (type #?ctx (,name ,@arg-vars) ,ret)
             ;; arguments are reversed so that typeclasses are evaluated after arguments
             ;; TODO: make order explicit by searching for arguments named "instance"?
             ,@(reverse (loop for arg-type in arg-types
                              for arg-var in arg-vars
                              collect `(type #?ctx ,arg-var ,arg-type)))))
      `(type #?ctx ,name (fn ,(apply #'wam-list arg-types) ,ret))))

(defun pre-type-define-cfunction (decl)
  (destructuring-bind (name args ret . body) (rest decl)
    (declare (ignore body))
    (push-type-logic (function-type-rule name (mapcar #'second args) ret))))

(defun pre-type-define-trait (decl)
  (destructuring-bind (name type-args . methods) (rest decl)
    (loop for method in methods
          with builtin = (case (first decl) (define-trait-builtin t))
          do (destructuring-bind (method-name args ret) method
               (push-type-logic
                (function-type-rule method-name
                                    (list* (list* name type-args) args)
                                    ret :builtin builtin))))))

(defun pre-type-define-impl (decl)
  (destructuring-bind (name args type) (rest decl)
    (let ((arg-vars (loop for arg in args collect (wam::make-var :name (gensym)))))
      (push-type-logic
       (let ((head `(typeclass #?ctx ,(if arg-vars (list* name arg-vars) name) ,type)))
         (if arg-vars
             `(:- ,head
                  ,@(loop for arg in args
                          for name in arg-vars
                          collect `(typeclass #?ctx ,name ,arg)))
             head))))))

(defun expand-type (type)
  (etypecase type
    ((or symbol var) type)
    (cons
     (ecase (first type)
       (ptr `(ptr ,(expand-type (second type))))
       (struct `(struct ,(apply #'wam-list (loop for (fname ftype) in (rest type)
                                                 collect (wam-cons fname (expand-type ftype))))))
       (tuple `(tuple ,(wam-mapcar #'expand-type (rest type))))))))

(defun pre-type-define-ctype (decl)
  (destructuring-bind (name type) (rest decl)
    (declare (type symbol name))
    (setf (gethash name *new-types*) type)
    (push-type-logic
     `(new-type ,name ,(expand-type type)))))

(defun type-define-cfunction (decl)
  (destructuring-bind (name args ret-type . body) (rest decl)
    (let* ((stmts (wam-mapcar #'expand-stmt body))
           (ret (wam::exec *type-logic*
                           ;; replace type vars with symbols?
                           `((type-block ,(wam-mapcar (lambda (pair) (wam-cons (first pair) (expand-type (second pair)))) args)
                                         ,(expand-type ret-type)
                                         ,stmts)
                             (wam::halt ,stmts)))))
      (if ret
          (values `(define-cfunction-typed ,name ,args ,@(uncons-lists ret))
                  'base:add-decl)
          (error "type error")))))

(defparameter *compiler-module*
  (base:make-compiler-module
   :phase-decl-functions (list (list 'evm:pre-type-phase 'define-cfunction 'pre-type-define-cfunction)
                               (list 'evm:pre-type-phase 'define-cfunction-extern 'pre-type-define-cfunction)
                               (list 'evm:pre-type-phase 'define-impl 'pre-type-define-impl)
                               (list 'evm:pre-type-phase 'define-impl-builtin 'pre-type-define-impl)
                               (list 'evm:pre-type-phase 'define-trait 'pre-type-define-trait)
                               (list 'evm:pre-type-phase 'define-trait-builtin 'pre-type-define-trait)
                               (list 'evm:pre-type-phase 'define-ctype 'pre-type-define-ctype)
                               (list 'evm:type-phase 'define-cfunction 'type-define-cfunction))
   :phase-functions (list (cons 'evm:pre-type-phase (lambda ()
                                                      (setf *type-logic*
                                                            (wam::link-program
                                                             (wam::compile-program
                                                              *type-logic*))))))
   :variables (list (cons '*type-logic* 'make-type-logic)
                    (cons '*new-types* 'make-hash-table))))

