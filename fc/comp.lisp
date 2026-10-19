(defpackage #:comp
  (:use #:cl #:types #:chi-symbols #:asm #:utils #:ir #:assemble #:libc))

(in-package #:comp)

(declaim (optimize (debug 3)))


(defparameter *array-increment* 16)

(defstruct (compile-ctx (:conc-name cctx-))
  (register 0 :type integer)
  (body (make-array *array-increment* :initial-element nil :adjustable t :fill-pointer 0) :type array)
  (scopes nil :type list)
  (scopes-store (make-hash-table :test 'eq) :type hash-table)
  (marked-for-spill (make-hash-table :test 'eq) :type hash-table))

(defstruct indirect-location
  (reg nil :type virtual-register)
  (offset nil :type (or virtual-register constant))
  type)

(defstruct compound-value
  (values nil :type list)
  type)

(defun flatten-compound-value (val)
  (typecase val
    (compound-value
     (loop for val in (compound-value-values val)
           append (flatten-compound-value val)))
    (t (list val))))

(defparameter *compile-ctx* nil)

(defun compile-get-register (type)
  (make-virtual-register
   :number (prog1 (cctx-register *compile-ctx*)
             (incf (cctx-register *compile-ctx*)))
   :type type))

(defun compile-get-registers (type &optional (seen (make-hash-table)))
  (setf type (unalias-ctype type seen))
  (case type
    ((bool u8 i8 u16 i16 u32 i32 u64 i64) (compile-get-register type))
    (t (ecase (first type)
         (ptr (compile-get-register type))
         (array
          (make-compound-value
           :values (loop for i from 0 below (third type)
                         collect (compile-get-registers (second type) seen))
           :type type))
         (tuple
          (make-compound-value
           :values (loop for subtype in (rest type)
                         collect (compile-get-registers subtype seen))
           :type type))
         (struct
          (make-compound-value
           :values (loop for (fname subtype) in (rest type)
                         collect (compile-get-registers subtype seen))
           :type type))
         ;; Handle other compound types
         ))))

(defun compile-lookup-scope (sym)
  (loop for scopes = (cctx-scopes *compile-ctx*) then (cdr scopes)
        while scopes
        for scope = (car scopes)
        do (multiple-value-bind (val found) (gethash sym scope)
             (when found
               (return (values val (case (length scopes) (1 :argument) (t :variable))))))
        finally (error "variable not in scope: ~s" sym)))

(defun compile-replace-scope (sym val)
  (loop for scope in (cctx-scopes *compile-ctx*)
        do (when (nth-value 1 (gethash sym scope))
             (setf (gethash sym scope) val))
        finally (error "variable not in scope: ~s" sym)))

(defun compile-append-insts (insts)
  (loop for inst in insts
        do (compile-append-inst inst)))

(defun compile-append-inst (inst)
  (vector-push-extend inst (cctx-body *compile-ctx*)))

(defmacro constant-eql (val i)
  (let ((val-sym (make-symbol "VAL")))
    `(let ((,val-sym ,val))
       (and (constant-p ,val-sym) (eql ,i (constant-value ,val-sym))))))

(defmacro check-op-types (&rest ops)
  (when ops
    (let ((type-sym (make-symbol "TYPE")))
      `(let ((,type-sym (op-type ,(car ops))))
         ,@(loop for op in (rest ops)
                 collect `(assert (ctype-equal ,type-sym (op-type ,op))))))))

(defun compile-neg (n)
  (if (constant-p n)
      (make-constant :value (- (constant-value n))
                     :type (constant-type n))
      (let ((ret (compile-get-register (op-type n))))
        (compile-append-inst (list 'neg ret n))
        ret)))

(defun compile-add (n m)
  (check-op-types n m)
  (cond
    ((constant-eql n 0) m)
    ((constant-eql m 0) n)
    (t (let ((ret (compile-get-register (op-type n))))
         (compile-append-inst (list 'add ret n m))
         ret))))

(defun compile-sub (n m)
  (check-op-types n m)
  (cond
    ((constant-eql m 0) n)
    ((constant-eql n 0)
     (let ((ret (compile-get-register (op-type n))))
         (compile-append-inst (list 'neg ret m))
         ret))
    (t (let ((ret (compile-get-register (op-type n))))
         (compile-append-inst (list 'sub ret n m))
         ret))))

(defun compile-mul (n m)
  (check-op-types n m)
  (cond
    ((constant-eql n 0)
     (make-constant :value 0 :type (op-type n)))
    ((constant-eql m 0)
     (make-constant :value 0 :type (op-type n)))
    (t (let ((ret (compile-get-register (op-type n))))
         (compile-append-inst (list 'mul ret n m))
         ret))))

(defun compile-div (n m)
  (check-op-types n m)
  (let ((type (op-type n)))
    (cond
      ((constant-eql m 1) n)
      ((constant-eql n 0)
       (make-constant :value 0 :type type))
      ((constant-eql m 0)
       (error "division by zero"))
      ;; Division by zero is undefined behaviour
      (t (let ((ret (compile-get-register type)))
           (compile-append-inst (list (if (signedp type) 'sdiv 'udiv) ret n m))
           ret)))))

(defun compile-madd (n m a)
  (check-op-types n m a)
  (cond
    ((constant-eql n 0) a)
    ((constant-eql m 0) a)
    ((constant-eql n 1)
     (compile-add m a))
    ((constant-eql m 1)
     (compile-add n a))
    ((constant-eql a 0)
     (compile-mul n m))
    (t (let ((ret (compile-get-register (op-type n))))
         (compile-append-inst (list 'madd n m a))
         ret))))

(defun compile-msub (n m a)
  (cond
    ((constant-eql n 0) a)
    ((constant-eql m 0) a)
    ((constant-eql n 1)
     (compile-sub a m))
    ((constant-eql m 1)
     (compile-sub a n))
    ((constant-eql a 0)
     (compile-neg (compile-mul n m)))
    (t (let ((ret (compile-get-register (op-type n))))
         (compile-append-inst (list 'msub n m a))
         ret))))

(defun compile-shift-indirect (loc num size new-type)
  (check-type loc indirect-location)
  (when (numberp num)
    (setf num (make-constant :value num :type 'i32)))
  (when (numberp size)
    (setf size (make-constant :value size :type 'i32)))
  (make-indirect-location
   :reg (indirect-location-reg loc)
   :offset (compile-madd num size (indirect-location-offset loc))
   :type new-type))

(defun compile-sublocs (loc)
  (etypecase loc
    ((or constant virtual-register compound-value) loc)
    (indirect-location
     (with-slots (reg offset type) loc
       (if (types:basic-ctype-p type)
           loc
           (ecase (first type)
             (array
              (make-compound-value
               :values (loop for i from 0 below (third type)
                             with size = (types:size (second type))
                             collect (compile-shift-indirect loc i size (second type)))
               :type type))
             (tuple
              (make-compound-value
               :values (loop for subtype in (rest type)
                             for offset = 0 then (align-extend
                                                  (+ offset (types:size subtype))
                                                  (types:align subtype))
                             collect (compile-shift-indirect loc offset 1 subtype))
               :type type))
             ;; Handle other compound types
             ))))))

(defun compile-load (loc)
  (etypecase loc
    (constant loc)
    (virtual-register
     (let ((new-reg (compile-get-register (virtual-register-type loc))))
       (compile-append-inst `(mov ,new-reg ,loc))
       new-reg))
    (indirect-location
     (let ((sublocs (compile-sublocs loc)))
       (if (compound-value-p sublocs)
           (compile-load sublocs)
           (with-slots (reg offset type) sublocs
             (let ((new-reg (compile-get-register type)))
               (compile-append-inst `(ldr ,new-reg ,reg ,offset))
               new-reg)))))
    (compound-value
     (make-compound-value
      :values (mapcar #'compile-load (compound-value-values loc))
      :type (compound-value-type loc)))))

(defun compile-load-constants (val)
  (etypecase val
    (virtual-register val)
    (constant
     (let ((new-reg (compile-get-register (constant-type val))))
       (compile-append-inst `(mov ,new-reg ,val))
       new-reg))
    (compound-value
     (make-compound-value
      :values (mapcar #'compile-load-constants (compound-value-values val))
      :type (compound-value-type val)))))

(defun compile-store (loc val)
  ;; The val here should already be loaded
  (check-type val (not indirect-location))
  (etypecase loc
    (constant (error "can not store into a constant value"))
    (virtual-register
     (check-type val (or constant virtual-register))
     (compile-append-inst `(mov ,loc ,val)))
    (indirect-location
     (let ((sublocs (compile-sublocs loc)))
       (if (compound-value-p sublocs)
           (compile-store sublocs val)
           (with-slots (reg offset type) sublocs
             (check-type val (or constant virtual-register))
             (assert (ctype-equal type (op-type val)))
             ;; TODO: implement constant val in ir.lisp
             (compile-append-inst `(str ,val ,reg ,offset))))))
    (compound-value
     (check-type val compound-value)
     (assert (eql (length (compound-value-values loc))
                  (length (compound-value-values val))))
     (loop for loc in (compound-value-values loc)
           for val in (compound-value-values val)
           do (compile-store loc val))))
  nil)

(defun compile-expr-load (expr)
  (multiple-value-bind (ret loaded)
      (compile-expr expr)
    (if (not loaded)
        (compile-load ret)
        ret)))

(defun type-from-num-instance (class)
  (ecase class
    (num-i8 'i8)
    (num-u8 'u8)
    (num-i16 'i16)
    (num-u16 'u16)
    (num-i32 'i32)
    (num-u32 'u32)
    (num-i64 'i64)
    (num-u64 'u64)))

(defun ptr-to-indirect (reg)
  (let ((type (virtual-register-type reg)))
    (destructuring-bind (ptr-sym type) type
      (assert (eql 'ptr ptr-sym))
      (make-indirect-location :reg reg
                              :offset (make-constant :value 0 :type 'i32)
                              :type type))))

(defun indirect-load-ptr (loc)
  (check-type loc indirect-location)
  (let ((reg (indirect-location-reg loc))
        (offset (indirect-location-offset loc)))
    (if (constant-eql offset 0)
        (compile-load reg)
        (let ((ret (compile-get-register (list 'ptr (indirect-location-type loc)))))
          (compile-append-inst `(add ,ret ,reg ,offset))
          ret))))

(defun compile-expr (expr)
  (macrolet ((literal (value type)
               `(values (make-constant :value ,value :type ',type) t)))
    (etypecase expr
      (integer (literal expr i32))
      (character
       (assert (char-lessp expr #\Rubout))
       (literal (char-code expr) i8))
      (symbol (case expr
                (true (literal 1 bool))
                (false (literal 0 bool))
                (t (values (compile-lookup-scope expr) nil))))
      (cons (ecase (first expr)
              ((index)
               (destructuring-bind (class val index) (rest expr)
                 (ecase class
                   (types::indexable-ptr
                    (let* ((reg (compile-expr-load val))
                           (type (second (virtual-register-type reg))))
                      (values (compile-shift-indirect
                               (ptr-to-indirect reg)
                               (compile-expr-load index)
                               (size type)
                               type)
                              nil))))))
              ((field)
               (destructuring-bind (kind x field) (rest expr)
                 (declare (ignore kind))
                 (multiple-value-bind (x-val loaded) (compile-expr x)
                   (etypecase x-val
                     (compound-value
                      (values (nth (field-index (compound-value-type x-val) field)
                                   (compound-value-values x-val))
                              loaded))
                     (indirect-location
                      (assert (not loaded))
                      (let* ((type (indirect-location-type x-val))
                             (subtype (second (assoc field (rest type)))))
                        (values (compile-shift-indirect x-val 1 (field-offset type field) subtype)
                                nil)))))))
              ((addr)
               (destructuring-bind (x) (rest expr)
                 (multiple-value-bind (x-val loaded) (compile-expr x)
                   (assert (not loaded))
                   (check-type x-val indirect-location)
                   (values (indirect-load-ptr x-val) t))))
              ((deref)
               (destructuring-bind (x) (rest expr)
                 (values (ptr-to-indirect (compile-expr-load x)) nil)))
              ((coerce)
               (destructuring-bind (class x) (rest expr)
                 (assert (and (consp class) (eql 'coercible-num (first class))))
                 (let* ((x-val (compile-expr-load x))
                        (ret (compile-get-register (type-from-num-instance (third class)))))
                   (compile-append-inst `(coerce ,ret ,x-val))
                   (values ret t))))
              ((= /=)
               (destructuring-bind (class x y) (rest expr)
                 (assert (and (consp class) (eql (first class) 'eq-num)))
                 (let ((x-val (compile-expr-load x))
                       (y-val (compile-expr-load y))
                       (cond (ecase (first expr) (= 'eq) (/= 'ne)))
                       (ret (compile-get-register 'bool)))
                   (compile-append-inst `(,cond ,ret ,x-val ,y-val))
                   (values ret t))))
              ((+ - / * mod)
               (destructuring-bind (class x y) (rest expr)
                 (let ((x-val (compile-expr-load x))
                       (y-val (compile-expr-load y))
                       (ret (compile-get-register (type-from-num-instance class))))
                   (compile-append-insts (ecase (first expr)
                                           (+ `((add ,ret ,x-val ,y-val)))
                                           (- `((sub ,ret ,x-val ,y-val)))
                                           (/ `((udiv ,ret ,x-val ,y-val)))
                                           ;; TODO: implement signed division
                                           (mod
                                            ;; TODO: fix this for other types
                                            (let ((tmp (compile-get-register 'i32)))
                                              `((udiv ,tmp ,x-val ,y-val)
                                                (msub ,ret ,tmp ,y-val ,x-val))))))
                   (values ret t))))
              ((funcall)
               (destructuring-bind (name args type) (rest expr)
                 (check-type (first expr) symbol)
                 (let ((ret (compile-get-registers type)))
                   (compile-append-inst
                    `(call ,name
                           ,(flatten-compound-value ret)
                           ,(loop for arg in args
                                  append (flatten-compound-value (compile-expr-load arg)))))
                   (values ret t))))
              ;; Handle other expression types
              )))))

(defun compile-stmt (stmt)
  (if (consp stmt)
      (case (first stmt)
        ((let)
         (destructuring-bind (name type expr) (rest stmt)
           (declare (ignore type))
           (compile-store (compile-lookup-scope name) (compile-expr-load expr))))
        ((return)
         (compile-append-inst
          `(ret ,(when (eql 2 (length stmt))
                   (flatten-compound-value (compile-expr-load (second stmt)))))))
        ((while)
         (destructuring-bind (condition rest) (rest stmt)
           (let ((while-sym (make-symbol "WHILE"))
                 (end-sym (make-symbol "END")))
             (compile-append-inst while-sym)
             (compile-append-insts `((cbz ,(compile-expr-load condition) ,end-sym)))
             (compile-block rest)
             (compile-append-insts `((b ,while-sym)
                                     ,end-sym)))))
        ((when)
         (destructuring-bind (condition rest) (rest stmt)
           (let ((end-sym (make-symbol "END")))
             (compile-append-insts `((cbz ,(compile-expr-load condition) ,end-sym)))
             (compile-block rest)
             (compile-append-insts `(,end-sym)))))
        ((set)
         (destructuring-bind (lvalue rvalue) (rest stmt)
           (multiple-value-bind (loc loaded) (compile-expr lvalue)
             (assert (not loaded))
             (compile-store loc (compile-expr-load rvalue)))))
        (t (compile-expr-load stmt)))
      (compile-expr-load stmt))
  nil)

(defun compile-block (stmts)
  (let ((scopes (cctx-scopes *compile-ctx*)))
    (setf (cctx-scopes *compile-ctx*) (gethash-strict stmts (cctx-scopes-store *compile-ctx*)))
    (loop for stmt in stmts
          do (compile-stmt stmt))
    (setf (cctx-scopes *compile-ctx*) scopes)))

(define-condition not-compound-register () ())

(defun mark-variable-component (expr)
  (labels ((get-component (expr)
             (typecase expr
               (symbol
                (let ((ret (compile-lookup-scope expr)))
                  (if (compound-value-p ret)
                      ret
                      (error 'not-compound-register))))
               (cons (case (first expr)
                       ((index)
                        (destructuring-bind (class val index) (rest expr)
                          (declare (ignore val index))
                          (ecase class
                            (types::indexable-ptr
                             (error 'not-compound-register)))))
                       ((field)
                        (destructuring-bind (kind x field) (rest expr)
                          (declare (ignore kind))
                          (let ((x-val (get-component x)))
                            (if (compound-value-p x-val)
                                (nth (field-index (compound-value-type x-val) field)
                                     (compound-value-values x-val))
                                (error 'not-compound-register)))))
                       (t (error 'not-compound-register))))
               (t (error 'not-compound-register)))))
    (handler-case
        (setf (gethash (get-component expr) (cctx-marked-for-spill *compile-ctx*)) t)
      (not-compound-register ()))))

(defun spill-marked-components ()
  (let ((marked (cctx-marked-for-spill *compile-ctx*))
        (scopes-searched (make-hash-table :test 'eq)))
    (labels ((remove-matched-subcomponents (component)
               (etypecase component
                 (virtual-register)
                 (compound-value
                  (loop for subcomponent in (compound-value-values component)
                        do (remhash subcomponent marked)
                        do (remove-matched-subcomponents subcomponent)))))
             (spill-component (component store)
               (let* ((type (etypecase component
                              (virtual-register (virtual-register-type component))
                              (compound-value (compound-value-type component))))
                      (reg (compile-get-register (list 'ptr type)))
                      (ret (ptr-to-indirect reg)))
                 (compile-append-inst `(alloca ,reg ,type))
                 (when store
                   (compile-store ret component))
                 ret))
             (spill-matched-subcomponents (component store)
               (etypecase component
                 (virtual-register)
                 (compound-value
                  (loop for subcomponents = (compound-value-values component) then (cdr subcomponents)
                        while subcomponents
                        for subcomponent = (car subcomponents)
                        when (gethash subcomponent marked)
                          do (setf (car subcomponents) (spill-component subcomponent store))
                        do (spill-matched-subcomponents subcomponent store))))))
      (loop for component in (loop for key being the hash-keys of marked collect key)
            do (remove-matched-subcomponents component))
      (loop for scopes being the hash-values of (cctx-scopes-store *compile-ctx*)
            with arguments-scope = (first (cctx-scopes *compile-ctx*))
            do (loop for scope in scopes
                     do (when (not (gethash scope scopes-searched))
                          (setf (gethash scope scopes-searched) t)
                          (loop for component being the hash-values of scope
                                do (if (gethash component marked)
                                       (setf (gethash component scope)
                                             (spill-component component (eq arguments-scope scope)))
                                       (spill-matched-subcomponents component (eq arguments-scope scope))))))))))

(defun analyse-expr (expr)
  (etypecase expr
    ((or integer character symbol))
    (cons (ecase (first expr)
            (addr
             (mark-variable-component (second expr))
             (analyse-expr (second expr)))
            (deref
             (analyse-expr (second expr)))
            (index
             (analyse-expr (third expr))
             (analyse-expr (fourth expr)))
            ((field coerce)
             (analyse-expr (third expr)))
            ((= /= + - / * mod)
             (analyse-expr (third expr))
             (analyse-expr (fourth expr)))
            (funcall
             (analyse-expr (second expr))
             (loop for arg in (third expr)
                   do (analyse-expr arg)))
            ;; Handle other expression types
            ))))

(defun analyse-stmt (stmt)
  (if (consp stmt)
      (case (first stmt)
        ((let)
         (analyse-expr (fourth stmt))
         (setf (gethash-strict (second stmt) (first (cctx-scopes *compile-ctx*)))
               (compile-get-registers (third stmt))))
        ((return)
         (when (eql 2 (length stmt))
           (analyse-expr (second stmt))))
        ((while when)
         (analyse-expr (second stmt))
         (analyse-block (third stmt)))
        ((set)
         (analyse-expr (second stmt))
         (analyse-expr (third stmt)))
        (t (analyse-expr stmt)))
      (analyse-expr stmt))
  nil)

(defun analyse-block (stmts)
  (push (make-hash-table) (cctx-scopes *compile-ctx*))
  (setf (gethash stmts (cctx-scopes-store *compile-ctx*)) (cctx-scopes *compile-ctx*))
  (loop for stmt in stmts
        do (analyse-stmt stmt))
  (pop (cctx-scopes *compile-ctx*)))

(defun compile-func (args body)
  (let* ((*compile-ctx* (make-compile-ctx))
         (scope (loop for (name type) in args
                      with tbl = (make-hash-table)
                      do (setf (gethash-strict name tbl) (compile-get-registers type))
                      finally (return tbl))))
    (push scope (cctx-scopes *compile-ctx*))
    (compile-append-inst `(args ,(loop for name being the hash-keys of scope
                                         using (hash-value arg)
                                       append (flatten-compound-value arg))))
    (analyse-block body)
    (spill-marked-components)
    (compile-block body)
    (let* ((insts (cctx-body *compile-ctx*))
           (end (aref insts (1- (length insts)))))
      (unless (and (consp end) (eql 'ret (first end)))
        (compile-append-inst `(ret)))
      insts)))

(defun compile-define-cfunction-typed (decl)
  (destructuring-bind (name args . body) (rest decl)
    (destructuring-bind (name &key extern) (if (consp name) name (list name))
      (let* ((insts (codegen (compile-func args body)))
             (length (loop for inst across insts
                           count (and (consp inst) (instruction-p (first inst))))))
        (values (concatenate 'list
                             `(asm
                               (.align 4)
                               ,@(when extern
                                   `((.global ,name)))
                               (.type ,name :function)
                               ,name)
                             insts
                             `((.size ,name ,length)))
                'base:add-decl)))))

(defun compile-define-cfunction-extern (decl)
  (destructuring-bind (name args ret) (rest decl)
    (declare (ignore args ret))
    (destructuring-bind (name) (if (consp name) name (list name))
      (values `(asm (.global ,(second decl))
                    (.type ,name :function))
              'base:add-decl))))

(defparameter *compiler-module*
  (base:make-compiler-module
   :phase-decl-functions (list (list 'evm:compile-phase 'define-cfunction-typed 'compile-define-cfunction-typed)
                               (list 'evm:compile-phase 'define-cfunction-extern 'compile-define-cfunction-extern))))

