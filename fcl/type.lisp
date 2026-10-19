(defun list-sort (list predicate &optional key)
  (setf predicate (coerce-to-function predicate)
        key (when key (coerce-to-function key)))
  (let* ((ret (list nil))
         (ret-end ret))
    (labels ((aux (list)
               (unless (cdr list)
                 (return-from aux list))
               (let ((list1 list) list2)
                 (loop for end = (cddr list) then (cddr end)
                       for middle = list then (cdr middle)
                       when (null end)
                         do (setf list2 (cdr middle))
                            (rplacd middle nil)
                            (return))
                 (setf list1 (aux list1)
                       list2 (aux list2))
                 (setf ret-end ret
                       (cdr ret) nil)
                 (loop while (and list1 list2)
                       do (if (funcall predicate
                                       (apply-key (car list1))
                                       (apply-key (car list2)))
                              (psetf list1 (cdr list1)
                                     (cdr ret-end) list1
                                     ret-end (rplacd list1 nil))
                              (psetf list2 (cdr list2)
                                     (cdr ret-end) list2
                                     ret-end (rplacd list2 nil)))
                       finally (when list1 (rplacd ret-end list1))
                               (when list2 (rplacd ret-end list2)))
                 (cdr ret))))
      (when list
        (aux list)))))

(defun %string-append (&rest strings)
  (loop for string in strings
        with ret = ""
        do (setf ret (%string-append-2 ret string))
        finally (return ret)))

(defun %vector (&rest args)
  (let ((ret (%make-simple-vector (list-length args))))
    (loop for arg in args
          for i from 0
          do (setf (aref ret i) arg))
    ret))

(defun %vector-append (&rest args)
  (let ((ret (%make-simple-vector (loop for arg in args sum (length arg)))))
    (loop for arg in args
          with i = 0
          do (loop for j from i below (incf i (length arg))
                   for k from 0
                   do (setf (aref ret j) (aref arg k))))
    ret))

(defun %make-object-full (class &rest args)
  (let ((ret (%make-object class (list-length args))))
    (loop for arg in args and i from 0
          do (setf (%object-slot ret i) arg))
    ret))

(define-constant unbound-slot (make-symbol "UNBOUND-SLOT"))

;; TODO: convert this to a defstruct, maybe
(defun make-slot-definition (name type initargs initfunction readers writers documentation)
  (list name type initargs initfunction readers writers documentation))
(defun slot-definition-name (slot-definition)
  (first slot-definition))
(defun slot-definition-type (slot-definition)
  (second slot-definition))
(defun slot-definition-initargs (slot-definition)
  (third slot-definition))
(defun slot-definition-initfunction (slot-definition)
  (fourth slot-definition))
(defun slot-definition-readers (slot-definition)
  (fifth slot-definition))
(defun slot-definition-writers (slot-definition)
  (sixth slot-definition))
(defun slot-definition-documentation (slot-definition)
  (seventh slot-definition))

;; TODO: brittle
(defun class-name (obj)
  (declare (type standard-class obj))
  (%object-slot obj 0))
(defun class-direct-superclasses (obj)
  (declare (type standard-class obj))
  (%object-slot obj 1))
(defun class-default-initargs (obj)
  (declare (type standard-class obj))
  (%object-slot obj 2))
(defun class-precedence-list (obj)
  (declare (type standard-class obj))
  (let ((ret (%object-slot obj 3)))
    (if (eql unbound-slot ret)
        (error "the class ~s is not yet finalized" (class-name obj))
        ret)))
(defun (setf class-precedence-list) (new obj)
  (declare (type standard-class obj))
  (setf (%object-slot obj 3) new))
(defun class-documentation (obj)
  (declare (type standard-class obj))
  (%object-slot obj 4))
(defun class-finalized-p (obj)
  (declare (type standard-class obj))
  (%object-slot obj 5))
(defun (setf class-finalized-p) (new obj)
  (declare (type standard-class obj))
  (setf (%object-slot obj 5) new))
(defun class-direct-slots (obj)
  (declare (type standard-class obj))
  (%object-slot obj 6))
(defun class-slots (obj)
  (declare (type standard-class obj))
  (%object-slot obj 7))
(defun (setf class-slots) (new obj)
  (declare (type standard-class obj))
  (setf (%object-slot obj 7) new))

(defmacro defclass (class-name superclass-names slot-specifiers &rest class-options)
  (declare (type proper-symbol class-name))
  (macrolet ((check-duplicate (symbol)
               `(when ,(intern (%string-append (symbol-name symbol) "-P"))
                  (error ,(%string-append ":" (symbol-name symbol) " may only be specified once")))))
    (with-ext-lists ((slot-definitions slot-definitions-end))
      (let (default-initargs default-initargs-p
            (metaclass 'standard-class) metaclass-p
            documentation documentation-p)
        (loop for class-option of-type cons in class-options
              do (ecase (car class-option)
                   ((:default-initargs)
                    (check-duplicate default-initargs)
                    (setq default-initargs (the list (cdr class-option))
                          default-initargs-p t))
                   ((:documentation)
                    (check-duplicate documentation)
                    (destructuring-bind (string) (cdr class-option)
                      (setq documentation (the string string)
                            documentation-p t)))
                   ((:metaclass)
                    (check-duplicate metaclass)
                    (destructuring-bind (class-name) (cdr class-option)
                      (setq metaclass (the proper-symbol class-name)
                            metaclass-p t)))))
        (loop for slot-specifier of-type (or cons symbol) in slot-specifiers
              do (with-ext-lists ((readers readers-end)
                                  (writers writers-end)
                                  (initargs initargs-end))
                   (let ((name (if (consp slot-specifier) (car slot-specifier) slot-specifier))
                         (keys (when (consp slot-specifier) (cdr slot-specifier)))
                         allocation allocation-p
                         (initfunction `(lambda () unbound-slot)) initfunction-p
                         (type t) type-p
                         documentation documentation-p)
                     (unless (evenp (length keys))
                       (error "odd number of keyword parameters in slot-specifier: ~s" slot-specifier))
                     (loop while keys
                           for (key value) = keys
                           do (ecase key
                                ((:reader)
                                 (push-tail value readers-end))
                                ((:writer)
                                 (push-tail value writers-end))
                                ((:accessor)
                                 (push-tail value readers-end)
                                 (push-tail value writers-end))
                                ((:initarg)
                                 (push-tail value initargs-end))
                                ((:allocation)
                                 (check-duplicate allocation)
                                 (setf allocation value allocation-p t)
                                 (error "unimplemented"))
                                ((:initform)
                                 (check-duplicate initfunction)
                                 (setf initfunction `(lambda () ,value)
                                       initfunction-p t))
                                ((:type)
                                 (check-duplicate type)
                                 (setf type value type-p t))
                                ((:documentation)
                                 (check-duplicate documentation)
                                 (setf documentation value documentation-p t)))
                              (setf keys (cddr keys)))
                     (push-tail `(make-slot-definition
                                  ',name
                                  ',type
                                  ',initargs
                                  ,initfunction
                                  ',readers
                                  ',writers
                                  ',documentation)
                                slot-definitions-end))))
        `(%defclass ',class-name ',metaclass ',superclass-names
                    (list ,@slot-definitions)
                    ,@(when default-initargs-p
                        `(:default-initargs ',default-initargs))
                    ,@(when documentation-p
                        `(:documentation ',documentation)))))))

(let ((standard-class (%make-object nil 8)))
  (defun make-standard-class (&key name direct-superclasses direct-slots default-initargs documentation)
    (when (and (not (eql t name)) (null direct-superclasses))
      (setf direct-superclasses (list 'standard-object)))
    (if (eql name 'standard-class)
        (progn
          (setf (%object-class standard-class) standard-class
                (%object-slot standard-class 0) name
                (%object-slot standard-class 1) direct-superclasses
                (%object-slot standard-class 2) default-initargs
                (%object-slot standard-class 3) unbound-slot
                (%object-slot standard-class 4) documentation
                (%object-slot standard-class 5) nil
                (%object-slot standard-class 6) direct-slots
                (%object-slot standard-class 7) nil)
          standard-class)
        (%make-object-full standard-class name direct-superclasses default-initargs nil documentation nil direct-slots nil))))

(defun make-instance (class-name &rest args)
  (let* ((class (let ((ret (if (symbolp class-name) (find-class class-name) class-name)))
                  (finalize-class ret)
                  ret))
         (slots (class-slots class))
         (ret (%make-object class (length slots))))
    (do-plist (key value args)
      (unless (loop for slot across slots
                      thereis (member key (slot-definition-initargs slot)))
        (error "invalid keyword argument to MAKE-INSTANCE: ~s" key)))
    (loop for slot across slots and i from 0
          do (setf (%object-slot ret i)
                   (loop for initarg in (slot-definition-initargs slot)
                         do (let ((value (getf args initarg unbound-marker)))
                              (unless (eql unbound-marker value)
                                (return value)))
                         finally (return (funcall (slot-definition-initfunction slot))))))
    ret))

(defun non-recursive-effective-slots (object)
  (let ((class (%object-class object)))
    (if (eql (find-class 'standard-class) class)
        (%object-slot class 7) ;; TODO: use constant instead?
        (slot-value class 'effective-slots))))

(defun slot-value (object slot-name)
  (declare (type symbol slot-name))
  (let ((slots (non-recursive-effective-slots object)))
    (loop for slot across slots and i from 0
          when (eql slot-name (slot-definition-name slot))
            do (return (let ((ret (%object-slot object i)))
                         (if (eql ret unbound-slot)
                             (error "unbound slot: ~s" slot-name)
                             ret)))
          finally (error "object ~s does not have a slot named ~s" object slot-name))))

(defun slot-boundp (object slot-name)
  (declare (type symbol slot-name))
  (let ((slots (non-recursive-effective-slots object)))
    (loop for slot across slots and i from 0
          when (eql slot-name (slot-definition-name slot))
            do (return (eql unbound-slot (%object-slot object i)))
          finally (error "object ~s does not have a slot named ~s" object slot-name))))

(defun (setf slot-value) (new object slot-name)
  (declare (type symbol slot-name))
  (let ((slots (non-recursive-effective-slots object)))
    (loop for slot across slots and i from 0
          when (eql slot-name (slot-definition-name slot))
            do (setf (%object-slot object i) new)
               (return new)
          finally (error "object ~s does not have a slot named ~s" object slot-name))))

(defvar *early-accessor-functions* nil)

(defun define-accessor-method (class name function writer-p)
  (setf name (if writer-p `(setf ,name) name))
  ;; NOTE: There are multiple classes which may be using the same accessor functions. Normally this
  ;;       could cause problems since the functions use raw %object-slot references, but since this function
  ;;       is only temporary (active within this file only), and classes defined in this file are not oddly
  ;;       structured with multiple-inheritance, it is OK. The real implementation, loaded later in this file
  ;;       uses generic function dispatch and so handles stranger situations correctly.
  (unless (and (fboundp name) (not (member name *early-accessor-functions* :key #'first :test #'equal)))
    (setf (fdefinition name) function))
  (aif (assoc name *early-accessor-functions*)
       (pushnew class (cdr it))
       (push (list name class) *early-accessor-functions*)))

(defun redefine-accessor-methods ()
  (loop for (name . classes) in *early-accessor-functions*
        collect (let* ((function (fdefinition name))
                       (writer-p (consp name))
                       (methods
                         (loop for class in classes
                               collect (make-instance 'standard-method
                                                      :name name
                                                      :lambda-list (if writer-p
                                                                       `(new (object ,(class-name class)))
                                                                       `((object ,(class-name class))))
                                                      :qualifiers nil
                                                      :specializers (if writer-p
                                                                        (list t (class-name class))
                                                                        (list (class-name class)))
                                                      :function function))))
                  (list name
                        (make-instance 'standard-generic-function
                                       :name name
                                       :lambda-list (if writer-p '(new object) '(object))
                                       :method-class 'standard-method 
                                      :method-combination (funcall (info :method-combination 'standard))
                                       :methods (apply #'%vector methods)
                                       :cache (if writer-p
                                                  nil
                                                  (loop for class in classes
                                                        ;; See NOTE above
                                                        with function = (fdefinition name)
                                                        collect (list* (list class) t function))))))
          into functions
        finally (loop for (name function) in functions
                      do (setf (fdefinition name) function)))
  (setf *early-accessor-functions* nil))

(defun %defclass (name metaclass direct-superclasses slot-definitions &rest options)
  (let ((class (let ((args (list* :name name
                                  :direct-superclasses direct-superclasses
                                  :direct-slots slot-definitions
                                  options)))
                 (if (eql 'standard-class metaclass)
                     (apply #'make-standard-class args)
                     (progn
                       (unless (subclassp metaclass 'slot-class)
                         (remf args :direct-slots))
                       (let ((ret (apply #'make-instance metaclass args)))
                         (when (subclassp metaclass 'built-in-class)
                           (ensure-finalize-class ret))
                         ret))))))
    (loop for slot in slot-definitions
          do (let ((name (slot-definition-name slot)))
               (loop for reader in (slot-definition-readers slot)
                     do (define-accessor-method class reader
                          (lambda (object) (error "class is not yet finalized"))
                          nil))
               (loop for writer in (slot-definition-writers slot)
                     do (define-accessor-method class writer
                          (lambda (new object) (error "class is not yet finalized"))
                          t))))
    (setf (info :class name) class)
    class))

(defun find-class (name &optional (errorp t))
  (let ((ret (info :class name)))
    (if (and (null ret) errorp)
        (error "no class named: ~s" name)
        ret)))

(defun (setf find-class) (new name)
  (setf (info :class name) new))

(defun ensure-finalize-class (class)
  (unless (class-finalized-p class)
    (finalize-class class))
  class)

(defun reduce-union (list)
  (cond ((null list) nil)
        ((null (cdr list)) (car list))
        ((null (cddr list)) (union (car list) (cadr list)))
        (t (union (car list) (reduce-union (cdr list))))))

(defun compute-precedence-list (class)
  (labels ((all-superclasses (class)
             (let ((superclasses (class-direct-superclasses class)))
               (cons class (reduce-union (mapcar #'all-superclasses superclasses)))))
           (rc (class)
             (loop for superclass in (class-direct-superclasses class)
                   collect (cons (prog1 class (setf class superclass))
                                 superclass)))
           (remove-links (r c)
             (loop for cons in r
                   unless (eql (car cons) c)
                     collect cons))
           (remove (c s)
             (loop for elem in s
                   unless (eql elem c)
                     collect elem)))
    (let* ((S_C (all-superclasses class))
           (R (loop for class in S_C append (rc class))))
      (with-ext-lists ((sorted sorted-end))
        (loop while S_C
              do (loop for class in S_C
                       when (loop for (D . C) in R never (eql C class))
                         collect class into N
                       finally (cond ((null N) (error "unreachable loop detected"))
                                     ((null (cdr N))
                                      (let ((C (car N)))
                                        (push-tail C sorted-end)
                                        (setf S_C (remove C S_C)
                                              R (remove-links R C))))
                                     (t
                                      (loop for C_j in sorted
                                            with C
                                            do (loop for N_i in N
                                                     do (when (member N_i (class-direct-superclasses C_j))
                                                          (setf C N_i)
                                                          (return)))
                                            finally (unless C (error "precedence list is inconsistent"))
                                                    (push-tail C sorted-end)
                                                    (setf S_C (remove C S_C)
                                                          R (remove-links R C))))))
              finally (return sorted))))))

(defun finalize-stage->= (class n)
  (let ((finalized (class-finalized-p class)))
    (if (eql t finalized)
        t
        (when finalized
          (>= finalized n)))))

(defun finalize-class (class)
  (labels ((finalize-superclasses (class seen)
             (unless (finalize-stage->= class 0)
               (when (member class seen)
                 (error "recursive class definition"))
               (loop for names on (class-direct-superclasses class)
                     for next-class = (find-class (car names))
                     do (setf (car names) next-class)
                        (finalize-superclasses next-class (cons class seen)))
               (setf (class-finalized-p class) 0)))
           (finalize-precedence-lists (class)
             ;; We already check for recursion in finalize-superclasses
             (unless (finalize-stage->= class 1)
               (loop for class in (class-direct-superclasses class)
                     do (finalize-precedence-lists class))
               (setf (class-precedence-list class) (compute-precedence-list class)
                     (class-finalized-p class) 1)))
           (finalize-effective-slots (class)
             (unless (finalize-stage->= class 2)
               (loop for class in (class-direct-superclasses class)
                     do (finalize-effective-slots class))
               (when (member (find-class 'slot-class) (class-precedence-list (%object-class class)))
                 (let ((slots (apply #'%vector
                                     (labels ((aux (classes)
                                                (when classes
                                                  (append (aux (cdr classes))
                                                          (class-direct-slots (car classes))))))
                                       (aux (class-precedence-list class))))))
                   (setf (class-slots class) slots)
                   (loop for slot across slots and i from 0
                         do (loop for reader in (slot-definition-readers slot)
                                  do (define-accessor-method class reader
                                       (let ((saved-i i))
                                         (lambda (object) (%object-slot object saved-i)))
                                       nil))
                            (loop for writer in (slot-definition-writers slot)
                                  do (define-accessor-method class writer
                                       (let ((saved-i i))
                                         (lambda (new object) (setf (%object-slot object saved-i) new)))
                                       t)))))
               (setf (class-finalized-p class) t))))
    (finalize-superclasses class nil)
    (finalize-precedence-lists class)
    (finalize-effective-slots class)
    nil))

(defun coerce-to-class (class)
  (if (symbolp class)
      (find-class class)
      class))

(defun subclassp (class1 class2)
  (setf class1 (coerce-to-class class1)
        class2 (coerce-to-class class2))
  (ensure-finalize-class class1)
  (and (member class2 (class-precedence-list class1)) t))

(defclass t () ()) ;; system class

(defclass slot-object (t) ())
(defclass structure-object (slot-object) ())
(defclass standard-object (slot-object) ())

(defclass class ()
  ((name
    :initarg :name
    :reader class-name)
   (direct-superclasses
    :initarg :direct-superclasses
    :reader class-direct-superclasses)
   (default-initargs
    :initarg :default-initargs
    :initform nil
    :reader class-default-initargs)
   (precedence-list
    :accessor class-precedence-list)
   (documentation
    :initarg :documentation
    :initform nil
    :reader class-documentation)
   (finalized-p
    :initform nil
    :accessor class-finalized-p)))

(defclass slot-class (class)
  ((direct-slots
    :initarg :direct-slots
    :initform nil
    :reader class-direct-slots)
   (effective-slots
    :initarg :slots
    :initform nil
    :accessor class-slots)))

(defclass standard-class (slot-class) ())
(defclass structure-class (slot-class) ())
(defclass built-in-class (class #| system-class |#) ())

(finalize-class (find-class 'standard-class))

(defclass sequence (t) ()) ;; system class
(defclass list (sequence) ()) ;; system class

(defclass symbol (t) ()
  (:metaclass built-in-class))

(defclass null (symbol list) ()
  (:metaclass built-in-class))
(defclass cons (list) ()
  (:metaclass built-in-class))

;; all system class
(defclass number (t) ())
(defclass real (number) ())
(defclass float (real) ())
(defclass rational (real) ())
(defclass integer (rational) ())

(defclass fixnum (integer) ()
  (:metaclass built-in-class))
(defclass single-float (float) ()
  (:metaclass built-in-class))
(defclass double-float (float) ()
  (:metaclass built-in-class))

(defclass character (t) ()
  (:metaclass built-in-class))

;; all system class
(defclass array (t) ())
(defclass vector (array sequence) ())
(defclass bit-vector (vector) ())
(defclass string (vector) ())
(defclass base-string (string) ())
(defclass character-string (string) ())

(defclass simple-array (array) ())
(defclass simple-string (string simple-array) ())

(defclass simple-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-bit-vector (bit-vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-base-string (base-string simple-string) ()
  (:metaclass built-in-class))
(defclass simple-character-string (character-string simple-string) ()
  (:metaclass built-in-class))
(defclass simple-single-float-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-unsigned-byte-8-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-unsigned-byte-16-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-unsigned-byte-32-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-signed-byte-8-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-signed-byte-16-vector (vector simple-array) ()
  (:metaclass built-in-class))
(defclass simple-signed-byte-32-vector (vector simple-array) ()
  (:metaclass built-in-class))

(defclass function (t) ()) ;; system class
(defclass generic-function (function standard-object) ())
(defclass standard-generic-function (generic-function)
  ((name
    :initarg :name
    :reader generic-function-name)
   (argument-precedence-order
    :initarg :argument-precedence-order
    :initform nil
    :accessor generic-function-argument-precedence-order)
   (documentation
    :initarg :documentation
    :accessor generic-function-documentation)
   (lambda-list
    :initarg :lambda-list
    :accessor generic-function-lambda-list)
   (method-class
    :initarg :method-class
    :accessor generic-function-method-class)
   (method-combination
    :initarg :method-combination
    :accessor generic-function-method-combination)
   (methods
    :initarg :methods
    :initform (%vector)
    :accessor generic-function-methods)
   (%generic-function-cache
    :initarg :cache
    :initform nil)))

(defclass method () ())
(defclass standard-method (method)
  ((name
    :initarg :name
    :reader method-name)
   (lambda-list
    :initarg :lambda-list
    :reader method-lambda-list)
   (qualifiers
    :initarg :qualifiers
    :reader method-qualifiers)
   (specializers
    :initarg :specializers
    :reader method-specializers)
   (function
    :initarg :function
    :reader method-function)
   (sticky-p
    :initarg :sticky-p
    :initform nil
    :reader method-sticky-p)))

(defclass method-combination () ())
(defclass standard-method-combination (method-combination)
  ((name
    :initarg :name
    :reader method-combination-name)
   (function
    :initarg :function
    :reader method-combination-function)))

(defun validate-argument-precedence-order (argument-precedence-order reqvars)
  (loop for var in argument-precedence-order
        do (unless (member var reqvars)
             (error "~s is not a required argument of the genereric function ~s" var name))
           (unless (eql 1 (loop for other-var in argument-precedence-order count (eql var other-var)))
             (error "duplicate argument in :ARGUMENT-PRECEDENCE-ORDER ~s" var)))
  argument-precedence-order)

(defun validate-congruent-lambda-lists (generic-function-lambda-list method-lambda-list)
  (destructuring-bind (nreq nopt rest-p key-p keys allow-other-keys-p)
      (nth-value 1 (parse-generic-lambda-list generic-function-lambda-list))
    (destructuring-bind (mnreq mnopt mrest-p mkey-p mkeys mallow-other-keys-p)
        (parse-method-lambda-list method-lambda-list)
      (unless (eql nreq mnreq)
        (error "method lambda-list ~s must have ~s required parameters" method-lambda-list nreq))
      (unless (eql nopt mnopt)
        (error "method lambda-list ~s must have ~s optional parameters" method-lambda-list nopt))
      (unless (if (or rest-p key-p) (or mrest-p mkey-p) t)
        (error "method lambda-list ~s must specify either &key or &rest" method-lambda-list))
      (loop for key in keys
            do (unless (or (member key mkeys)
                           mallow-other-keys-p
                           (and mrest-p (not mkey-p)))
                 (error "method lambda-list ~s must specify key ~s" method-lambda-list key))))))

(defun set-generic-function-lambda-list (func lambda-list &key validate-argument-precedence-order-p)
  (let ((argument-precedence-order (generic-function-argument-precedence-order func)))
    (if argument-precedence-order
        (when validate-argument-precedence-order-p
          (validate-argument-precedence-order
           argument-precedence-order
           (parse-generic-lambda-list lambda-list)))
        (setf (generic-function-argument-precedence-order func)
              (parse-generic-lambda-list lambda-list))))
  (setf (generic-function-lambda-list func) lambda-list))

(defun invalidate-generic-function-cache (func)
  (loop for (types locked . function) in (slot-value func '%generic-function-cache)
        when locked
          do (error "Attempting to invalidate a locked cache entry."))
  (setf (slot-value func '%generic-function-cache) nil))

(defun ensure-generic-function (name &key
                                       (argument-precedence-order nil argument-precedence-order-p)
                                       (documentation nil documentation-p)
                                       environment
                                       (generic-function-class 'standard-generic-function)
                                       (lambda-list nil lambda-list-p)
                                       (method-class 'standard-method)
                                       (method-combination
                                        (funcall (info :method-combination 'standard))))
  (declare (ignore environment))
  (if (fboundp name)
      (let ((func (fdefinition name))
            invalidate-cache-p)
        (unless (typep func 'generic-function)
          (error "~s is already defined as a non-generic function" name))
        (when lambda-list-p
          (when argument-precedence-order-p
            (validate-argument-precedence-order
             argument-precedence-order
             (parse-generic-lambda-list lambda-list)))
          (loop for method across (generic-function-methods func)
                do (validate-congruent-lambda-lists lambda-list (method-lambda-list mnethod)))
          (set-generic-function-lambda-list
           func lambda-list
           :validate-argument-precedence-order-p argument-precedence-order-p)
          (setf invalidate-cache-p t))
        (when argument-precedence-order-p
          (when (slot-boundp func 'lambda-list)
            (validate-argument-precedence-order
             argument-precedence-order
             (parse-generic-lambda-list (generic-function-lambda-list func))))
          (setf (generic-function-argument-precedence-order func) argument-precedence-order
                invalidate-cache-p t))
        (when documentation-p
          (setf documentation (generic-function-documentation func)))
        (unless (eql generic-function-class (class-of func))
          (error "can not not change class"))
        ;; TODO: what to do about method-combination (how to compare equal?)
        (unless (eql method-class (generic-function-class func))
          (setf (generic-function-method-class func) method-class
                invalidate-cache-p t))
        (when invalidate-cache-p
          (invalidate-generic-function-cache func))
        func)
      (setf (fdefinition name)
            (apply #'make-instance generic-function-class
                   :name name
                   :documentation documentation
                   :method-class method-class
                   :method-combination method-combination
                   (append (when argument-precedence-order-p
                             `(:argument-precedence-order ,argument-precedence-order))
                           (when lambda-list-p
                             `(:lambda-list ,lambda-list)))))))

(defun eql-specializer-p (s)
  (and (consp s) (eql (list-length s) 2) (eql 'eql (car s))))

(defun parse-method (name args)
  (let ((qualifiers (loop while (and args (not (listp (car args))))
                          collect (pop args)))
        (lambda-list (if args
                         (pop args)
                         (error "DEFMETHOD for ~s must have a lambda-list" name)))
        (args-sym (make-symbol "ARGS")))
    (unless (function-name-p name)
      (error "~s is not a valid function name" name))
    (multiple-value-bind (bindings specializers) (expand-specialized-lambda-list lambda-list args-sym)
      `(:name ',name
        :lambda-list ',lambda-list
        :qualifiers ',qualifiers
        :specializers (list ,@(loop for (name specializer) in specializers
                                    collect (cond ((eql unbound-marker specializer) t)
                                                  ((symbolp specializer) `',specializer)
                                                  ((eql-specializer-p specializer)
                                                   `(list 'eql ',(cadr specializer)))
                                                  (t (error "invalid specializer ~s" specializer)))))
        :function (lambda (&rest ,args-sym)
                    (block ,(function-name-block-name name)
                      (let* ,bindings
                        (declare (ignorable ,@(loop for (name specializer) in specializers
                                                    unless (eql unbound-marker specializer)
                                                      collect name)))
                        (flet ((call-next-method (&rest args)
                                 (if *next-methods*
                                     (apply (pop *next-methods*)
                                            (if args args ,args-sym))
                                     (error "no-next-method")))
                               (next-method-p ()
                                 (consp *next-methods*)))
                          ,@args))))))))

(defmacro defgeneric (name lambda-list &rest options)
  (unless (function-name-p name)
    (error "~s is not a valid function name" name))
  (with-ext-lists ((methods methods-end)
                   (keys keys-end))
    (loop for (option-name . option-args) in options
          do (ecase option-name
               ((:argument-precedence-order)
                (append-tail (list :argument-precedence-order option-args) keys-end))
               ((declare)
                (error "unimplemented"))
               ((:documentation)
                (destructuring-bind (string) option-args
                  (declare (type string string))
                  (append-tail (list :documentation string) keys-end)))
               ((:method-combination)
                (destructuring-bind (method-combination &rest method-combination-args) option-args
                  (append-tail (list :method-combination `(funcall (info :method-combination ',method-combination)
                                                                   ,@method-combination-args))
                               keys-end)))
               ((:generic-function-class)
                (destructuring-bind (name) option-args
                  (declare (type proper-symbol name))
                  (append-tail (list :generic-function-class name) keys-end)))
               ((:method-class)
                (destructuring-bind (name) option-args
                  (declare (type proper-symbol name))
                  (append-tail (list :method-class name) keys-end)))
               ((:method)
                (push-tail `(list ,@(parse-method name opt-args)) methods-end))))
    `(%defgeneric ',name ',lambda-list (list ,@methods) ,@keys)))

(defun types-and-methods-match-p (types methods)
  (loop for method in methods
        when (loop for specializer in (method-specializers method)
                   and type in types
                   always (if (eql-specializer-p specializer)
                              (eql type (class-of (cadr specializer)))
                              (member (find-class specializer) (class-precedence-list type))))
          do (return t)))

(defun invalidate-generic-function-cache-methods (func methods)
  (setf (slot-value func '%generic-function-cache)
        (loop for entry in (slot-value func '%generic-function-cache)
              for (types locked . function) = entry
              when (not (types-and-methods-match-p types methods))
                collect entry
              else do (when locked
                        (error "Attempting to remove a locked cache entry.")))))

(defun remove-methods-if (func predicate)
  (let* (removed
         (kept (loop for method across (generic-function-methods func)
                     when (not (funcall predicate method))
                       collect method
                     else do (push method removed))))
    (invalidate-generic-function-cache-methods func removed)
    (setf (generic-function-methods func) (apply #'%vector kept))))

(defun add-methods (func methods)
  (invalidate-generic-function-cache-methods func methods)
  (setf (generic-function-methods func) (%vector-append (generic-function-methods func) (apply #'%vector methods))))

(defun %defgeneric (name lambda-list method-args &rest args &key (method-class 'standard-method) &allow-other-keys)
  (let ((func (fboundp name)))
    (when (and func (typep func 'generic-function))
      (remove-methods-if (lambda (method) (not (method-sticky-p method))))))
  (let ((func (apply #'ensure-generic-function name :lambda-list lambda-list args)))
    (add-methods (loop for args in method-args collect (apply #'make-instance method-class args)))
    func))

(defun specializers-agree-p (s1 s2)
  (and (eql (list-length s1) (list-length s2))
       (loop for p1 in s1 and p2 in s2
             always (or (eql p1 p2)
                        (and (eql-specializer-p p1)
                             (eql-specializer-p p2)
                             (eql (cadr p1) (cadr p2)))))))

(defmacro defmethod (name &rest args)
  `(%defmethod ',name ,@(parse-method name args)))

(defun %defmethod (name &rest args &key lambda-list specializers qualifiers &allow-other-keys)
  (let* ((func (fboundp name)))
    (if func
        (progn
          (unless (typep func 'generic-function)
            (error "~s is already defined as a non-generic function" name))
          (if (slot-boundp func 'lambda-list)
              (set-generic-function-lambda-list
               (convert-method-lambda-list-to-generic-lambda-list lambda-list))
              (validate-congruent-lambda-lists (generic-function-lambda-list func) lambda-list))
          (let ((methods (generic-function-methods func))
                (method (apply #'make-instance (generic-function-method-class func) args)))
            (loop for old-method across methods
                  for i from 0
                  do (when (and (specializers-agree-p specializers (method-specializers old-method))
                                (equal qualifiers (method-qualifiers old-method)))
                       (invalidate-generic-function-cache-methods func (list (aref methods i) method))
                       (setf (aref methods i) method)
                       (return))
                  finally (add-methods func (list method)))))
        (setf func (ensure-generic-function
                    name
                    :lambda-list (convert-method-lambda-list-to-generic-lambda-list lambda-list))
              (generic-function-methods func) (%vector (apply #'make-instance 'standard-method args))))
    func))

(defun pattern-p (obj)
  (or (listp obj) (eql '* obj)))

(defun qualifiers-match-p (pattern method)
  (labels ((aux (pattern qualifiers)
             (cond
               ((eql '* pattern) t)
               ((eql 'nil pattern) (eql nil qualifiers))
               ((consp pattern)
                (when (eql (car pattern) (car qualifiers))
                  (aux (cdr pattern) (cdr qualifiers))))
               (t (error "pattern must be a list or the symbol '*")))))
    (aux pattern (method-qualifiers method))))

(defun long-method-combination (name lambda-list method-group-specifiers &rest body)
  (declare (type symbol name))
  (with-ext-lists ((after-effects after-effects-end))
    (let ((method-group-names (mapcar #'first method-group-specifiers))
          (args-sym (make-symbol "ARGS"))
          (applicable-methods (make-symbol "APPLICABLE-METHODS")))
      (loop while (and body (consp (car body)))
            do (case (caar body)
                 (:arguments
                  (error "unimplemented :ARGUMENTS to DEFINE-METHOD-COMBINATION"))
                 (:generic-function
                  (error "unimplemented :GENERIC-FUNCTION to DEFINE-METHOD-COMBINATION"))
                 (t (return))))
      `(%define-method-combination
        ',name
        (lambda (&rest ,args-sym)
          (make-instance
           'standard-method-combination
           :name ',name
           :function (lambda (,applicable-methods)
                       (let* ,(expand-lambda-list lambda-list args-sym)
                         (let ,method-group-names
                           (loop while ,applicable-methods
                                 do (block nil
                                      ,@(loop for (name . args) in method-group-specifiers
                                              collect (progn
                                                        (unless args
                                                          (error "method group specifier requires a pattern or predicate"))
                                                        (let ((form (if (pattern-p (car args))
                                                                        (orify (loop while (and args (pattern-p (car args)))
                                                                                     collect `(qualifiers-match-p
                                                                                               ',(pop args)
                                                                                               (car ,applicable-methods))))
                                                                        `(funcall ',(pop args) (car ,applicable-methods)))))
                                                          (destructuring-bind (&key description (order :most-specific-first) required) args
                                                            (declare (ignore description))
                                                            (when required
                                                              (push-tail `(unless ,name
                                                                            (error "method group ~s requires an applicable method" ',name))
                                                                         after-effects-end))
                                                            (awhen (if (and (symbolp order) (constantp order))
                                                                       (ecase (symbol-value order)
                                                                         (:most-specific-first
                                                                          `(setq ,name (list-nreverse ,name)))
                                                                         (:most-specific-last))
                                                                       `(ecase ,order
                                                                          (:most-specific-first
                                                                           (setq ,name (list-nreverse ,name)))
                                                                          (:most-specific-last)))
                                                              (push-tail it after-effects-end))
                                                            `(when ,form
                                                               (psetq ,applicable-methods (cdr ,applicable-methods)
                                                                      ,name (rplacd ,applicable-methods ,name))
                                                               (return))))))
                                      (error "no applicable method group")))
                           ,@after-effects
                           ,@body)))))))))

(defun short-method-combination (name &key documentation identity-with-one-argument (operator name))
  (declare (ignore documentation))
  (long-method-combination
   name '(&optional (order :most-specific-first))
   `((around (:around))
     (primary (,operator) :order order :required t))
   `(let ((form ,(let ((full-form ``(,',operator ,@(loop for method in primary
                                                 collect `(call-method ,method)))))
                   (if identity-with-one-argument
                       `(if (cdr primary)
                            ,full-form
                            `(call-method ,(first primary)))
                       full-form))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
          form))))

(defun %define-method-combination (name function)
  (setf (info :method-combination name) function))

(defmacro define-method-combination (name &rest args)
  (if (and args (listp (car args)))
      (apply #'long-method-combination name args)
      (apply #'short-method-combination name args)))

(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument t)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)

(define-method-combination standard ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after) :order :most-specific-last))
  (flet ((call-methods (methods)
           (loop for method in methods
                 collect `(call-method ,method))))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods after))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
          form))))

(defun %compute-applicable-methods (func args)
  (let ((argument-precedence-order (generic-function-argument-precedence-order func))
        (reqvars (parse-generic-lambda-list (generic-function-lambda-list func))))
    (list-sort
     (loop for method across (generic-function-methods func)
           when (loop for specializer in (method-specializers method)
                      and arg in args
                      always (typep arg specializer))
             collect method)
     (macrolet ((loop-body ()
                  `(cond ((eql-specializer-p type1)
                          (when (symbolp type2)
                            (return t)))
                         ((eql-specializer-p type2)
                          (return nil))
                         ((not (eql type1 type2))
                          (let ((c1 (find-class type1)) (c2 (find-class type2)))
                            (return
                              (loop for class in (class-precedence-list (class-of arg))
                                    do (cond ((eql c1 class) (return t))
                                             ((eql c2 class) (return nil))))))))))
       (if argument-precedence-order
           (let* ((reorder (loop for var in argument-precedence-order
                                 collect (list-find var reqvars))))
             (lambda (method1 method2)
               (let ((specializers1 (method-specializers method1))
                     (specializers2 (method-specializers method2)))
                 (loop for index in reorder
                       do (let ((type1 (nth index specializers1))
                                (type2 (nth index specializers2))
                                (arg (nth index args)))
                            (loop-body))))))
           (lambda (method1 method2)
             (loop for type1 in (method-specializers method1)
                   and type2 in (method-specializers method2)
                   and arg in args
                   do (loop-body))))))))

(defvar *next-methods*)

(defun coerce-method-to-function (method)
  (if (functionp method)
      method
      (method-function method)))

(defun coerce-methods-to-functions (methods)
  (loop for tail on methods
        for method = (car tail)
        do (unless (functionp method)
             (setf (car tail) (method-function method))))
  methods)

(defun %compute-effective-method-function (form)
  (let ((args (make-symbol "ARGS")))
    `(lambda (&rest ,args)
       (macrolet ((call-method (method &optional next-method-list)
                    `(let ((*next-methods* (coerce-methods-to-functions (list ,@next-method-list))))
                       (apply ,(coerce-method-to-function method) ,',args)))
                  (make-method (form)
                    `(lambda (&rest ,',args)
                       ,form)))
         ,form))))

(defun funcall-generic-function (func &rest args)
  (let ((cache (slot-value func '%generic-function-cache))
        (types (mapcar #'class-of args)))
    (apply
     (aif (assoc types cache :test #'equal)
          (cddr it)
          (let ((nreq (car (nth-value 1 (parse-generic-lambda-list (generic-function-lambda-list func)))))) 
            (unless (or (eql 0 nreq) (nthcdr (- nreq 1) args))
              (error "not enough arguments passed to generic function"))
            (let* ((method-combination (generic-function-method-combination func))
                   (effective-method-form (funcall (method-combination-function method-combination)
                                                   (%compute-applicable-methods func args)))
                   (effective-method-function (%compute-effective-method-function effective-method-form))
                   (ret (eval effective-method-function)))
              (push (list* types nil ret) (slot-value func '%generic-function-cache))
              ret)))
     args)))

(defun %deftype (name function)
  (declare (type symbol name))
  (setf (info :type name) function))

(defmacro deftype (name lambda-list &body body)
  `(%deftype ',name (lambda ,lambda-list ,@body)))

(deftype boolean () `(member t nil))
(deftype proper-symbol () `(and symbol (not null)))

(defun expand-type (type)
  (if (or (symbolp type) (consp type))
      (let ((name (if (symbolp type) type (car type)))
            (args (if (symbolp type) nil (cdr type))))
        (aif (info :type name)
             (expand-type (apply it args))
             type))
      type))

(defun class-of (object)
  (if (%normal-double-float-p object)
      (find-class 'double-float)
      (ecase (%boxed-tag object)
        ((0 4) (find-class 'double-float))
        (5 (ecase (%boxed-sector-32-tag object)
             (0 (find-class 'single-float))
             (1 (find-class 'character))))
        (6 (find-class 'fixnum))
        (7 (ecase (%boxed-ptr-tag object)
             (0 (find-class 'null))
             (1 (find-class 'cons))
             (2 (find-class 'symbol))
             (3 (find-class
                 (ecase (%simple-vector-type-tag object)
                   (0 'simple-vector)
                   (3 'simple-bit-vector)
                   (4 'simple-base-string)
                   (5 'simple-character-string)
                   (6 'simple-single-float-vector)
                   (8 'simple-unsigned-byte-8-vector)
                   (9 'simple-unsigned-byte-16-vector)
                   (10 'simple-unsigned-byte-32-vector)
                   (12 'simple-signed-byte-8-vector)
                   (13 'simple-signed-byte-16-vector)
                   (14 'simple-signed-byte-32-vector))))
             (4 (find-class 'function))
             (7 (%object-class object)))))))

(defun typep (object type)
  (setf type (expand-type type))
  (cond ((symbolp type)
         (loop for class in (class-precedence-list (class-of object))
               thereis (eql type (class-name class))))
        ((consp type)
         (ecase (car type)
           ((and)
            (loop for type in (cdr type)
                  always (typep object type)))
           ((or)
            (loop for type in (cdr type)
                    thereis (typep object type)))
           ((eql)
            (destructuring-bind (obj) (cdr type)
              (eql obj object)))
           ((member)
            (member obj (cdr type)))
           ((satisfies)
            (destructuring-bind (predicate) (cdr type)
              (when (funcall predicate object) t)))
           ((not)
            (destructuring-bind (typespec) (cdr type)
              (not (typep object typespec))))
           ((cons)
            (destructuring-bind (&optional
                                   (car-typespec nil car-typespec-p)
                                   (cdr-typespec nil cdr-typespec-p))
                (cdr type)
              (and (consp object)
                   (or (not car-typespec-p)
                       (typep (car object) car-typespec))
                   (or (not cdr-typespec-p)
                       (typep (cdr object) cdr-typespec)))))))
        (t
         (loop for class in (class-precedence-list (class-of object))
               thereis (eql type class)))))

(defun values-typep (values type)
  (if (and (consp type) (eql 'values (car type)))
      (loop for value-type in (cdr type)
            always (typep (pop values) value-type))
      (typep (car values) type)))

(finalize-class (find-class 'standard-generic-function))
(finalize-class (find-class 'standard-method-combination))
(finalize-class (find-class 'standard-method))

(defun define-accessor-method (class name function writer-p)
  (setf name (if writer-p `(setf ,name) name))
  (%defmethod name
              :lambda-list (if writer-p `(new (object ,(class-name class))) `((object ,(class-name class))))
              :qualifiers nil
              :specializers (if writer-p (list t (class-name class)) (list (class-name class)))
              :function function))

(redefine-accessor-methods)

(defclass structure-descriptor ()
  ((name
    :initarg :name
    :reader structure-descriptor-name)
   (type
    :initarg :type
    :reader structure-descriptor-type)
   (parent
    :initarg :parent
    :reader structure-descriptor-parent)
   (slots
    :initarg :slots
    :reader structure-descriptor-slots)))

(defun error-initfunction ()
  (error "Can not call initfunction of a structure slot."))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (let* ((name (if (consp name-and-options)
                   (car name-and-options)
                   name-and-options))
         (options (when (consp name-and-options)
                    (cdr name-and-options)))
         (conc-name (%string-append (symbol-name name) "-"))
         (make-name (%string-append "MAKE-" (symbol-name name)))
         (copier (%string-append "COPY-" (symbol-name name)))
         (predicate (%string-append (symbol-name name) "-P"))
         constructors boa-constructors structure-type (element-type t)
         named-p initial-offset include-name include-slots print-method
         (object-arg (make-symbol (symbol-name name)))
         effective-type slots)
    (labels ((constructor-form (args)
               (ecase structure-type
                 ((nil) `(%make-object-full (find-class ',name) ,@args))
                 ((list) `(list ,@(when named-p `(',name)) ,@args))
                 ((vector)
                  `(make-array
                    ,(+ (if named-p 1 0) (list-length args))
                    :element-type ,element-type
                    :initial-contents (list ,@(when named-p `(',name)) ,@args)))))
             (parse-slot-descriptions (slot-descriptions)
               (loop for desc in slot-descriptions
                     with seen
                     collect (destructuring-bind (name &optional init-form &key (type t) read-only)
                                 (if (symbolp desc) (list desc) desc)
                               (unless (symbolp name)
                                 (error "slot name must be a symbol: ~s" name))
                               (if (member name seen)
                                   (error "duplicate slot description: ~s" name)
                                   (setf seen (cons name seen)))
                               (list name init-form type read-only)))))
      (loop for option in options
            for option-name = (if (consp option)
                                  (car option)
                                  option)
            for option-args = (when (consp option)
                                (cdr option))
            do (ecase option-name
                 ((:conc-name)
                  (destructuring-bind (&optional conc) option-args
                    (setf conc-name conc)))
                 ((:constructor)
                  (destructuring-bind (&optional (name nil args-p) (lambda-list nil boa-p)) option-args
                    (when args-p
                      (unless (symbolp name)
                        (error ":CONSTRUCTOR must be a symbol"))
                      (when (or (not make-name)
                                (and (null name) (or constructors boa-constructors))
                                (and name (not constructors) (not boa-constructors)))
                        (error "a NIL :CONSTRUCTOR may only be specified if there are no other constructors"))
                      (if name
                          (if boa-p
                              (push (cons name lambda-list) boa-constructors)
                              (push name constructors))
                          (setf make-name nil)))))
                 ((:copier)
                  (destructuring-bind (&optional (copier-name nil arg-p)) option-args
                    (when arg-p
                      (unless (symbolp copier-name)
                        (error "copier name must be a symbol: ~s" copier-name))
                      (setf copier copier-name))))
                 ((:predicate)
                  (destructuring-bind (&optional (predicate-name nil arg-p)) option-args
                    (when arg-p
                      (unless (symbolp predicate-name)
                        (error "predicate name must be a symbol: ~s" predicate-name))
                      (setf predicate predicate-name))))
                 ((:include)
                  (destructuring-bind (included-structure-name &rest slot-descriptions) option-args
                    (unless (symbolp included-structure-name)
                      (error ":INCLUDE must specify a symbol for the included structure"))
                    (setf include-name included-structure-name
                          include-slots (parse-slot-descriptions slot-descriptions))))
                 ((:print-object)
                  (destructuring-bind (&optional (printer-name nil arg-p)) option-args
                    (unless (null print-method)
                      (error "duplicate :PRINT-OBJECT or :PRINT-FUNCTION"))
                    (unless (symbolp printer-name)
                      (error ":PRINT-OBJECT requires a symbol argument but got: ~s" printer-name))
                    (setf print-method `((defmethod print-object ((object ,name) stream)
                                           (funcall #',printer-name object stream))))))
                 ((:print-function)
                  (destructuring-bind (&optional (printer-name nil arg-p)) option-args
                    (unless (null print-method)
                      (error "duplicate :PRINT-OBJECT or :PRINT-FUNCTION"))
                    (unless (symbolp printer-name)
                      (error ":PRINT-FUNCTION requires a symbol argument but got: ~s" printer-name))
                    (setf print-method `((defmethod print-object ((object ,name) stream)
                                           (funcall #',printer-name object stream 0))))))
                 ((:type)
                  (destructuring-bind (type) option-args
                    (cond ((eql 'list type)
                           (setf structure-type 'list))
                          ((eql 'vector type)
                           (setf structure-type 'vector))
                          ((and (consp type) (eql 'vector (car type)) (= 2 (list-length type)))
                           (setf structure-type 'vector
                                 element-type (cadr type)))
                          (t (error "invalid structure type: ~s" type)))))
                 ((:named)
                  (setf named-p t))
                 ((:initial-offset)
                  (destructuring-bind (local-initial-offset) option-args
                    (unless (and (integerp initial-offset) (<= 0 initial-offset))
                      (error ":INITIAL-OFFSET must be a positive integer, got ~s" local-initial-offset))
                    (setf initial-offset loca-initial-offset)))))
      (when (and initial-offset (not structure-type))
        (error ":INITIAL-OFFSET must be specified with a corresponding :TYPE"))
      (if (and structure-type (not named-p))
          (setf effective-type structure-type)
          (setf effective-type name))
      (let* ((type (if (eql structure-type 'vector) `(vector ,element-type) structure-type))
             (include-descriptor (when include-name (info :structure include-name)))
             (parent-slots (when include-descriptor (structure-descriptor-slots include-descriptor))))
        (when include-descriptor
          (unless (equal type (structure-descriptor-type include-descriptor))
            (error "included structures must have the same :TYPE")))
        (setf slots (%vector-append
                     (if parent-slots parent-slots (%vector))
                     (apply #'%vector (make-list (if initial-offset initial-offset 0)))
                     (apply #'%vector (parse-slot-descriptions slot-descriptions))))
        (when parent-slots
          (loop for i from (list-length parent-slots) below (list-length slots)
                for desc = (aref slots i)
                when desc
                  do (loop for parent-desc across parent-slots
                           when parent-desc
                             do (when (eql (car parent-desc) (car desc))
                                  (error "duplicate slot name in parent structure: ~s" (car desc)))))
          (loop for (name _ type read-only) in include-slots
                do (loop for desc across parent-slots
                         when (and desc (eql name (car desc)))
                           do (destructuring-bind (parent-name _ parent-type parent-read-only) desc
                                (declare (ignore _))
                                (when (eql parent-name name)
                                  (unless (and (subtypep type parent-type) (subtypep parent-type type))
                                    (error ":INCLUDE slot-description must have the same type as parent: ~s" name))
                                  (when (and parent-read-only (not read-only))
                                    (error ":INCLUDE slot-description must be read-only: ~s" name))
                                  (return)))
                         finally (error ":INCLUDE slot-description not found in parent structure ~s" name)))
          (loop for desc across parent-slots
                for i from 0
                when desc
                  do (awhen (assoc (car desc) include-slots)
                       (setf (aref slots i) it)))))
      (when (and structure-type (not named-p))
        (unless (or (stringp predicate) (null predicate))
          (error ":PREDICATE can only be specified when the structure is :NAMED"))
        (setf predicate nil))
      (when (and print-method structure-type)
        (error "neither :PRINT-OBJECT or :PRINT-FUNCTION can be used if :TYPE is specified"))
      `(progn
         (%defstruct (make-instance 'structure-descriptor
                                    :name ',name :type ',structure-type
                                    :parent ',include-name :slots ,slots)
                     ,(unless structure-type
                        `(make-instance 'structure-class
                                        :name ',name
                                        :direct-superclasses (list ,(if include-name `',include-name `'structure-object))
                                        :direct-slots ',(loop for (name initform type read-only) across slots
                                                              collect (make-slot-definition name type nil #'error-initfunction nil nil nil)))))
         ,@(loop for name in (append (when make-name (list (intern make-name))) constructors)
                 with keyword-args = (loop for desc across slots
                                           collect (when desc
                                                     (list (intern (symbol-name (car desc)) *keyword-package*)
                                                           (make-symbol (symbol-name (car desc))))))
                 collect `(defun ,name (&key ,@(loop for desc across slots
                                                     for arg in keyword-args
                                                     when desc
                                                       collect (list arg (cadr desc))))
                            (declare ,@(loop for desc across slots
                                             and arg in keyword-args
                                             when desc
                                               collect (list 'type (caddr desc) (cadr arg))))
                            ,(constructor-form (mapcar #'cadr keyword-args))))
         ,@(loop for (name . lambda-list) in boa-constructors
                 with form = (make-symbol "FORM")
                 collect (multiple-value-bind (bindings vars)
                             (expand-boa-lambda-list
                              lambda-list form
                              (loop for desc across slots
                                    when desc
                                      collect (cons (car desc) (cadr desc))))
                           `(defun ,name (&rest ,form)
                              (let* ,bindings
                                ,(constructor-form (loop for desc across slots
                                                         collect (when desc
                                                                   (if (member (car desc) vars) (car desc) (cadr desc)))))))))
         ,@(when predicate
             `((defun ,(if (symbolp predicate) predicate (intern predicate)) (,object-arg)
                 ,(ecase structure-type
                    ((nil)
                     `(and (%object-p ,object-arg)
                           (eql (find-class ',name) (%object-class ,object-arg))))
                    ((list)
                     `(and (consp ,object-arg)
                           (eql ',name (car ,object-arg))))
                    ((vector)
                     `(and (simple-vector-p ,object-arg)
                           (eql ',name (aref ,object-arg 0))))))))
         ,@(when copier
             `((defun ,(if (symbolp copier) copier (intern copier)) (,object-arg)
                 (declare (type ,effective-type ,object-arg))
                 ,(if structure-type
                      `(copy-seq ,object-arg)
                      `(copy-structure ,object-arg)))))
         ,@print-method
         ,@(loop for desc across slots
                 for i from (if (and structure-type named-p) 1 0)
                 when desc
                   append (destructuring-bind (name _ type read-only) desc
                            (declare (ignore _))
                            (let ((getter-name (intern (%string-append conc-name (symbol-name name))))
                                  (new (make-symbol "NEW"))
                                  (form (case structure-type
                                          ((nil) `(%object-slot ,object-arg ,i))
                                          ((list) `(nth ,object-arg ,(+ effective-offset i)))
                                          ((vector) `(aref ,object-arg ,(+ effective-offset i))))))
                              `((defun ,getter-name (,object-arg)
                                  (declare (type ,effective-type ,object-arg))
                                  (the ,type ,form))
                                (defun (setf ,getter-name) (,new ,object-arg)
                                  (declare (type ,type ,new) (type ,effective-type ,object-arg))
                                  ,(if read-only
                                       `(error "slot is read-only")
                                       `(setf ,form ,new)))))))
         ',name))))

(defun %defstruct (descriptor class)
  (setf (info :structure (structure-descriptor-name descriptor)) descriptor)
  (when class
    (setf (info :class (class-name class)) class)))

(defun copy-structure (structure)
  (let* ((length (%object-length structure))
         (ret (%make-object (%object-class structure) length)))
    (loop for i below length
          do (setf (%object-slot ret i) (%object-slot structure i)))
    ret))
