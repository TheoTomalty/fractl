(defpackage #:wam
  (:use #:cl)
  (:export #:var #:make-var #:var-name #:wam-cons #:wam-cons-p
           #:wam-car #:wam-cdr #:wam-null))

(in-package #:wam)

(defstruct var
  (name nil))

(defmethod print-object ((var var) stream)
  (write-string "#?" stream)
  (print-object (var-name var) stream))

(defmethod make-load-form ((var var) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots var))

(defstruct (wam-cons (:conc-name wam-))
  (car nil)
  (cdr nil))

(defun wam-cons (car cdr)
  (make-wam-cons :car car :cdr cdr))

(defstruct wam-null)

(defparameter *wam-nil* (make-wam-null))

(defmethod make-load-form ((var var) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots var))

(defmethod make-load-form ((cons wam-cons) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots cons))

(defmethod make-load-form ((null wam-null) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots null))

(defun skip-whitespace (stream)
  (let ((char (read-char stream nil nil t)))
    (case char
      ((#\Tab #\Newline #\  #\Return)
       (skip-whitespace stream))
      (t (unread-char char stream)))))

(defun read-wam-list-end (stream)
  (skip-whitespace stream)
  (ecase (read-char stream t nil t)
    (#\) *wam-nil*)
    (#\.
     (prog1
         (read stream t nil t)
       (skip-whitespace stream)
       (unless (eql #\) (read-char stream t nil t))
         (sb-int:simple-reader-error stream "expected end of list after dot and expression"))))))

(defun read-wam-list-elt (stream)
  (make-wam-cons :car (prog1 (read stream t nil t)
                        (skip-whitespace stream))
                 :cdr (case (peek-char nil stream)
                        ((#\) #\.) (read-wam-list-end stream))
                        (t (read-wam-list-elt stream)))))

(defun wam-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((char (read-char stream t nil t)))
    (case char
      (#\(
       (let ((char (read-char stream t nil t)))
         (if (eql #\) char)
             *wam-nil*
             (progn
               (unread-char char stream)
               (read-wam-list-elt stream)))))
      (t
       (unread-char char stream)
       (let ((symbol (read stream t nil t)))
         (unless (typep symbol 'symbol)
           (sb-int:simple-reader-error stream "expected symbol after #?"))
         (make-var :name symbol))))))

(set-dispatch-macro-character #\# #\? 'wam-reader)

(defmethod print-object ((val wam-cons) stream)
  (write-string "#?(" stream)
  (loop with car = (wam-car val)
        with cdr = (wam-cdr val)
        do (print-object car stream)
        do (typecase cdr
             (wam-null (return))
             (wam-cons
              (write-string " " stream)
              (setf car (wam-car cdr)
                    cdr (wam-cdr cdr)))
             (t
              (write-string " . " stream)
              (print-object cdr stream)
              (return))))
  (write-string ")" stream))

(defmethod print-object ((val wam-null) stream)
  (declare (ignore val))
  (write-string "#?()" stream))

(defstruct (local-var (:include var)))
(defstruct (global-var (:include var)))


(defmethod print-object ((var local-var) stream)
  (format stream "X~a" (var-name var)))
(defmethod print-object ((var global-var) stream)
  (format stream "Y~a" (var-name var)))

(defmacro var (name)
  `(make-var :name ',name))

(defparameter *empty-cell* (make-symbol "VOID"))

(defun vector-extend (vector new-length &optional value)
  (loop for i from (length vector) below new-length
        do (vector-push-extend value vector)))

(defun arguments (term)
  (etypecase term
    (cons (cdr term))
    (symbol nil)))

(defun arity (term)
  (length (arguments term)))

(defun flatten-term (term &key (min-reserved 0) (bindings (make-hash-table :test #'equalp)))
  (let* ((argnum (max min-reserved (arity term)))
         (terms (make-array (max argnum 5)
                            :adjustable t
                            :initial-element *empty-cell*
                            :fill-pointer argnum))
         (allocation argnum))
    (loop for key being the hash-keys of bindings
            using (hash-value val)
          do (when (local-var-p val)
               (vector-extend terms (1+ (local-var-name val)) *empty-cell*)
               (setf (aref terms (local-var-name val)) key)))
    (labels ((aux (term &optional index)
               (etypecase term
                 ((or symbol integer character wam-null)
                  (if index
                      (add-term term index)
                      term))
                 (var
                  (unless (gethash term bindings)
                    (setf (gethash term bindings)
                          (add-term term nil)))
                  (when index
                    (setf (aref terms index) (gethash term bindings)))
                  (gethash term bindings))
                 (cons
                  (unless index
                    (setf index (allocate-term)))
                  (add-term (cons (first term) (mapcar #'aux (rest term))) index))
                 (wam-cons
                  (unless index
                    (setf index (allocate-term)))
                  (add-term (make-wam-cons :car (aux (wam-car term)) :cdr (aux (wam-cdr term))) index))))
             (allocate-term ()
               (loop while (< allocation (length terms))
                     do (progn
                          (incf allocation)
                          (when (eql *empty-cell* (aref terms (1- allocation)))
                            (return-from allocate-term (1- allocation)))))
               (vector-push-extend *empty-cell* terms)
               (incf allocation)
               (1- (length terms)))
             (add-term (term index)
               (unless index
                 (setf index (allocate-term)))
               (setf (aref terms index) term)
               (make-local-var :name index)))
      (loop for arg in (arguments term)
            for i from 0
            do (aux arg i))
      terms)))

(defun flatten-query (query &rest rest)
  (let ((terms (apply #'flatten-term query rest)))
    (let (ordered-terms)
      (labels ((aux (i)
                 (let ((term (aref terms i)))
                   (setf (aref terms i) *empty-cell*)
                   (unless (eql term *empty-cell*)
                     (etypecase term
                       ((or local-var global-var wam-null symbol integer character)
                        (push (list (make-local-var :name i) term) ordered-terms))
                       (var)
                       (wam-cons
                        (when (local-var-p (wam-car term))
                          (aux (var-name (wam-car term))))
                        (when (local-var-p (wam-cdr term))
                          (aux (var-name (wam-cdr term))))
                        (push (list (make-local-var :name i) term)
                              ordered-terms))
                       (cons
                        (loop for subterm in (rest term)
                              do (when (local-var-p subterm)
                                   (aux (var-name subterm))))
                        (push (list* (make-local-var :name i) `(/ ,(first term) ,(length (rest term))) (rest term))
                              ordered-terms)))))))
        (loop for i from 0 below (arity query)
              do (aux i))
        (reverse ordered-terms)))))

(defun flatten-program (program &rest rest)
  (let ((terms (apply #'flatten-term program rest)))
    (loop for term across terms
          for i from 0
          unless (eql *empty-cell* term)
            append (etypecase term
                     ((or local-var global-var wam-cons wam-null symbol integer character)
                      `((,(make-local-var :name i) ,term)))
                     (var)
                     (cons
                      `((,(make-local-var :name i) (/ ,(first term) ,(length (rest term))) ,@(rest term))))))))

(defun compile-term (bindings &key query-p (seen (make-hash-table :test #'equalp)))
  (loop for (i functor . rest) in bindings
        do (setf (gethash i seen) t)
        append (labels ((compile-argument (arg)
                          (etypecase arg
                            ((or symbol integer character wam-null)
                             `(,(if query-p 'set-constant 'unify-constant) ,arg))
                            (var
                             (if (gethash arg seen)
                                 `(,(if query-p 'set-value 'unify-value) ,arg)
                                 (progn
                                   (setf (gethash arg seen) t)
                                   `(,(if query-p 'set-variable 'unify-variable) ,arg)))))))
                 (etypecase functor
                   (var
                    (prog1
                        (if (gethash functor seen)
                            `((,(if query-p 'put-value 'get-value)
                               ,functor ,i))
                            `((,(if query-p 'put-variable 'get-variable)
                               ,functor ,i)))
                      (setf (gethash functor seen) t)))
                   ((or symbol integer character wam-null)
                    `((,(if query-p 'put-constant 'get-constant) ,functor ,i)))
                   (wam-cons
                    (list (list (if query-p 'put-list 'get-list) i)
                          (compile-argument (wam-car functor))
                          (compile-argument (wam-cdr functor))))
                   (cons
                    (list* `(,(if query-p 'put-structure 'get-structure)
                             ,functor ,i)
                           (loop for arg in rest
                                 collect (compile-argument arg))))))))

(defun allocate-globals (head query &rest more-queries)
  (let ((map (make-hash-table :test #'equalp))
        (ret (make-hash-table :test #'equalp))
        (global -1))
    (labels ((recurse (arg &key (seen (make-hash-table :test #'equalp)))
               (typecase arg
                 (var
                  (unless (gethash arg seen)
                    (setf (gethash arg seen) t)
                    (if (gethash arg map)
                        (incf (gethash arg map))
                        (setf (gethash arg map) 1))))
                 (wam-cons
                  (recurse (wam-car arg) :seen seen)
                  (recurse (wam-cdr arg) :seen seen))
                 (cons
                  (loop for elem in arg
                        do (recurse elem :seen seen))))))
      (recurse (list head query))
      (loop for query in more-queries
            do (recurse query)))
    (loop for key being the hash-keys of map
            using (hash-value val)
          when (< 1 val)
            do (setf (gethash key ret) (make-global-var :name (incf global))))
    ret))

(defun copy-var-map (tbl)
  (let ((ret (make-hash-table :test #'equalp)))
    (loop for key being the hash-keys of tbl
            using (hash-value val)
          do (setf (gethash key ret) val))
    ret))

(defun hash-keys (tbl)
  (loop for key being the hash-keys of tbl
        collect key))

(defun compile-rule (head query &rest more-queries &aux neck-cut)
  (when (eql '! query)
    (loop while (eql '! (first more-queries))
          do (pop more-queries))
    (if more-queries
        (setf neck-cut t
              query (pop more-queries))
        (progn
          (assert head)
          (compile-fact head :neck-cut t))))
  (let* ((global-map (apply #'allocate-globals head query more-queries))
         (map (copy-var-map global-map))
         (reserved (max (length (rest head)) (length (rest query))))
         (seen (make-hash-table :test #'equalp))
         (num-global-registers (length (hash-keys global-map)))
         (deep-cut (when (loop for query in more-queries thereis (eql '! query))
                     (make-global-var :name num-global-registers))))
    (append
     `((allocate ,(+ num-global-registers (if deep-cut 1 0))))
     (when deep-cut
       `((get-level ,deep-cut)))
     (when head
       (compile-term (flatten-program head :min-reserved reserved :bindings map) :seen seen))
     (when neck-cut
       `((neck-cut)))
     (compile-term (flatten-query query :min-reserved reserved :bindings map) :query-p t :seen seen)
     (if (consp query)
         `((call ,(first query) ,(length (rest query))))
         `((call ,query 0)))
     (loop for query in more-queries
           do (loop for key in (hash-keys seen)
                    when (local-var-p key)
                      do (remhash key seen))
           append (compile-term (flatten-query query :bindings (copy-var-map global-map)) :query-p t :seen seen)
           collect (etypecase query
                     ((eql !) `(cut ,deep-cut))
                     (cons `(call ,(first query) ,(length (rest query))))
                     (symbol `(call ,query 0))))
     `((deallocate)))))

(defun compile-query (query &rest more-queries)
  (apply #'compile-rule nil query more-queries))

(defun compile-fact (fact &key neck-cut)
  (append
   (compile-term (flatten-program fact))
   (when neck-cut
     `((neck-cut)))
   `((proceed))))

(defun get-functor (rule)
  (when (eql :- (first rule))
    (setf rule (second rule)))
  `(/ ,(first rule) ,(length (rest rule))))

(defun compile-rules (rule &rest more-rules)
  (labels ((compile-rule-local (rule)
             (if (eql :- (first rule))
                 (apply #'compile-rule (rest rule))
                 (compile-fact rule))))
    (let ((symbols (loop repeat (length more-rules)
                         collect (gensym "L")))
          (functor (get-functor rule)))
      (append
       (list functor)
       (when (< 0 (length more-rules))
         `((try-me-else ,(first symbols) ,(second functor))))
       (compile-rule-local rule)
       (loop for rule = (pop more-rules)
             while rule
             do (assert (equal functor (get-functor rule)))
             collect (pop symbols)
             collect (if more-rules
                         `(retry-me-else ,(first symbols) ,(second functor))
                         `(trust-me ,(second functor)))
             append (compile-rule-local rule))))))

(defun compile-program (rules)
  (let (groups)
    (loop for rule across rules
          for functor = (get-functor rule)
          for assoc = (assoc functor groups :test #'equal)
          do (if assoc
                 (push rule (cdr assoc))
                 (push (list functor rule) groups))
          finally (return (mapcan (lambda (group) (apply #'compile-rules (reverse (cdr group))))
                                  (reverse groups))))))

(defstruct logic-module
  (code nil :type vector)
  (map (make-hash-table :test #'equal) :type hash-table))

(defmacro with-query-loaded ((var module queries) &body body)
  (let ((module-sym (gensym "MODULE"))
        (code-sym (gensym "CODE"))
        (var-sym (gensym "VAR")))
    `(let* ((,module-sym ,module)
            (,code-sym (logic-module-code ,module-sym))
            (,var-sym (length ,code-sym))
            (,var ,var-sym))
       (unwind-protect
            (progn
              (link-program
               (apply #'compile-query ,queries)
               ,module-sym)
              ,@body)
         (setf (fill-pointer ,code-sym) ,var-sym)))))

(defun link-program (insts &optional module)
  (let* ((map (if module
                  (logic-module-map module)
                  (make-hash-table :test #'equal)))
         (ret (if module
                  (logic-module-code module)
                  (make-array 10 :adjustable t :fill-pointer 0)))
         (start (length ret)))
    (loop for inst in insts
          do (if (or (symbolp inst) (eql '/ (first inst)))
                 (if (gethash inst map)
                     (error "functor already exists")
                     (setf (gethash inst map) (length ret)))
                 (vector-push-extend inst ret)))
    (loop for i from start below (length ret)
          for inst = (aref ret i)
          do (case (first inst)
               ((call)
                (let ((value (gethash (list '/ (second inst) (third inst)) map)))
                  (when value
                    (setf (aref ret i) `(call ,value ,(third inst))))))
               ((try-me-else retry-me-else)
                (setf (aref ret i) (list (first inst) (gethash (second inst) map) (third inst))))))
    (if module module (make-logic-module :code ret :map map))))

(defstruct stack-pointer
  (offset 0 :type (integer 0)))

(defparameter *builtins* (make-hash-table :test #'equal))
(defparameter *heap* nil)

(defmacro define-builtin (name args &body body)
  `(setf (gethash (list ',name ,(length args)) *builtins*)
         (lambda ,args ,@body)))

(define-condition backtrack () ())
(define-condition halt ()
  ((value :initarg :value :accessor halt-value)))

(defun exec (module queries)
  (declare (optimize (debug 3)))
  (with-query-loaded (p module queries)
    (let* ((heap (make-array 1028 :initial-element nil :fill-pointer 0 :adjustable t))
           (*heap* heap)
           (registers (make-array 1028 :initial-element nil))
           (trail (make-array (* 100 1028) :initial-element nil))
           (code (logic-module-code module))
           (s 0)
           (e 0)
           (cp nil)
           (b nil)
           (b0 nil)
           (hb nil)
           (tr 0)
           (write-p nil)
           (num-of-args nil)
           (pdl (make-array 1028 :initial-element nil :fill-pointer 0))
           (stack (make-array (expt 2 15)))
           fail)
      (symbol-macrolet ((h (length heap)))
        (labels ((backtrack ()
                   (error 'backtrack))
                 (store-get (addr)
                   (etypecase addr
                     (integer
                      (aref heap addr))
                     (local-var
                      (aref registers (local-var-name addr)))
                     (global-var
                      (aref stack (+ e (global-var-name addr) 3)))
                     (stack-pointer
                      (aref stack (stack-pointer-offset addr)))))
                 (store-set (addr value)
                   (etypecase addr
                     (integer
                      (setf (aref heap addr) value))
                     (local-var
                      (setf (aref registers (local-var-name addr)) value))
                     (global-var
                      (setf (aref stack (+ e (global-var-name addr) 3)) value))
                     (stack-pointer
                      (setf (aref stack (stack-pointer-offset addr)) value))))
                 (deref (addr)
                   (destructuring-bind (tag value) (store-get addr)
                     (if (and (eql tag 'ref) (not (eql value addr)))
                         (deref value)
                         addr)))
                 (bind (addr1 addr2)
                   (if (and (eql 'ref (first (store-get addr1)))
                            (or (not (eql 'ref (first (store-get addr2))))
                                (< addr2 addr1)))
                       (progn
                         (store-set addr1 (store-get addr2))
                         (trail addr1))
                       (progn
                         (store-set addr2 (store-get addr1))
                         (trail addr2))))
                 (trail (addr)
                   (check-type addr (integer 0))
                   (when (and hb (< addr hb))
                     (setf (aref trail tr) addr)
                     (incf tr)))
                 (unwind-trail (addr1 addr2)
                   (check-type addr1 (integer 0))
                   (check-type addr2 (integer 0))
                   (loop for i from addr1 below addr2
                         do (store-set (aref trail i) `(ref ,(aref trail i)))))
                 (tidy-trail ()
                   (loop with i = (aref stack (+ b (aref stack b) 5))
                         while (< i tr)
                         do (if (and hb (< (aref trail i) hb))
                                (incf i)
                                (setf (aref trail i) (aref trail (1- tr))
                                      tr (1- tr)))))
                 (unify (addr1 addr2)
                   (setf (fill-pointer pdl) 0)
                   (vector-push addr1 pdl)
                   (vector-push addr2 pdl)
                   (loop while (not (= 0 (length pdl)))
                         do (let ((d1 (deref (vector-pop pdl)))
                                  (d2 (deref (vector-pop pdl))))
                              (when (not (equal d1 d2))
                                (destructuring-bind (t1 v1) (store-get d1)
                                  (destructuring-bind (t2 v2) (store-get d2)
                                    (if (eql t1 'ref)
                                        (bind d1 d2)
                                        (ecase t2
                                          ((ref)
                                           (bind d1 d2))
                                          ((lis)
                                           (unless (eql t1 'lis)
                                             (backtrack))
                                           (vector-push v1 pdl)
                                           (vector-push v2 pdl)
                                           (vector-push (1+ v1) pdl)
                                           (vector-push (1+ v2) pdl))
                                          ((con)
                                           (unless (and (eql t1 'con) (eql v1 v2))
                                             (backtrack)))
                                          ((str)
                                           (unless (eql t1 'str)
                                             (backtrack))
                                           (destructuring-bind (f1 n1) (rest (store-get v1))
                                             (destructuring-bind (f2 n2) (rest (store-get v2))
                                               (if (and (eql f1 f2) (eql n1 n2))
                                                   (loop for i from 1 to n1
                                                         do (vector-push (+ v1 i) pdl)
                                                         do (vector-push (+ v2 i) pdl))
                                                   (backtrack)))))))))))))
                 (heap-push (value)
                   (vector-push-extend value heap)))
          (loop while (and p (not fail))
                for inst = (aref code p)
                ;; do (format t "~%~s ~s" p inst)
                do (handler-case
                       (ecase (first inst)
                         ((put-structure)
                          (heap-push `(str ,(1+ h)))
                          (heap-push (second inst))
                          (store-set (third inst) (aref heap (- h 2)))
                          (incf p))
                         ((put-variable)
                          (heap-push `(ref ,h))
                          (store-set (second inst) (aref heap (1- h)))
                          (store-set (third inst) (aref heap (1- h)))
                          (incf p))
                         ((put-value)
                          (store-set (third inst) (store-get (second inst)))
                          (incf p))
                         ((put-constant)
                          (store-set (third inst) `(con ,(second inst)))
                          (incf p))
                         ((put-list)
                          (store-set (second inst) `(lis ,h))
                          (incf p))
                         ((set-variable)
                          (heap-push `(ref ,h))
                          (store-set (second inst) (aref heap (1- h)))
                          (incf p))
                         ((set-value)
                          (heap-push (store-get (second inst)))
                          (incf p))
                         ((set-constant)
                          (heap-push `(con ,(second inst)))
                          (incf p))
                         ((get-variable)
                          (store-set (second inst) (store-get (third inst)))
                          (incf p))
                         ((get-value)
                          (unify (second inst)
                                 (third inst))
                          (incf p))
                         ((get-structure)
                          (let* ((addr (deref (third inst)))
                                 (store (store-get addr)))
                            (case (first store)
                              ((ref)
                               (heap-push `(str ,(1+ h)))
                               (heap-push (second inst))
                               (bind addr (- h 2))
                               (setf write-p t))
                              ((str)
                               (if (equal (aref heap (second store)) (second inst))
                                   (setf s (1+ (second store))
                                         write-p nil)
                                   (backtrack)))
                              (t (backtrack)))
                            (incf p)))
                         ((get-constant)
                          (let* ((addr (deref (third inst)))
                                 (store (store-get addr)))
                            (case (first store)
                              ((ref)
                               (store-set addr `(con ,(second inst)))
                               (trail addr))
                              ((con)
                               (unless (eql (second store) (second inst))
                                 (backtrack)))
                              (t (backtrack)))
                            (incf p)))
                         ((get-list)
                          (let* ((addr (deref (second inst)))
                                 (store (store-get addr)))
                            (case (first store)
                              ((ref)
                               (heap-push `(lis ,(1+ h)))
                               (bind addr (- h 1))
                               (setf write-p t))
                              ((lis)
                               (setf s (second store)
                                     write-p nil))
                              (t (backtrack)))
                            (incf p)))
                         ((unify-variable)
                          (if write-p
                              (progn
                                (heap-push `(ref ,h))
                                (store-set (second inst) (aref heap (1- h))))
                              (store-set (second inst) (aref heap s)))
                          (incf s)
                          (incf p))
                         ((unify-value)
                          (if write-p
                              (heap-push (store-get (second inst)))
                              (unify (second inst) s))
                          (incf s)
                          (incf p))
                         ((unify-constant)
                          (if write-p
                              (heap-push `(con ,(second inst)))
                              (let* ((addr (deref s))
                                     (store (store-get addr)))
                                (case (first store)
                                  ((ref)
                                   (store-set addr `(con ,(second inst)))
                                   (trail addr))
                                  ((con)
                                   (unless (eql (second store) (second inst))
                                     (backtrack)))
                                  (t (backtrack)))))
                          (incf s)
                          (incf p))
                         ((call)
                          (etypecase (second inst)
                            (integer
                             (setf cp (1+ p)
                                   b0 b
                                   p (second inst)
                                   num-of-args (third inst)))
                            (symbol
                             (let* ((f (second inst))
                                    (n (third inst))
                                    (func (gethash (list f n) *builtins*)))
                               (if func
                                   (apply func
                                          (loop for arg across registers
                                                for i from 0 below (third inst)
                                                collect arg))
                                   (backtrack))
                               (incf p)))))
                         ((proceed)
                          (setf p cp))
                         ((allocate)
                          (let ((new-e (if (or (not b) (< b e))
                                           (+ e (aref stack (+ e 2)) 3)
                                           (+ b (aref stack b) 8))))
                            (setf (aref stack new-e) e
                                  (aref stack (+ new-e 1)) cp
                                  (aref stack (+ new-e 2)) (second inst)
                                  e new-e))
                          (incf p))
                         ((deallocate)
                          (setf p (aref stack (1+ e))
                                e (aref stack e)))
                         ((try-me-else)
                          (let ((new-b (if (or (not b) (< b e))
                                           (+ e (aref stack (+ e 2)) 3)
                                           (+ b (aref stack b) 8)))
                                (n num-of-args))
                            ;; (format t "~%call: ~s ~{~s~^ ~}" (third inst) (loop for i below num-of-args collect (construct (aref registers i))))
                            (loop for i from 0 below n
                                  do (setf (aref stack (+ new-b i 1)) (aref registers i)))
                            (setf (aref stack new-b) n
                                  (aref stack (+ new-b n 1)) e
                                  (aref stack (+ new-b n 2)) cp
                                  (aref stack (+ new-b n 3)) b
                                  (aref stack (+ new-b n 4)) (second inst)
                                  (aref stack (+ new-b n 5)) tr
                                  (aref stack (+ new-b n 6)) h
                                  (aref stack (+ new-b n 7)) b0
                                  b new-b
                                  hb h))
                          (incf p))
                         ((retry-me-else)
                          (let ((n (aref stack b)))
                            (loop for i from 0 below n
                                  do (setf (aref registers i) (aref stack (+ b i 1))))
                            (setf e (aref stack (+ b n 1))
                                  cp (aref stack (+ b n 2))
                                  (aref stack (+ b n 4)) (second inst))
                            (unwind-trail (aref stack (+ b n 5)) tr)
                            (setf tr (aref stack (+ b n 5)))
                            (setf (fill-pointer heap) (aref stack (+ b n 6)))
                            (setf hb h))
                          (incf p))
                         ((trust-me)
                          ;; (format t "deallocating branch: ~s ~s~%" (second inst) b)
                          (let ((n (aref stack b)))
                            (loop for i from 0 below n
                                  do (setf (aref registers i) (aref stack (+ b i 1))))
                            (setf e (aref stack (+ b n 1))
                                  cp (aref stack (+ b n 2)))
                            (unwind-trail (aref stack (+ b n 5)) tr)
                            (setf tr (aref stack (+ b n 5)))
                            (setf (fill-pointer heap) (aref stack (+ b n 6)))
                            (setf b (aref stack (+ b n 3))
                                  hb (when b (aref stack (+ b n 6)))))
                          (incf p))
                         ((neck-cut)
                          (when (> b b0)
                            (setf b b0
                                  hb (when b (aref stack (+ b (aref stack b) 6))))
                            (tidy-trail))
                          (incf p))
                         ((get-level)
                          (store-set (second inst) b0)
                          (incf p))
                         ((cut)
                          (let ((stored-b0 (store-get (second inst))))
                            (when (> b stored-b0)
                              (setf b stored-b0
                                    hb (when b (aref stack (+ b (aref stack b) 6))))
                              (tidy-trail)))
                          (incf p)))
                     (halt (e)
                       (return-from exec (halt-value e)))
                     (backtrack ()
                       (if b
                           (progn
                             (setf b0 (aref stack (+ b (aref stack b) 7)))
                             (setf p (aref stack (+ b (aref stack b) 4))))
                           (setf fail t)))))
          (and (not fail) t))))))

(defun deref (addr)
  (loop while (eql 'ref (first addr))
        do (let ((candidate (aref *heap* (second addr))))
             (if (equal candidate addr)
                 (return addr)
                 (setf addr candidate)))
        finally (return addr)))

(defun partial-construct (addr &optional (recurse #'identity))
  (let ((val (deref addr)))
    (ecase (first val)
      ((ref)
       (make-var :name (make-symbol (format nil "_~d" (second val)))))
      ((con)
       (second val))
      ((str)
       (let ((addr (second val)))
         (destructuring-bind (f n) (rest (aref *heap* addr))
           (cons f (loop for i from 1 to n
                         collect (funcall recurse (aref *heap* (+ addr i))))))))
      ((lis)
       (let ((addr (second val)))
         (wam-cons (funcall recurse (aref *heap* addr))
                   (funcall recurse (aref *heap* (1+ addr)))))))))

(defun construct (addr)
  (partial-construct addr #'construct))

(define-builtin writeln (arg)
  (format t "~%~s" (construct arg)))

(define-builtin halt (arg)
  (error 'halt :value (construct arg)))

(define-builtin var (arg)
  (typecase (partial-construct arg)
    (var)
    (t (error 'backtrack))))

(define-builtin structure (arg)
  (typecase (partial-construct arg)
    (cons)
    (t (error 'backtrack))))

(define-builtin integer (arg)
  (typecase (partial-construct arg)
    (integer)
    (t (error 'backtrack))))

(define-builtin character (arg)
  (typecase (partial-construct arg)
    (character)
    (t (error 'backtrack))))

(define-builtin symbol (arg)
  (typecase (partial-construct arg)
    (symbol)
    (t (error 'backtrack))))
