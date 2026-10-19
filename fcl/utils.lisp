(in-package "FCL")


(define-condition required-argument (error)
  ((symbol :initarg :symbol
           :reader required-argument-symbol
           :type symbol))
  (:report (lambda (condition stream)
             (format stream "~s is a required argument" (required-argument-symbol condition)))))

(deftype non-negative-integer () `(integer 0))
(deftype positive-integer () `(integer 1))

(defun required-argument (symbol)
  (error 'required-argument :symbol symbol))

(defmacro comptime (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun align-extend (offset alignment)
  (if (= 0 (mod offset alignment))
      offset
      (+ (- offset (mod offset alignment))
         alignment)))

(defun flatten (x)
  (cond ((null x) nil)
        ((consp x) (loop for y in x append (flatten y)))
        (t (list x))))

(defun lastcar (list &optional (n 1))
  (car (last list n)))

(defun hashash (key hash-table)
  (nth-value 1 (gethash key hash-table)))

(defun gethash-strict (key hash-table)
  (multiple-value-bind (ret found) (gethash key hash-table)
    (when (not found)
      (error "key not found in hash-table: ~s" key))
    ret))

(defun (setf gethash-strict) (val key hash-table)
  (when (nth-value 1 (gethash key hash-table))
    (error "key already in hash-table: ~s" key))
  (setf (gethash key hash-table) val))

(defun copy-hash-table (tbl &key (copy-key #'identity) (copy-value #'identity) test size rehash-size rehash-threshold)
  (let ((ret (make-hash-table :test (or test (hash-table-test tbl))
                              :size (or size (hash-table-size tbl))
                              :rehash-size (or rehash-size (hash-table-rehash-size tbl))
                              :rehash-threshold (or rehash-threshold (hash-table-rehash-threshold tbl)))))
    (loop for key being the hash-keys of tbl
            using (hash-value val)
          do (setf (gethash (funcall copy-key key) ret) (funcall copy-value val)))
    ret))

(defun make-special-variable (symbol)
  (check-type symbol symbol)
  (sb-impl::%defvar symbol (sb-c:source-location)))

(defun special-variable-p (symbol)
  (eql (sb-int:info :variable :kind symbol) :special))

(defun make-dynamic-array (&rest keys)
  (apply #'make-array 16 :adjustable t :fill-pointer 0 keys))

(defun xor (b1 b2)
  (or (and b1 (not b2)) (and (not b1) b2)))

;; (defmethod print-object ((tbl hash-table) stream)
;;   (format stream "~@<#{~;~{~{~s → ~s~}~^, ~}~;}~@:>"
;;           (loop for key being the hash-keys of tbl
;;                   using (hash-value val)
;;                 collect (list key val))))

(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for symbol in symbols
               collect (let ((name (etypecase symbol
                                     (cons
                                      (destructuring-bind (symbol1 name1) symbol
                                        (setf symbol symbol1)
                                        name1))
                                     (symbol
                                      (symbol-name symbol)))))
                         (list symbol `(gensym ,name))))
     ,@body))

(defmacro once-only (symbols &body body)
  (let ((symbol-vars (loop for symbol in symbols collect (gensym (symbol-name symbol)))))
    `(let ,(loop for var in symbol-vars collect `(,var (gensym ,(symbol-name var))))
       (list 'let (list ,@(loop for symbol in symbols and var in symbol-vars collect `(list ,var ,symbol)))
             (let ,(loop for symbol in symbols and var in symbol-vars collect `(,symbol ,var))
               ,@body)))))

(defmacro incf* (place &optional (n 1) &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (with-gensyms (tmp)
      `(let* (,@(mapcar #'list vars vals) (,tmp ,reader-form))
         (prog1 ,tmp
           (let ((,(car store-vars) (+ ,tmp ,n)))
             ,writer-form))))))

(defmacro decf* (place &optional (n 1) &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (with-gensyms (tmp)
      `(let* (,@(mapcar #'list vars vals) (,tmp ,reader-form))
         (prog1 ,tmp
           (let ((,(car store-vars) (- ,tmp ,n)))
             ,writer-form))))))

(defmacro abort-protect (form &body body)
  "Like UNWIND-PROTECT except that BODY is executed only if FORM exits nonlocally."
  (with-gensyms (failed)
    `(let ((,failed t))
       (unwind-protect
           (multiple-value-prog1 ,form
             (setf ,failed nil))
         (when ,failed
           ,@body)))))

(defmacro switch-macro-translate (translations &body body &aux (max 1))
  (when translations
    (setf max (max max (loop for (name . rest) in translations maximize (length rest)))))
  (labels ((count-switches (x)
             (typecase x
               (cons
                (if (eql 'switch (car x))
                    (setf max (max max (length (cdr x))))
                    (progn (count-switches (car x)) (count-switches (cdr x)))))
               (sb-impl::comma
                (count-switches (sb-int:comma-expr x)))))
           (aux (x n)
             (typecase x
               (cons
                (if (eql 'switch (car x))
                    (aux (nth n (cdr x)) n)
                    (cons (aux (car x) n) (aux (cdr x) n))))
               (sb-impl::comma
                (sb-int:unquote (aux (sb-int:comma-expr x) n) (sb-int:comma-kind x)))
               (symbol
                (let ((assoc (assoc x translations)))
                  (if assoc (nth n (cdr assoc)) x)))
               (t x))))
    (count-switches body)
    `(progn
       ,@(loop for n below max append (aux body n))
       nil)))

(defmacro switch-macro (&body body)
  `(switch-macro-translate () ,@body))

(defstruct dll-node
  (next nil :type (or null dll-node))
  (prev nil :type (or null dll-node))
  (data nil)
  (list nil))

(defstruct dll
  (start nil :type (or null dll-node)))

(defmethod print-object ((node dll-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":data ~s" (dll-node-data node))))

(defmethod print-object ((list dll) stream)
  (format stream "#dll(~{~s~^ ~})" (dll-list list)))

(defun delete-dll-node (node)
  (let ((list (dll-node-list node))
        (next (dll-node-next node))
        (prev (dll-node-prev node)))
    (when prev (setf (dll-node-next prev) next))
    (when next (setf (dll-node-prev next) prev))
    (when (eq node (dll-start list))
      (setf (dll-start list) next))
    list))

(defun dll-push (data list)
  (let* ((start (dll-start list))
         (node (make-dll-node :list list :data data :next start)))
    (when start
      (setf (dll-node-prev start) node))
    (setf (dll-start list) node)
    node))

(defmacro dll-dolist ((var list &key result (node (gensym "NODE"))) &body body)
  `(loop for ,node = (dll-start ,list) then (dll-node-next ,node)
         while ,node
         for ,var = (dll-node-data ,node)
         do (progn ,@body)
         finally (return ,result)))

(defmacro dll-dolist-maybe ((var list &rest keys &key &allow-other-keys) &body body)
  (let ((sym (gensym "LIST")))
    `(let ((,sym ,list))
       (when ,sym
         (dll-dolist (,var ,sym ,@keys) ,@body)))))

(defun dll-list (dll)
  (let* ((ret (list nil))
         (last ret))
    (dll-dolist (elem dll)
      (setf last (cdr (rplacd last (list elem)))))
    (cdr ret)))

(defun dll-member (item list &key (test #'eql))
  (dll-dolist (elem list :node node)
    (when (funcall test item elem)
      (return node))))

(defstruct (ring-buffer (:constructor %make-ring-buffer))
  (buffer nil :type simple-vector)
  (empty-p t :type boolean)
  ;; Indexes are all (mod (length buffer))
  (start 0 :type non-negative-integer)
  (end 0 :type non-negative-integer))

(defun make-ring-buffer (size)
  (declare (type non-negative-integer size))
  (%make-ring-buffer :buffer (make-array size)))

(defun ring-buffer-length (ring-buffer)
  (with-slots (buffer empty-p start end) ring-buffer
    (cond (empty-p 0)
          ((< start end) (- end start))
          (t (+ (- (length buffer) start) end)))))

(defun ring-buffer-size (ring-buffer)
  (length (ring-buffer-buffer ring-buffer)))

(defun ring-buffer-full-p (ring-buffer)
  (= (ring-buffer-size ring-buffer) (ring-buffer-length ring-buffer)))

(defun ring-buffer-internal-idx (ring-buffer i)
  (let ((length (ring-buffer-length ring-buffer)))
    (assert (<= 0 i (1- length))
            nil "Invalid index ~d for ring-buffer of length ~d" i length))
  (mod (+ i (ring-buffer-start ring-buffer)) (ring-buffer-size ring-buffer)))

(defun ring-buffer-aref (ring-buffer i)
  (aref (ring-buffer-buffer ring-buffer) (ring-buffer-internal-idx ring-buffer i)))

(defun (setf ring-buffer-aref) (new ring-buffer i)
  (setf (aref (ring-buffer-buffer ring-buffer) (ring-buffer-internal-idx ring-buffer i)) new))

(defun ring-buffer-push (item ring-buffer &key delete)
  (with-slots (buffer empty-p start end) ring-buffer
    (let ((size (length buffer)) (idx end))
      (when (and (not empty-p) (= end start))
        (when delete (funcall delete (aref buffer idx)))
        (setf start (mod (1+ start) size)))
      (setf end (mod (1+ end) size)
            empty-p nil
            (aref buffer idx) item)))
  ring-buffer)

(defun ring-buffer-pop (ring-buffer)
  (with-slots (buffer empty-p start end) ring-buffer
    (assert (not empty-p))
    (setf end (mod (1- end) (length buffer))
          empty-p (= end start))
    (shiftf (aref buffer end) 0)))

(defun compile-and-load (file &rest args)
  (let ((pathname (pathname file)))
    (apply #'compile-file pathname args)
    (load (merge-pathnames (make-pathname :type "fasl") pathname))))

(defun read-file-string (filespec)
  (with-output-to-string (out)
    (with-open-file (in filespec :direction :input)
      (let ((string (make-string 1000))
            (n 0))
        (loop while (< 0 (setf n (read-sequence string in)))
              do (write-string string out :end n))))))

(defmacro define (&rest args)
  (declare (ignore args))
  (error "DEFINE may not be used outside of PROGS form."))

(defmacro macro (&rest args)
  (declare (ignore args))
  (error "MACRO may not be used outside of PROGS form."))

(defun prognify (forms)
  (if (and (consp forms) (cdr forms))
      `(progn ,@forms)
      (car forms)))

(defun unprognify (form)
    (cond ((and (consp form) (eql 'progn (first form)))
           (rest form))
          ((null form)
           nil)
          (t
           (list form))))
  
(defun listify (form)
  (if (listp form) form (list form)))

(defun apply-bindings (binding-groups form)
  "Wraps FORM in the correct combination of LET, LET*, LABELS, DESTRUCTURING-BIND,
and MULTIPLE-VALUE-BIND that match the given BINDING-GROUPS.  BINDING-GROUPS is
a list of binding-groups which are to be applied simultaneously (within a
binding-group variables are bound like LET rather than LET*).  Each
binding-group is a list of bindings.  Each binding is a form of shape (LHS RHS).
RHS is a normal expression form, and LHS is one of the following:
- (VALUES ,symbol1 ... ,symboln) :: binds symbols with LET or MULTIPLE-VALUE-BIND
- (LIST ,@lambda-list) :: binds symbols using DESTRUCTURING-BIND
- (FUNCTION ,name ,@args) :: binds a function using LABELS (the RHS is not evaluated)"
  (let ((stubs nil)
        (bivalent-stubs (make-hash-table)))
    ;; - STUBS is a list of form-headers to be nested inner-first onto FORM.
    ;; - Each stub can be either (LET ,bindings), (LET* ,bindings), (MULTIPLE-VALUE-BIND ,vars ,val),
    ;;   (LABELS ,bindings), or NIL.
    ;; - A stub of NIL is ignored on form-construction (it is used to delineate binding-groups).
    ;; - BIVALENT-STUBS are LET stubs that may be converted to LET* without changing the semantics
    (labels ((process-binding-group (binding-group)
               (let ((temp-vars (make-hash-table))
                     (functions nil))
                 (push nil stubs)
                 (loop for (lhs rhs . rest) in binding-group
                       do (assert (endp rest) nil "Extra elements in binding ~s" rest)
                          (ecase (first lhs)
                            (values
                             (let ((symbols (rest lhs)))
                               (loop for symbol in symbols
                                     do (assert (symbolp symbol)
                                                nil "Can not bind non-symbol values in LHS ~s" lhs))
                               (case (length symbols)
                                 (0
                                  (with-gensyms (ignore)
                                    (push-let-stub ignore rhs :ignore t)))
                                 (1
                                  (push-let-stub (make-temp-var (first symbols) temp-vars) rhs))
                                 (t
                                  (push `(multiple-value-bind
                                             ,(loop for symbol in symbols
                                                    collect (make-temp-var symbol temp-vars))
                                             ,rhs)
                                        stubs)))))
                            (list
                             (let ((bindings (expand-pattern (rest lhs) rhs)))
                               (loop for pair in bindings
                                     do (when (symbol-in-tree (first pair) (rest lhs))
                                          (setf (first pair) (make-temp-var (first pair) temp-vars))))
                               (push `(let* ,bindings) stubs)))
                            (function
                             (assert (symbolp (second lhs)) nil "Function name must be a symbol in LHS ~s" lhs)
                             (push (list (second lhs) (cddr lhs) rhs) functions))))
                 (when functions
                   (push `(labels ,functions) stubs))
                 (case (first (first stubs))
                   (let (loop for pair in (second (first stubs))
                              do (awhen (gethash (first pair) temp-vars)
                                   (remhash (shiftf (first pair) it) temp-vars))))
                   ((multiple-value-bind)
                    (when (loop for temp-var being the hash-keys of temp-vars
                                always (member temp-var (second (first stubs))))
                      (loop for cons on (second (first stubs))
                            do (awhen (gethash (car cons) temp-vars)
                                 (remhash (shiftf (car cons) it) temp-vars))))))
                 (loop for temp being the hash-keys of temp-vars
                       using (hash-value symbol)
                       do (push-let-stub symbol temp :bivalent t))))
             (symbol-in-tree (symbol tree)
               (if (consp tree)
                   (or (symbol-in-tree symbol (car tree))
                       (symbol-in-tree symbol (cdr tree)))
                   (eql symbol tree)))
             (make-temp-var (symbol temp-vars)
               (let ((temp-var (gensym (symbol-name symbol))))
                 (setf (gethash temp-var temp-vars) symbol)
                 temp-var))
             (push-let-stub (var val &key ignore bivalent)
               (if (eql 'let (first (first stubs)))
                   (progn
                     (setf (cdr (last (second (first stubs)))) (list (list var val)))
                     (unless bivalent
                       (remhash (first stubs) bivalent-stubs)))
                   (progn
                     (push `(let ((,var ,val))) stubs)
                     (when bivalent
                       (setf (gethash (first stubs) bivalent-stubs) t))))
               (when ignore
                 (let* ((declare (or (cddr (first stubs))
                                     (car (setf (cddr (first stubs)) (list (list 'declare))))))
                        (ignores (or (assoc 'ignore (cdr declare))
                                     (car (push (list 'ignore) (cdr declare))))))
                   (pushnew var (cdr ignores))))
               nil)
             (stub-mergeable-p (stub)
               (or (eql 'let* (first stub))
                   (and (eql 'let (first stub))
                        (or (gethash stub bivalent-stubs) (= 1 (length (second stub)))))))
             (normalise-stubs (stubs)
               "Removes NIL stubs, and collapses chains of single-LET's into LET* stubs."
               (setf stubs (delete nil stubs))
               (loop for cons on stubs
                     do (loop while (and (cdr cons)
                                         (stub-mergeable-p (first cons))
                                         (stub-mergeable-p (second cons)))
                              do (setf (first cons)
                                       `(let* ,(nconc (second (second cons)) (second (first cons)))
                                          ,@(cddr (second cons))
                                          ,@(cddr (first cons))))
                                 (setf (cdr cons) (cddr cons))))
               stubs))
      (loop for group in binding-groups
            do (process-binding-group group))
      (let ((ret form))
        (loop for stub in (normalise-stubs stubs)
              do (setf ret (nconc stub (unprognify ret))))
        ret))))

(defun normalise-bindings (bindings)
  (loop for (lhs rhs . rest) in bindings
        do (assert (endp rest) nil "Extra elements in binding ~s" rest)
        collect (list (etypecase lhs
                        (symbol `(values ,lhs))
                        (cons
                         (if (member (first lhs) '(values list))
                             lhs
                             `(function ,@lhs))))
                      rhs)))

(defmacro bind (bindings &body body)
  (apply-bindings
   (list (normalise-bindings bindings))
   `(progn ,@body)))

(defmacro bind* (bindings &body body)
  (apply-bindings
   (mapcar #'list (normalise-bindings bindings))
   `(progn ,@body)))

(defun apply-macro-bindings (bindings form)
  (let ((macro-bindings nil)
        (symbol-macro-bindings nil))
    (loop for binding in bindings
          do (assert (and (listp binding) (<= 1 (length binding) 2))
                     nil "Invalid binding form ~s" binding)
             (destructuring-bind (lhs &optional rhs) binding
               (etypecase lhs
                 (cons (push `(,(first lhs) ,(rest lhs) ,@(unprognify rhs)) macro-bindings))
                 (symbol (push `(,lhs ,rhs) symbol-macro-bindings)))))
    (let ((ret form))
      (when symbol-macro-bindings
        (setf ret `(symbol-macrolet ,(nreverse symbol-macro-bindings) ,@(unprognify ret))))
      (when macro-bindings
        (setf ret `(macrolet ,(nreverse macro-bindings) ,@(unprognify ret))))
      ret)))

(defmacro macro-bind (bindings &body body)
  (apply-macro-bindings bindings `(progn ,@body)))

;; TODO: do bind* and make sure that normal case expands to let*

(defmacro progs (&body body &environment env)
  ;; TODO: delete
  ;; Stands for "prog scope"
  (labels ((apply-bindings (bindings body)
             (if bindings
                 (list (append (car bindings)
                         (apply-bindings (cdr bindings) body)))
                 body))
           (unsplice (pairs)
             (loop while pairs
                   collect (pop pairs) into vars
                   collect (pop pairs) into forms
                   finally (return (values vars forms))))
           (cons-car-p (car form)
             (and (consp form) (eql car (car form))))
           (expand (body)
             (when body
               (let ((form (car body)))
                 (loop with expanded-p = t
                       until (or (cons-car-p 'define form) (cons-car-p 'macro form) (not expanded-p))
                       do (multiple-value-setq (form expanded-p) (macroexpand-1 form env)))
                 (if (and (consp form) (member (car form) '(define macro)))
                     (let ((expand (ecase (car form) (define #'expand-define) (macro #'expand-macro))))
                       (assert (evenp (length (cdr form))))
                       (multiple-value-call expand (unsplice (cdr form)) (expand (cdr body))))
                     (cons form (expand (cdr body)))))))
           (expand-macro (vars forms body)
             (let ((symbol-bindings nil)
                   (macro-bindings nil))
               (loop for var in vars and form in forms
                     do (if (consp var)
                            (push `(,(car var) ,(cdr var) ,form) macro-bindings)
                            (push `(,var ,form) symbol-bindings)))
               ;; TODO: `declare' handling
               (apply-bindings (append (when macro-bindings
                                         `((macrolet ,(reverse macro-bindings))))
                                       (when symbol-bindings
                                         `((symbol-macrolet ,(reverse symbol-bindings)))))
                               body)))
           (expand-define (vars forms body)
             (let ((bindings nil)
                   (functions nil)
                   (aliases nil))
               (loop for var in vars and form in forms
                     do (cond ((cons-car-p 'values var)
                               (push `(multiple-value-bind ,(cdr var) ,form) bindings))
                              ((cons-car-p 'list var)
                               ;; TODO: implement recursive destructuring to remove this check
                               (loop for sym in (cdr var)
                                     do (assert (symbolp sym)))
                               (push `(destructuring-bind ,(cdr var) ,form) bindings))
                              ((consp var)
                               (push `(,(car var) ,(cdr var) ,form) functions))
                              (t
                               (if (cons-car-p 'let (car bindings))
                                   (setf (cdr (last (second (car bindings)))) `((,var ,form)))
                                   (push `(let ((,var ,form))) bindings)))))
               ;; TODO: `declare' handling
               (loop for binding in (cdr bindings)
                     do (flet ((make-alias (var)
                                 (let ((alias (make-symbol (symbol-name var))))
                                   (push (list var alias) aliases)
                                   alias)))
                          (ecase (first binding)
                            ((multiple-value-bind)
                             (setf (second binding)
                                   (loop for var in (second binding)
                                         collect (make-alias var))))
                            ((destructuring-bind)
                             ;; TODO: implement recursive destructuring
                             (setf (second binding)
                                   (loop for var in (second binding)
                                         collect (if (member var lambda-list-keywords)
                                                     var
                                                     (make-alias var)))))
                            ((let)
                             (loop for pair in (second binding)
                                   do (setf (first pair) (make-alias (first pair))))))))
               (when aliases
                 (if (cons-car-p 'let (car bindings))
                     (setf (cdr (last (second (car bindings)))) aliases)
                     (push `(let ,aliases) bindings)))
               (let* ((var-forms (apply-bindings (reverse bindings) body)))
                 (if functions
                     `((labels ,(reverse functions) ,@var-forms))
                     var-forms)))))
    (prognify (expand body))))

(defun string-invcase (string)
  (let ((ret (make-string (length string) :element-type (array-element-type string))))
    (loop for ch across string and i from 0
          do (setf (aref ret i)
                   (cond ((upper-case-p ch) (char-downcase ch))
                         ((lower-case-p ch) (char-upcase ch))
                         (t ch))))
    ret))

(defun char-caret (char)
  (cond ((char= char #\Del)
         #\?)
        ((char<= #\Nul char #\Us)
         (code-char (logior #x40 (char-code char))))))

(defun caret-char (caret)
  (cond ((char= caret #\?)
         #\Del)
        ((or (char<= #\@ caret #\_) (char<= #\a caret #\z))
         (code-char (logand #x1f (char-code caret))))))

(defun char-width (char)
  (let ((caret (char-caret char)))
    ;; TODO: logic for multi-column unicode characters
    (if caret
        (values 2 #\^ caret)
        (values 1 char nil))))

(defun true-char-width (ch vcol tabsize)
  (if (char= ch #\Tab)
      (values (- (align-extend (1+ vcol) tabsize) vcol) nil nil)
      (char-width ch)))

(defmacro unimplemented (&rest args)
  `(error ,@(if args
                (list* (concatenate 'string "unimplemented: " (first args)) (rest args))
                (list "unimplemented"))))

(defmacro unreachable (&rest args)
  `(error ,@(if args
                (list* (concatenate 'string "unreachable: " (first args)) (rest args))
                (list "unreachable"))))

(defun sign (n)
  (cond ((< 0 n) 1)
        ((= 0 n) 0)
        ((< n 0) -1)))

(defun unescape (string &key (substitute t))
  (let ((ret (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length string))
          do (let ((ch (aref string i)))
               (vector-push-extend
                (if (eql ch #\\)
                    (let ((ch (aref string (incf i))))
                      (if substitute
                          (case ch
                            ((#\n) #\Newline)
                            ((#\t) #\Tab)
                            ((#\r) #\Return)
                            ((#\0) #\Nul)
                            (t ch))
                          ch))
                    ch)
                ret))
          do (incf i))
    (coerce ret 'simple-string)))

;; Testing

(defvar *test-suite*)
(defvar *test-suites* nil)

(define-condition test-error (error) ())
(define-condition assert-error (test-error simple-error) ())

(defun %defsuite (name)
  (delete-suite name)
  (push (cons name (make-array 0 :adjustable t :fill-pointer 0)) *test-suites*))

(defmacro suite-tests (name)
  (once-only (name)
    `(cdr (or (assoc ,name *test-suites*)
              (error "Suite ~a not found" ,name)))))

(defmacro test-suite (name &body body)
  `(let ((*test-suite* ',name))
     (%defsuite ',name)
     ,@body))

(defun %deftest (name function)
  (let ((suite (if (boundp '*test-suite*)
                   *test-suite*
                   (error "Test ~a declared outside of any suite" name))))
    (vector-push-extend (list name function) (suite-tests suite))))

(defmacro test (name &body body)
  `(%deftest ',name (sb-int:named-lambda ,name ()
                      (block ,name
                        ,@body))))

(defun run-suite (name)
  (loop for (name function) across (suite-tests name)
        do (handler-case
               (progn
                 (funcall function)
                 (format t "✓ ~s~%" name))
             (error (e)
               (format t "✗ ~s~%  ~a~%" name e)))))

(defun run-test (suite test)
  (loop for (name function) across (suite-tests suite)
        do (when (eql name test)
             (funcall function))))

(defun delete-suite (name)
  (setf *test-suites* (delete name *test-suites* :key #'first))
  name)

(defun delete-test (suite name)
  (aif (assoc suite *test-suites*)
       (setf (cdr it) (delete name (cdr it) :key #'first))
       (error "Suite ~a not found" suite))
  name)

(defmacro test-equal (x y)
  (let ((x-form x) (y-form y))
    (once-only (x y)
      `(unless (equal ,x ,y)
         (error 'assert-error :format-control "~@<The test ~s failed with ~s=~s and ~s=~s~:@>"
                              :format-arguments (list (list 'equal ',x-form ',y-form) ',x-form ,x ',y-form ,y))))))

(defun hash-table-alist (hash-table)
  (loop for key being the hash-keys of hash-table
        using (hash-value val)
        collect (cons key val)))

(defun alist-hash-table (alist &rest make-hash-table-args)
  (let ((tbl (apply #'make-hash-table make-hash-table-args)))
    (loop for (key . val) in alist
          do (setf (gethash key tbl) val))
    tbl))

(defun assoc-value (item alist &rest assoc-args)
  (cdr (apply #'assoc item alist assoc-args)))

(defun assoc-value-strict (item alist &rest assoc-args)
  (aif (apply #'assoc item alist assoc-args)
       (cdr it)
       (error "key not found in alist: ~s" item)))

(define-setf-expander assoc-value (item place &rest assoc-args &environment env)
  (multiple-value-bind (vars vals news writer reader) (get-setf-expansion place env)
    (assert (= 1 (length news)))
    (with-gensyms ((item-sym "ITEM") (alist-sym "ALIST") (it-sym "IT") (new-sym "NEW"))
      (values (append (list item-sym) vars (list alist-sym it-sym))
              (append (list item) vals (list reader `(assoc ,item-sym ,alist-sym ,@assoc-args)))
              `(,new-sym)
              `(if ,it-sym 
                   (setf (cdr ,it-sym) ,new-sym)
                   (let ((,(car news) (acons ,item-sym ,new-sym ,alist-sym)))
                     ,writer
                     ,new-sym))
              `(assoc-value ,item ,reader ,@assoc-args)))))

(defun (setf assoc-value-strict) (new item alist &rest assoc-args)
  (aif (apply #'assoc item alist assoc-args)
       (setf (cdr it) new)
       (error "key already in alist: ~s" item)))

(setf (fdefinition 'is) #'eq)
(setf (fdefinition '==) #'eql)
(setf (fdefinition '===) #'equal)
(setf (fdefinition '====) #'equalp)
(setf (macro-function '<-) (macro-function 'setf))

(defmacro -> (&rest args)
  (declare (ignore args))
  (error "Using -> macro outside of case/typecase context"))

(defmacro tagbody* (&body forms)
  "Same as TAGBODY, but returns the last non-label form's value if it is executed,
and NIL otherwise."
  (with-gensyms (name)
    `(block ,name
       (tagbody
          ,@(loop for cons on forms for form = (car cons)
                  collect (if (and (not (symbolp form))
                                   (loop for form in (cdr cons) always (symbolp form)))
                              `(return-from ,name ,form)
                              form))))))
