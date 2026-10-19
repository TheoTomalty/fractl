(in-package "FTX")

(defvar *grammar-id* 0)

(defstruct grammar
  (id (incf *grammar-id*) :type fixnum)
  (entrypoints nil :type list)
  (opcodes nil :type (array (unsigned-byte 8)))
  (args1   nil :type (simple-array t *))
  (args2   nil :type (simple-array t *))
  (match-potential-reductions (make-hash-table :test #'eq) :type hash-table))

(defvar *char-classes* (make-hash-table))
(defvar *ast-arguments*)

(defun int-map-combine (maps targets &key (to-int #'identity) (from-int #'identity) (min 0) (max most-positive-fixnum))
  (let ((transitions (loop for (x1 x2 . rest) in maps
                           for i from 1
                           append (let ((n1 (funcall to-int x1))
                                        (n2 (funcall to-int x2)))
                                    (assert (<= n1 n2))
                                    (list (list 'start n1 i)
                                          (list 'end (1+ n2) i))))))
    (push (list 'start min 0) transitions)
    (push (list 'end max 0) transitions)
    (setf transitions (sort transitions #'< :key #'second))
    (loop while transitions
          with start = min
          with active-set = nil
          append (let ((prev-active-set active-set)
                       (prev-start start)
                       (offset (second (car transitions))))
                   (loop for (type offset-next i) = (car transitions)
                         while (eql offset offset-next)
                         do (ecase type
                              (start
                               (push i active-set))
                              (end
                               (setf active-set
                                     (remove i active-set))))
                         do (setf transitions (cdr transitions)))
                   (setf active-set (sort (remove-duplicates active-set) #'<))
                   (labels ((targets (set)
                              (if set
                                  (remove-duplicates
                                   (funcall targets (mapcar #'1- (cdr set)))
                                   :test #'equal)
                                  nil)))
                     (let ((prev-targets (targets prev-active-set)))
                       (unless (null (set-exclusive-or (targets active-set) prev-targets :test #'equal))
                         (setf start offset)
                         (when (and (not (eql prev-start offset)) prev-targets)
                           (list (list* (funcall from-int (max prev-start min))
                                        (funcall from-int (1- offset))
                                        prev-targets))))))))))

(defun int-map-or (maps &rest keys)
  (apply #'int-map-combine maps
         (lambda (is)
           (loop for i in is
                 append (cddr (nth i maps))))
         keys))

(defun char-map-combine (maps target)
  (int-map-combine maps target :to-int #'char-code :from-int #'code-char :max char-code-limit))

(defun char-map-or (maps)
  (int-map-or maps :to-int #'char-code :from-int #'code-char :max char-code-limit))

(defun char-map-and (maps)
  (let ((all (loop for map in maps
                   for i from 0
                   collect i))
        (targets (cddr (first maps))))
    (loop for map in (rest maps)
          do (assert (equal targets (cddr map))))
    (char-map-combine maps (lambda (is)
                             (if (and is (equal is all))
                                 targets
                                 nil)))))

(defun char-map-invert (maps)
  (let ((targets (cddr (first maps))))
    (loop for map in (rest maps)
          do (assert (equal targets (cddr map))))
    (char-map-combine maps (lambda (is)
                             (if is
                                 nil
                                 targets)))))


(defun expand-char-class-entries (entries)
  (loop for entry in entries
        append (if (symbolp entry)
                   (or (gethash entry *char-classes*)
                       (error "no such char class ~s" entry))
                   (list entry))))

(defun char-class-entries-to-map (entries &optional (value t))
  (char-map-or
   (loop for entry in entries
         collect (etypecase entry
                   (character
                    (list entry entry value))
                   (cons
                    (destructuring-bind (first second) entry
                      (list first second value)))))))

(defun map-to-char-class-entries (map)
  (loop for (start end) in map
        collect (if (eql start end)
                    start
                    (list start end))))

(defun expand-char-class (positive negative)
  (setf positive (char-class-entries-to-map (expand-char-class-entries positive)))
  (setf negative (char-class-entries-to-map (expand-char-class-entries negative)))
  (map-to-char-class-entries
   (if positive
       (char-map-or
        (loop for maplet in positive
              append (char-map-invert
                      (char-map-or
                       (append (char-map-invert (list maplet)) negative)))))
       (char-map-invert negative))))

(defun grammar-compile-char-class (positive negative &key return trace trace-list)
  (let ((next-sym (make-symbol "NEXT")))
    `((,(if return 'i-char-r 'i-char)
       ,@(loop for (ch1 ch2 . rest) in (char-class-entries-to-map (expand-char-class positive negative) next-sym)
               collect (list* (char-code ch1) (char-code ch2) rest)))
      ,next-sym
      ,@(when trace
          `((,(if trace-list 'i-trace-list 'i-trace) ,trace))))))

(defun register-trace (trace)
  (let ((assoc (assoc trace *ast-arguments*)))
    (if assoc
        (assert (eql nil (cdr assoc)) () "Trace already declared as list.")
        (push (cons trace nil) *ast-arguments*))))

(defun register-trace-list (trace)
  (let ((assoc (assoc trace *ast-arguments*)))
    (if assoc
        (assert (eql t (cdr assoc)) () "Trace already declared as variable.")
        (push (cons trace t) *ast-arguments*))))

(defun grammar-compile-expr (expr &key (stack-top 0) bindings blocks tags return)
  (macrolet ((recurse (expr &key (stack-top 'stack-top)
                              (bindings 'bindings)
                              (blocks 'blocks)
                              (tags 'tags)
                              (return 'return))
               `(grammar-compile-expr ,expr
                                      :stack-top ,stack-top
                                      :bindings ,bindings
                                      :blocks ,blocks
                                      :tags ,tags
                                      :return ,return))
             (lookup (sym)
               `(or (cdr (assoc ,sym bindings))
                    (error "variable not in scope ~s" ,sym))))
    (typecase expr
      ((or boolean character integer)
       (when return
         `((i-push ,expr))))
      (symbol
       (when return
         (if (special-variable-p expr)
             `((i-dyn-get ,expr))
             `((i-peek ,(lookup expr))))))
      (cons
       (case (first expr)
         ((char-class)
          (grammar-compile-char-class (second expr) (third expr) :return return))
         ((char-class-trace)
          (register-trace (second expr))
          (grammar-compile-char-class (third expr) (fourth expr) :return return :trace (second expr)))
         ((char-class-trace-list)
          (register-trace-list (second expr))
          (grammar-compile-char-class (third expr) (fourth expr) :return return :trace (second expr) :trace-list t))
         ((eof eof-trace eof-trace-list any any-trace any-trace-list)
          (let ((next (make-symbol "NEXT"))
                any trace trace-list)
            (setf (values any trace trace-list)
                  (ecase (first expr)
                    (eof (values nil nil nil))
                    (eof-trace (values nil t nil))
                    (eof-trace-list (values nil nil t))
                    (any (values t nil nil))
                    (any-trace (values t t nil))
                    (any-trace-list (values t nil t))))
            (list* (list 'i-char (if any (list 0 char-code-limit next) (list -1 -1 next)))
                   next
                   (cond (trace
                          (register-trace (second expr))
                          (list (list 'i-trace (second expr))))
                         (trace-list
                          (register-trace-list (second expr))
                          (list (list 'i-trace-list (second expr))))))))
         ((asm)
          (append (cdr expr)
                  (when return
                    (list `(i-push nil)))))
         ((tagbody)
          (let ((tag-map (make-hash-table)))
            (loop for e in (cdr expr)
                  when (symbolp e)
                    do (setf (gethash e tag-map) (cons (gensym "TAG") stack-top)))
            (append
             (loop for e in (cdr expr)
                   append (if (symbolp e)
                              (list (car (gethash e tag-map)))
                              (recurse e :tags (cons tag-map tags) :return nil)))
             (when return
               (list `(i-push nil))))))
         ((go)
          (loop for tag-map in tags
                do (multiple-value-bind (val found)
                       (gethash (cadr expr) tag-map)
                     (when found
                       (return (list `(i-drop ,(- stack-top (cdr val)))
                                     `(i-br ,(car val))))))
                finally (error "tag not found: ~s" (cadr expr))))
         ((block)
          (let ((end-sym (gensym "BLOCK")))
            (append
             (recurse (cons 'progn (cddr expr))
                      :blocks (cons `(,(second expr) ,end-sym ,stack-top ,return)
                                    blocks))
             (list end-sym))))
         ((return-from)
          (destructuring-bind (end-sym stack-position return)
              (cdr (or (assoc (second expr) blocks)
                       (error "block not found: ~s" (cadr expr))))
            (append
             (recurse (third expr) :return return)
             (list (if return
                       `(i-collapse ,(- stack-top stack-position))
                       `(i-drop ,(- stack-top stack-position)))
                   `(i-br ,end-sym)))))
         ((progn)
          (loop for exprs = (cdr expr) then (cdr exprs)
                while exprs
                append (if (cdr exprs)
                           (recurse (car exprs) :return nil)
                           (recurse (car exprs)))))
         ((alt)
          (let* ((exprs (cdr expr))
                 (next-syms (loop for expr in (cdr exprs)
                                  collect (gensym "NEXT")))
                 (end-sym (gensym "END")))
            (loop for expr in exprs
                  for next-sym = (car next-syms)
                  do (pop next-syms)
                  append (if next-sym
                             `((i-fork ,next-sym)
                               ,@(recurse expr)
                               (i-br ,end-sym)
                               ,next-sym)
                             `(,@(recurse expr)
                               ,end-sym)))))
         ((many)
          (let ((begin-sym (gensym "BEGIN"))
                (end-sym (gensym "END")))
            (if return
                `((i-push nil)
                  ,begin-sym
                  (i-fork ,end-sym)
                  ,@(recurse (cons 'progn (cdr expr))
                             :stack-top (1+ stack-top))
                  (i-swap)
                  (i-call-foreign 2 cons)
                  (i-br ,begin-sym)
                  ,end-sym
                  (i-call-foreign 1 reverse))
                `(,begin-sym
                  (i-fork ,end-sym)
                  ,@(recurse (cons 'progn (cdr expr)))
                  (i-br ,begin-sym)
                  ,end-sym))))
         ((more)
          (let ((begin-sym (gensym "BEGIN")))
            (if return
                `((i-push nil)
                  ,begin-sym
                  ,@(recurse (cons 'progn (cdr expr))
                             :stack-top (1+ stack-top))
                  (i-swap)
                  (i-call-foreign 2 cons)
                  (i-fork ,begin-sym)
                  (i-call-foreign 1 reverse))
                `(,begin-sym
                  ,@(recurse (cons 'progn (cdr expr)))
                  (i-fork ,begin-sym)))))
         ((maybe)
          (let ((end-sym (gensym "END"))
                (next-sym (gensym "NEXT")))
            (if return
                `((i-fork ,next-sym)
                  ,@(recurse (cons 'progn (cdr expr)))
                  (i-br ,end-sym)
                  ,next-sym
                  (i-push nil)
                  ,end-sym)
                `((i-fork ,end-sym)
                  ,@(recurse (cons 'progn (cdr expr)))
                  ,end-sym))))
         ((reject)
          (let ((next-sym (gensym "NEXT")))
            (if return
                `((i-fork-ord ,next-sym)
                  ,@(recurse (cons 'progn (cdr expr)))
                  (i-negate)
                  ,next-sym
                  (i-push nil))
                `((i-fork-ord ,next-sym)
                  ,@(recurse (cons 'progn (cdr expr)))
                  (i-negate)
                  ,next-sym))))
         ((peek)
          `((i-push-offset)
            ,@(recurse (cons 'progn (cdr expr))
                       :stack-top (+ stack-top 2))
            ,@(when return
                `((i-nrot)))
            (i-pop-offset)))
         ((capt)
          (assert return)
          `((i-push-offset)
            ,@(recurse (cons 'progn (cdr expr))
                       :stack-top (+ stack-top 2)
                       :return nil)
            (i-push-offset)
            (i-capture)))
         ;; ((capture)
         ;;  (assert return)
         ;;  `(,@(recurse (cadr expr))
         ;;    ,@(recurse (caddr expr) :stack-top (1+ stack-top))
         ;;    (i-capture)))
         ((intersperse)
          (let ((body-sym (gensym "BODY"))
                (separator-sym (gensym "SEPARATOR")))
            (if return
                `((i-push nil)
                  (i-br ,body-sym)
                  ,separator-sym
                  ,@(recurse (third expr) :stack-top (1+ stack-top) :return nil)
                  ,body-sym
                  ,@(recurse (second expr) :stack-top (1+ stack-top))
                  (i-swap)
                  (i-call-foreign 2 cons)
                  (i-fork ,separator-sym))
                `((i-br ,body-sym)
                  ,separator-sym
                  ,@(recurse (third expr))
                  ,body-sym
                  ,@(recurse (second expr))
                  (i-fork ,separator-sym)))))
         ((match match-trace match-trace-list)
          (let ((trace (case (first expr) ((match-trace match-trace-list) (second expr))))
                (trace-list (case (first expr) (match-trace-list t)))
                (args (case (first expr) (match (cdr expr)) (t (cddr expr)))))
            (when trace
              (if trace-list (register-trace-list trace) (register-trace trace)))
            (etypecase (first args)
              (symbol
               (aif (get (first args) 'rule-macro)
                    (recurse (funcall it))
                    (progn
                      (aif (get (first args) 'group-info)
                           (assert (= (second it) (length (cdr args))) nil "Inconsistent number of arguments to group ~s" (first args))
                           (setf (get (first args) 'group-info) (list :implicit (length (cdr args)))))
                      `(,@(loop for arg in (rest args)
                                for new-top = stack-top then (1+ new-top)
                                append (recurse arg :stack-top new-top :return t))
                        ,@(let ((length (length (rest args)))
                                (symbol (first args)))
                            `((.extern ,symbol)
                              (i-call ,length ,symbol)
                              (,(if return 'i-match-r 'i-match) ,length ,symbol)
                              ,@(when trace
                                  `((,(if trace-list 'i-trace-list 'i-trace) ,trace)))))))))
              (string
               `(,@(recurse
                    `(progn ,@(loop for ch across (first args)
                                    collect `(char-class (,ch)))))
                 ,@(when trace
                     `((,(if trace-list 'i-trace-push-list 'i-trace-push) ,trace ,(first args))))))
              (character
               (recurse `(,(ecase (first expr)
                             (match 'char-class)
                             (match-trace 'char-class-trace)
                             (match-trace-list 'char-class-trace-list))
                          ,@(when trace (list trace))
                          (,(first args))))))))
         ((trace-push)
          (register-trace (second expr))
          `((i-trace-push ,(second expr) ,(third expr))
            ,@(when return
                `((i-push nil)))))
         ((trace-push-list)
          (register-trace-list (second expr))
          `((i-trace-push-list ,(second expr) ,(third expr))
            ,@(when return
                `((i-push nil)))))
         ((advance)
          (if (integerp (second expr))
              `((i-advance ,(second expr))
                ,@(when return
                    `((i-push nil))))
              `(,@(recurse (second expr) :return t)
                (i-advance-dyn)
                ,@(when return
                    `((i-push nil))))))
         ((stop)
          `((i-stop)))
         ;; ((offset)
         ;;  (when return
         ;;    `((i-push-offset))))
         ((set-offset)
          `(,@(recurse (cadr expr) :return t)
            ,@(when return
                `((i-dup)))
            (i-pop-offset)))
         ((let)
          (let ((nargs (length (cadr expr))))
            `(,@(loop for (name expr) in (cadr expr)
                      for new-top = stack-top then (1+ new-top)
                      append (recurse expr :stack-top new-top :return t))
              ,@(recurse (cons 'progn (cddr expr))
                         :stack-top (+ nargs stack-top)
                         :bindings (nconc
                                    (loop for i from stack-top
                                          for (name) in (cadr expr)
                                          collect (cons name i))
                                    bindings))
              ,@(when (< 0 nargs)
                  (if return
                      `((i-collapse ,nargs))
                      `((i-drop ,nargs)))))))
         ((let*)
          (let ((nargs (length (cadr expr)))
                (bindings bindings))
            `(,@(loop for (name expr) in (cadr expr)
                      for new-top = stack-top then (1+ new-top)
                      append (recurse expr
                                      :stack-top new-top
                                      :bindings bindings
                                      :return t)
                      do (push (cons name new-top) bindings))
              ,@(recurse `(progn ,@(cddr expr))
                         :stack-top (+ nargs stack-top)
                         :bindings bindings)
              ,@(when (< 0 nargs)
                  (if return
                      `((i-collapse ,nargs))
                      `((i-drop ,nargs)))))))
         ((quote)
          (when return
            `((i-push ,(second expr)))))
         ((if)
          (let ((else-sym (gensym "ELSE"))
                (end-sym (gensym "END")))
            `(,@(recurse (second expr) :return t)
              (i-bz ,else-sym)
              ,@(recurse (third expr))
              (i-br ,end-sym)
              ,else-sym
              ,@(recurse (fourth expr))
              ,end-sym)))
         ((setq)
          (let ((sym (second expr)))
            (check-type sym symbol)
            `(,@(recurse (third expr) :return t)
              ,@(when return
                  `((i-dup)))
              ,(if (special-variable-p sym)
                   `(i-dyn-set ,sym)
                   `(i-poke ,(lookup (second expr)))))))
         ((the sb-kernel:the* sb-ext:truly-the)
          (recurse (third expr)))
         ((macrolet)
          (recurse `(progn ,@(cddr (sb-walker:macroexpand-all expr)))))
         ((symbol-macrolet)
          (recurse `(progn ,@(cddr expr))))
         ((declare)
          (when return
            `((i-push nil))))
         ((trace-range)
          (register-trace (second expr))
          `((i-trace-range ,(second expr))
            ,@(when return
                `((i-push nil)))))
         ((trace-range-list)
          (register-trace-list (second expr))
          `((i-trace-range-list ,(second expr))
            ,@(when return
                `((i-push nil)))))
         (t
          (assert (symbolp (first expr)))
          (when (special-operator-p (first expr))
            (error "unsupported special operator in ~s" expr))
          (if (macro-function (first expr))
              (recurse (macroexpand expr))
              `(,@(loop for arg in (cdr expr)
                        for new-top = stack-top then (1+ new-top)
                        append (recurse arg :stack-top new-top :return t))
                (i-call-foreign ,(length (cdr expr)) ,(first expr))
                ,@(unless return
                    `((i-pop))))))))
      (t
       (when return
         `((i-push ,expr)))))))

(defun grammar-compile-body (name groups args body &key return trace-return)
  (declare (type (member i-reduce i-reduce-r) return))
  `((.extern ,@groups)
    ,@groups
    ,@(grammar-compile-expr
       `(block ,name ,@body)
       :stack-top (length args)
       :bindings (loop for arg in args
                       for i from 0
                       collect (cons arg i))
       :return (eql 'i-reduce-r return))
    ,@(when (or *ast-arguments* trace-return)
        `((i-trace-return ,name)))
    ,@(loop for group in groups collect (list return (length args) group))
    (i-stop)))

(defun %define-chars (name positive negative)
  (setf (gethash name *char-classes*) (expand-char-class (coerce positive 'list) (coerce negative 'list))))

(defmacro define-chars (name positive &optional negative)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-chars ',name ',positive ',negative)))

(defvar *cache* nil)

(defmacro define-accessor (name lambda-list expr)
  `(progn
     (defun ,name ,lambda-list ,expr)
     (defun (setf ,name) (new ,@lambda-list) (setf ,expr new))))

(defun get-strict (symbol indicator)
  (let ((ret (get symbol indicator '#0=#.(make-symbol "UNBOUND-MARKER"))))
    (if (eq ret '#0#)
        (error "Indicator ~s not found in symbol-plist of ~s." indicator symbol)
        ret)))

(defun (setf get-strict) (new symbol indicator)
  (setf (get symbol indicator) new))

(define-accessor rule-insts (symbol) (get-strict symbol 'rule-insts))
(define-accessor rule-groups (symbol) (get symbol 'rule-groups))
(define-accessor rule-labels (symbol) (get symbol 'rule-labels))
(define-accessor label-addrs (symbol) (get symbol 'label-addrs))
(define-accessor group-rules (symbol) (get symbol 'group-rules))

(defvar *debug-evm* nil)

(defgeneric evm-run (text grammar &key start symbol))
(defgeneric eval-ast (ast-node &optional evaluator))
(defstruct (ast (:constructor nil) (:copier nil)))

(defun ast-variable (node var)
  (multiple-value-bind (rule foundp) (ast-rule node)
    (assert foundp nil "Node is not traced")
    (assert (member var (get rule 'rule-params) :key #'car) nil
            "Node of type ~s does not have ~s as a parameter" rule var))
  (eval-ast node
            (lambda ()
              (when (boundp var) (symbol-value var)))))

(define-condition evm-parse-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "parse error"))))

(defstruct (string-text (:constructor %make-string-text))
  (grammar-id nil :type (or null fixnum))
  (string nil :type string)
  (states nil :type simple-vector))

(defun make-string-text (string)
  (%make-string-text :string string
                     :states (make-array (* (1+ (length string)) 3)
                                         :initial-element nil)))

(defun string-text-clear-state (text)
  (setf (string-text-states text) (make-array (* (1+ (length (string-text-string text))) 3)
                                              :initial-element nil)
        (string-text-grammar-id text) nil))

(defun evm-run-strict (text grammar &key (start 0))
  (multiple-value-bind (ast parsed-p) (evm-run text grammar :start start)
    (unless parsed-p
      (error 'evm-parse-error))
    ast))

(defun evm-parse (text grammar &key (start 0))
  (eval-ast (evm-run-strict text grammar :start start)))

(defun evm-eval (text grammar)
  (let ((repeatable-text (if (stringp text) (make-string-text text) text)))
    (labels ((compute-pos (ast)
               (let ((ret (ast-origin ast)))
                 (etypecase ret
                   (integer ret)
                   (point (point-pos ret)))))
             (run-eval (ast)
               (let ((form (eval-ast ast)))
                 (with-compilation-unit (:source-plist (list :fed-pos (compute-pos ast)))
                   (eval form)))))
      (loop with i = 0 and prev-ast = nil and first-p = t
            do (let* ((node (evm-run-strict repeatable-text grammar :start i))
                      (ast (eval-ast node)))
                 (if ast
                     (progn
                       (if first-p
                           (setf first-p nil)
                           (run-eval prev-ast))
                       (setf prev-ast ast)
                       (incf i (ast-length node)))
                     (return (if first-p (values) (run-eval prev-ast)))))))))

(defun evm-load (filespec grammar)
  (let ((*package* *package*))
    (with-compilation-unit (:source-namestring (namestring filespec)
                            :allow-other-keys t)
      (evm-eval (read-file-string filespec) grammar))))

(defun eval-ast-arg (arg)
  (if (ast-p arg) (eval-ast arg) arg))

(defun eval-ast-arg-list (arg)
  (loop for cell on arg do (setf (car cell) (eval-ast-arg (car cell))))
  arg)

(defun invalidate-groups (groups)
  (setf *cache*
        (delete-if (lambda (entry)
                     (loop for group in groups thereis (member group (cddr entry))))
                   *cache*)))

(defun symbol-map-changed-p (name symbol-map)
  (null (set-difference (loop for symbol being the hash-keys of symbol-map
                                using (hash-value ips)
                              nconc (when (and (symbolp symbol) (get symbol 'label-extern))
                                      (loop for ip in ips collect (cons symbol ip))))
                        (loop for label in (rule-labels name)
                              nconc (loop for (rule . ip) in (label-addrs label)
                                          when (eql name rule)
                                            collect (cons label ip)))
                        :test #'equal)))

(defun %define-syntax (name groups nargs insts ast-arguments trace)
  (loop for group in groups
        do (when (get group 'rule-macro)
             (warn "Rule group ~s is already defined as a macro" group))
           (setf (get group 'group-info) (list t nargs)))
  (multiple-value-bind (insts symbol-map) (grammar-assemble insts)
    (unless (and (equalp insts (get name 'rule-insts '#:unbound))
                 (equalp groups (rule-groups name))
                 (symbol-map-changed-p name symbol-map))
      (invalidate-groups groups)
      (rmakunbound name)
      (setf (rule-insts name) insts)
      (loop for symbol being the hash-keys of symbol-map
              using (hash-value ips)
            do (when (and (symbolp symbol) (get symbol 'label-extern))
                 (loop for i in ips do (push (cons name i) (label-addrs symbol)))
                 (pushnew symbol (rule-labels name))))
      (loop for group in groups
            do (pushnew group (rule-groups name))
               (pushnew name (group-rules group))))
    (setf (get name 'rule-default-evaluator) trace
          (get name 'rule-params) ast-arguments)
    name))

(defmacro define-syntax (name-and-options &body body)
  (if (and (stringp (first body)) (cdr body)) (pop body))
  (destructuring-bind (name &key args group (trace nil trace-p) (trace-lazy nil trace-lazy-p) return)
      (if (consp name-and-options) name-and-options (list name-and-options))
    (assert (or (not trace) (not trace-lazy)) () "Can specify TRACE or TRACE-LAZY but not both.")
    (let ((*ast-arguments* nil)
          (groups (cond ((consp group) group) ((null group) (list name)) (t (list group)))))
      `(%define-syntax ',name ',groups ,(length args)
                     ',(grammar-compile-body name groups args body
                                             :return (if return 'i-reduce-r 'i-reduce)
                                             :trace-return (or trace-p trace-lazy-p))
                     ',(setf *ast-arguments* (nreverse *ast-arguments*))
                     ,(when (or trace-p trace-lazy-p *ast-arguments*)
                        `(lambda ()
                           (declare (special ,@(mapcar #'car *ast-arguments*)))
                           (let ,(loop for (arg) in *ast-arguments* collect `(,arg (when (boundp ',arg) ,arg)))
                             (declare (special ,@(mapcar #'car *ast-arguments*)))
                             ,@(unless trace-lazy
                                 (loop for (arg . listp) in *ast-arguments*
                                       collect (if listp
                                                   `(setf ,arg (eval-ast-arg-list ,arg))
                                                   `(setf ,arg (eval-ast-arg ,arg)))))
                             ,(cond (trace-p trace)
                                    (trace-lazy-p trace-lazy)))))))))

(defun %define-syntax-macro (name macro-function)
  (ecase (first (get name 'group-info))
    ((t) (error "Rule macro ~s is already declared as a group" name))
    (:implicit (error "Rule macro ~s is already implicitly assumed to be a group" name))
    ((nil)))
  (setf (get name 'rule-macro) macro-function))

(defmacro define-syntax-macro (name &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-syntax-macro ',name (lambda () '(progn ,@body)))))

(defun %define-syntax-group (name)
  (setf (get name 'group-info) (list t 0)))

(defmacro define-syntax-group (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-syntax-group ',name)))

(defun %define-evaluator (indicator rule name evaluator constantp)
  (aif (assoc name (get rule indicator))
       (setf (cdr it) (list evaluator constantp))
       (push (list name evaluator constantp) (get rule indicator))))

(defmacro define-rule-evaluator (rule name &body body)
  (assert (get rule 'rule-default-evaluator) nil "Rule ~s is not traced" rule)
  (let* ((form (prognify body))
         (constantp (constantp form)))
    `(%define-evaluator 'rule-evaluators ',rule ',name
                        ,(if constantp
                             form
                             `(lambda () (declare (special ,@(mapcar #'car (get rule 'rule-params)))) ,@body))
                        ,constantp)))

(defmacro define-group-evaluator (group name &body body)
  (let* ((form (prognify body))
         (constantp (constantp form)))
    `(%define-evaluator 'group-evaluators ',group ',name
                        ,(if constantp form `(lambda () ,@body))
                        ,constantp)))

(defmacro lambda-grammar (options &body body)
  (let ((name (make-symbol "ANONYMOUS-RULE")))
    `(or (get ',name 'lambda-grammar)
         (progn
           (define-syntax (,name ,@options) ,@body)
           (setf (get ',name 'lambda-grammar) (compile-grammar '(,name)))))))

(defun rule-p (name)
  (declare (type symbol name))
  (not (eq '#0=#.(make-symbol "UNBOUND-MARKER") (getf (symbol-plist name) 'rule-insts '#0#))))

(defun rmakunbound (name)
  (when (rule-p name)
    (let ((groups (rule-groups name))
          (labels (rule-labels name)))
      (invalidate-groups groups)
      (remprop name 'rule-insts)
      (remprop name 'rule-groups)
      (remprop name 'rule-labels)
      (remprop name 'rule-default-evaluator)
      (remprop name 'rule-params)
      (loop for group in groups
            do (setf (group-rules group) (delete name (group-rules group))))
      (loop for label in labels
            do (setf (label-addrs label) (delete name (label-addrs label) :key #'car)))))
  name)

(defun gmakunbound (name)
  (loop for rule in (copy-list (get name 'group-rules))
        do (rmakunbound rule))
  (remprop name 'group-info)
  (remprop name 'rule-macro))

(defun set-branches (inst symbol-map)
  (labels ((expand-symbols (syms)
             (loop for sym in syms
                   append (let ((value (gethash sym symbol-map)))
                            (append (when (and (symbolp sym) (get sym 'label-extern))
                                      (list sym))
                                    value))))
           (expand-maps (maps &key (numkeys 0))
             (loop for map in maps
                   append (typecase map
                            (cons (list (append (subseq map 0 numkeys)
                                                (expand-symbols (nthcdr numkeys map)))))
                            (t (list map))))))
    (case (first inst)
      ((i-call)
       (list* (first inst) (second inst) (expand-symbols (cddr inst))))
      ((i-fork i-fork-ord i-bz i-br)
       (list* (first inst) (expand-symbols (rest inst))))
      ((i-match i-match-r)
       (list* (first inst) (second inst) (expand-maps (cddr inst) :numkeys 1)))
      ((i-char i-char-r)
       (list* (first inst) (expand-maps (rest inst) :numkeys 2)))
      (t inst))))

(defun get-branches (inst)
  (case (first inst)
    (i-call (values t (cddr inst)))
    ((i-match i-match-r)
     (let ((ret nil)
           (branches nil))
       (loop for char in (cddr inst)
             do (if (characterp char)
                    (setf ret t)
                    (setf branches (union branches (cdr char)))))
       (values ret branches)))
    ((i-char i-char-r)
     (let ((ret nil)
           (branches nil))
       (loop for sym in (cdr inst)
             do (if (symbolp sym)
                    (setf ret t)
                    (setf branches (union branches (cddr sym)))))
       (values ret branches)))
    (i-stop (values nil nil))
    ((i-fork i-fork-ord i-bz) (values t (cdr inst)))
    (i-br (values nil (cdr inst)))
    (t (values t nil))))

(defun grammar-assemble (insts)
  (let* ((local-symbols (make-hash-table))
         (ret (make-array 16 :adjustable t :fill-pointer 0)))
    (loop for inst in insts
          do (etypecase inst
               (cons
                (case (first inst)
                  (.extern
                   (loop for label in (rest inst)
                         do (setf (get label 'label-extern) t)))
                  (t (vector-push-extend inst ret))))
               ((or symbol integer)
                ;; TODO: Good idea to use integers as jump labels (conflicts with interpretation as fixed offset)?
                (let ((i (length ret)))
                  (pushnew i (gethash inst local-symbols))))))
    (loop for inst across ret
          for i from 0
          when (consp inst)
            do (setf (aref ret i) (set-branches inst local-symbols)))
    (values (coerce ret 'simple-vector)
            local-symbols)))

(defvar *groups-reached*)

(defun optimise-unify (symbols)
  (let ((epsilons (make-hash-table :test 'equal))
        (epsilons-entry (make-hash-table :test 'equal)))
    (labels ((addr1+ (addr)
               (cons (car addr) (1+ (cdr addr))))
             (addr< (a1 a2)
               (if (eql (car a1) (car a2))
                   (< (cdr a1) (cdr a2))
                   (string< (symbol-name (car a1)) (symbol-name (car a2)))))
             (addrjmp (addr i)
               (declare (type integer i))
               (cons (car addr) i))
             (addrjmps (addr is)
               (loop for i in is
                     append (etypecase i
                              (symbol
                               (assert (eql t (first (get i 'group-info))) nil "Grammar non-terminal ~s is unbound" i)
                               (let ((addrs (label-addrs i)))
                                 (loop for (sym . i) in addrs
                                       do (loop for group in (rule-groups sym)
                                                do (pushnew group *groups-reached*)))
                                 addrs))
                              (integer
                               (list (addrjmp addr i))))))
             (addr-get (addr)
               (aref (rule-insts (car addr)) (cdr addr)))
             (%closure (addrs called visited call next)
               (loop for addr in addrs
                     unless (or (gethash addr visited)
                                (progn (setf (gethash addr visited) t) nil))
                       do (flet ()
                            (let ((inst (addr-get addr)))
                              (case (car inst)
                                (i-call
                                 (when called
                                   (assert (<= (second inst) called)))
                                 (%closure (list (addr1+ addr)) called visited call next)
                                 (%closure (addrjmps addr (cddr inst)) (second inst) visited call next))
                                (i-fork
                                 (%closure (cons (addr1+ addr) (addrjmps addr (rest inst))) called visited call next))
                                (i-br
                                 (%closure (addrjmps addr (rest inst)) called visited call next))
                                (i-noop
                                 (%closure (list (addr1+ addr)) called visited call next))
                                (i-stop)
                                (t (if called
                                       (setf (gethash (cons called addr) call) t)
                                       (setf (gethash addr next) t))))))))
             (closure (addrs &key called)
               (let ((visited (make-hash-table :test 'equal))
                     (call (make-hash-table :test 'equal))
                     (next (make-hash-table :test 'equal)))
                 (%closure addrs called visited call next)
                 (values (sort (loop for addr being the hash-keys of next
                                     collect addr)
                               #'addr<)
                         (loop for (nargs . addr) being the hash-keys of call
                               with map = (make-hash-table)
                               do (pushnew addr (gethash nargs map))
                               finally (return (loop for nargs being the hash-keys of map
                                                       using (hash-value addrs)
                                                     collect (cons nargs (sort addrs #'addr<))))))))
             (merge-instructions (addrs)
               (loop for addr in addrs
                     for inst = (addr-get addr)
                     with chars = nil
                     with chars-r = nil
                     with matches = nil
                     with matches-r = nil
                     with bzs = nil
                     with inst-map = (make-hash-table :test 'equal)
                     do (case (first inst)
                          (i-char (setf chars (append (loop for char in (rest inst) collect (list* (first char) (second char) (addrjmps addr (cddr char)))) chars)))
                          (i-char-r (setf chars-r (append (loop for char in (rest inst) collect (list* (first char) (second char) (addrjmps addr (cddr char)))) chars-r)))
                          ((i-match i-match-r)
                           (loop with tbl = (let ((matches* (ecase (first inst) (i-match matches) (i-match-r matches-r))))
                                              (cdr (or (assoc (second inst) matches*)
                                                       (first (let ((new (list* (cons (second inst) (make-hash-table)) matches*)))
                                                                (ecase (first inst) (i-match (setf matches new)) (i-match-r (setf matches-r new))))))))
                                 for match in (cddr inst)
                                 do (multiple-value-bind (sym addrs) (if (symbolp match) (values match (list (addr1+ addr))) (values (first match) (rest match)))
                                      (loop for addr in addrs
                                            do (push addr (gethash sym tbl))))))
                          (i-bz (push (cons (addr1+ addr) (addrjmps addr (rest inst))) bzs))
                          (i-stop)
                          (i-fork-ord
                           (push (addr1+ addr)
                                 (gethash `(i-fork-ord ,(get-epsilon-entry (addrjmps addr (rest inst))))
                                          inst-map)))
                          (t (push (addr1+ addr) (gethash inst inst-map))))
                     finally (return
                               (let* ((chars
                                        (loop for char in (int-map-or chars :min -1 :max char-code-limit)
                                              collect `(,(first char)
                                                        ,(second char)
                                                        ,(get-epsilon-entry (cddr char)))))
                                      (chars-r
                                        (loop for char in (int-map-or chars-r :min -1 :max char-code-limit)
                                              collect `(,(first char)
                                                        ,(second char)
                                                        ,(get-epsilon-entry (cddr char)))))
                                      (matches
                                        (loop for (n . tbl) in matches
                                              collect (list
                                                       (list* 'i-match n
                                                              (loop for match being the hash-keys of tbl
                                                                      using (hash-value addrs)
                                                                    collect `(,match ,(get-epsilon-entry addrs)))))))
                                      (matches-r
                                        (loop for (n . tbl) in matches-r
                                              collect (list
                                                       (list* 'i-match-r n
                                                              (loop for match being the hash-keys of tbl
                                                                      using (hash-value addrs)
                                                                    collect `(,match ,(get-epsilon-entry addrs)))))))
                                      (bzs
                                        (when bzs
                                          `(((i-bz ,(get-epsilon-entry (apply #'append (mapcar #'cdr bzs))))
                                             (i-br ,(get-epsilon-entry (mapcar #'car bzs)))))))
                                      (rest
                                        (loop for inst being the hash-keys of inst-map
                                                using (hash-value addrs)
                                              collect `(,inst
                                                        (i-br ,(get-epsilon-entry addrs)))))
                                      (groups
                                        `(,@(when chars
                                              `(((i-char ,@chars))))
                                          ,@(when chars-r
                                              `(((i-char-r ,@chars-r))))
                                          ,@matches
                                          ,@matches-r
                                          ,@bzs
                                          ,@rest)))
                                 (case (length groups)
                                   (0 `((i-stop)))
                                   (1 (first groups))
                                   (t (let ((syms (loop for i from 1 below (length groups)
                                                        collect (gensym "FORK"))))
                                        (append `((i-fork ,@syms))
                                                (first groups)
                                                (loop for group in (rest groups)
                                                      for sym in syms
                                                      append `(,sym ,@group))))))))))
             (get-epsilon (addrs)
               (if (gethash addrs epsilons)
                   (first (gethash addrs epsilons))
                   (let* ((symbol (gensym "EPSILON"))
                          (list (list symbol)))
                     (setf (gethash addrs epsilons) list)
                     (setf (cdr list) (merge-instructions addrs))
                     symbol)))
             (get-epsilon-entry (addrs)
               (multiple-value-bind (next-addrs call-addrs) (closure addrs)
                 (let ((key (list next-addrs call-addrs)))
                   (if (gethash key epsilons-entry)
                       (first (gethash key epsilons-entry))
                       (let* ((symbol (gensym "EPSILON"))
                              (list (list symbol)))
                         (setf (gethash key epsilons-entry) list)
                         (setf (cdr list)
                               `(,@(loop for (nargs . addrs) in call-addrs
                                         collect `(i-call ,nargs ,(get-epsilon addrs)))
                                 ,(if next-addrs
                                      `(i-br ,(get-epsilon next-addrs))
                                      `(i-stop))))
                         symbol))))))
      (multiple-value-bind (i-brs entrypoints)
          (loop for symbol in symbols and i from 0
                collect `(i-br ,(get-epsilon-entry (addrjmps nil (list symbol)))) into i-brs
                collect (cons symbol i) into entrypoints
                finally (return (values i-brs entrypoints)))
        (values
         (grammar-assemble
          (append
           i-brs
           (loop for insts being the hash-values of epsilons-entry
                 append insts)
           (loop for insts being the hash-values of epsilons
                 append insts)))
         entrypoints)))))

(defun optimise-breaks (insts entrypoints)
  (let* (;; An array where each element represents the ultimate
         ;; instruction in the i-br chain of the corresponding
         ;; instruction in the insts array.
         (indirection (make-array (length insts)))
         ;; A hash-table which maps the indexes of non-i-br
         ;; instructions to a list of indexes that jump to it
         ;; (including itself).  It is the multi-inverse of the
         ;; indirection array.
         (links (make-hash-table))
         (ret (make-array (length insts) :adjustable t :fill-pointer 0)))
    ;; Initialise indirection and links
    (loop for inst across insts
          for i from 0
          do (let ((ind (list i)))
               (loop while (eql 'i-br (first (aref insts (car ind))))
                     do (destructuring-bind (j) (rest (aref insts (car ind)))
                          (push j ind)))
               (setf (aref indirection i) (car ind))
               (loop for ip in ind
                     do (pushnew ip (gethash (car ind) links)))))
    ;; Populate ret
    (let ((completed (make-hash-table)))
      (loop named outer
            with pending = (mapcar #'cdr entrypoints) and i = nil
            do (unless i
                 (loop while pending
                       do (let ((ip (aref indirection (pop pending))))
                            (unless (gethash ip completed)
                              (setf i ip)
                              (return)))
                       finally (return-from outer)))
               (assert i)
               (if (gethash i completed)
                   (progn
                     (vector-push-extend `(i-br ,i) ret)
                     (setf i nil))
                   (progn
                     (setf (gethash i completed) t)
                     (loop for link in (gethash i links)
                           do (vector-push-extend link ret))
                     (vector-push-extend (aref insts i) ret)
                     (multiple-value-bind (fallthrough branches)
                         (get-branches (aref insts i))
                       (setf pending (append pending branches))
                       (setf i (when fallthrough
                                 (aref indirection (1+ i))))))))
      (multiple-value-bind (ret int-map) (grammar-assemble (coerce ret 'list))
        (values ret
                (loop for (name . ip) in entrypoints
                      collect (destructuring-bind (mapped-ip) (gethash ip int-map)
                                (cons name mapped-ip))))))))

(deftype u32 () `(unsigned-byte 32))
(deftype ip-array () `(vector u32 *))

(defun encode-integer-list (is)
  (the ip-array (make-array (length is) :element-type '(unsigned-byte 32) :initial-contents is)))

(defun encode-symbol-list (syms)
  (the simple-vector (make-array (length syms) :element-type 'symbol :initial-contents syms)))

(defun encode-list (list)
  (the simple-vector (make-array (length list) :element-type 'symbol :initial-contents list)))

(defun encode-char-map (maps)
  (let ((tbl (make-hash-table))
        (ranges (make-array 10 :adjustable t :fill-pointer 0))
        (last-code2 nil))
    (loop for (code1 code2 . ips) in maps
          do (let ((encoded-ips (encode-list ips)))
               (assert (and (<= code1 code2) (or (null last-code2) (< last-code2 code1))))
               (setf last-code2 code2)
               (if (< (1+ (- code2 code1)) 5)
                   (loop for i from code1 to code2 do (setf (gethash i tbl) encoded-ips))
                   (progn
                     (vector-push-extend code1 ranges)
                     (vector-push-extend code2 ranges)
                     (vector-push-extend encoded-ips ranges)))))
    (values tbl (coerce ranges 'simple-vector))))

(defun encoded-char-map-ips (tbl ranges code)
  (let ((range-size 3))
    (multiple-value-bind (ips found-p) (gethash code tbl)
      (the (or simple-vector null)
           (if found-p
               ips
               (labels ((aux (start end)
                          (unless (= start end)
                            (let* ((mid (+ (truncate (- end start) 2) start)))
                              (cond ((< code (aref ranges (* mid range-size))) (aux start mid))
                                    ((<= code (aref ranges (+ (* mid range-size) 1))) (aref ranges (+ (* mid range-size) 2)))
                                    (t (aux (1+ mid) end)))))))
                 (aux 0 (truncate (length ranges) range-size))))))))

(defvar *evm-instruction-reverse-lookup* (make-hash-table))

(macrolet ((def (&rest templates)
             (let* ((n-arglists 2)
                    (arglists (loop for i below n-arglists collect (make-symbol (format nil "ARGLIST~d" i)))))
               `(progn
                  ,@(loop for (name) in templates and i from 0 append `((defconstant ,name ,i) (setf (gethash ,i *evm-instruction-reverse-lookup*) ',name)))
                  (defun grammar-encode (insts)
                    (let ((opcodes (make-array (length insts) :element-type '(unsigned-byte 8)))
                          ,@(loop for arglist in arglists collect `(,arglist (make-array (length insts) :initial-element nil))))
                      (loop for inst across insts
                            and i from 0
                            do (ecase (car inst)
                                 ,@(loop for (name . lambda-list) in templates
                                         collect (let ((args (loop for arg in (remove-if (lambda (x) (member x lambda-list-keywords)) lambda-list)
                                                                   collect (if (symbolp arg) (list arg) arg))))
                                                   (assert (<= (length args) n-arglists))
                                                   `((,name)
                                                     (destructuring-bind
                                                         ,(loop for x in lambda-list
                                                                collect (if (consp x) (first x) x))
                                                         (rest inst)
                                                       (setf (aref opcodes i) ,name)
                                                       (let ((vals (append ,@(loop for arg in args and arg-idx from 0
                                                                                   collect (destructuring-bind (name &key (type t type-p) encode) arg
                                                                                             (let ((val name))
                                                                                               (if (typep type '(cons (eql values)))
                                                                                                   (setf type (append type (list '&optional)))
                                                                                                   (setf type `(values ,type &optional)))
                                                                                               (when encode (setf val `(,encode ,val)))
                                                                                               (when type-p (setf val `(the ,type ,val)))
                                                                                               `(multiple-value-list ,val)))))))
                                                         (assert (<= (length vals) ,n-arglists))
                                                         ,@(loop for arglist in arglists and arglist-idx from 0
                                                                 collect `(setf (aref ,(nth arglist-idx arglists) i) (nth ,arglist-idx vals))))))))))
                      (values opcodes ,@arglists)))))))
  (def
    (i-call-foreign (n-args :type u32) (symbol :type symbol))
    (i-call         (n-args :type u32) &rest (ips :encode encode-integer-list :type ip-array))
    (i-match   (n-args :type u32) &rest (syms :encode encode-list :type simple-vector))
    (i-match-r (n-args :type u32) &rest (syms :encode encode-list :type simple-vector))
    (i-reduce   (n-args :type u32) (sym :type symbol))
    (i-reduce-r (n-args :type u32) (sym :type symbol))
    (i-reject   (n-args :type u32) (sym :type symbol))
    (i-stop)
    (i-trace            (sym :type symbol))
    (i-trace-list       (sym :type symbol))
    (i-trace-range      (sym :type symbol))
    (i-trace-range-list (sym :type symbol))
    (i-trace-push       (sym :type symbol) val)
    (i-trace-push-list  (sym :type symbol) val)
    (i-trace-return     (sym :type symbol))
    (i-char    &rest (maps :encode encode-char-map :type (values hash-table simple-vector)))
    (i-char-r  &rest (maps :encode encode-char-map :type (values hash-table simple-vector)))
    (i-fork     &rest (ips :encode encode-integer-list :type ip-array))
    (i-fork-ord &rest (ips :encode encode-integer-list :type ip-array))
    (i-br (ip :type u32))
    (i-bz (ip :type u32))
    (i-pop)
    (i-push val)
    (i-peek (n :type u32))
    (i-poke (n :type u32))
    (i-advance (n :type u32))
    (i-advance-dyn)
    (i-push-offset)
    (i-pop-offset)
    (i-capture)
    (i-swap)
    (i-dup)
    (i-drop     (n :type u32))
    (i-collapse (n :type u32))
    (i-dyn-get (key :type symbol))
    (i-dyn-set (key :type symbol))
    (i-noop)
    (i-negate)
    (i-nrot)))

(defun grammar-disassemble (grammaroid)
  (let ((grammar (coerce-grammar grammaroid)))
    (loop for i from 0
          and op across (grammar-opcodes grammar)
          and arg1 across (grammar-args1 grammar)
          and arg2 across (grammar-args2 grammar)
          do (format t "~%~d: ~a(~a, ~a)" i (gethash op *evm-instruction-reverse-lookup*) arg1 arg2))))

(defun compile-grammar (symbols)
  (let ((*groups-reached* nil)
        insts entrypoints)
    (setf (values insts entrypoints) (optimise-unify symbols))
    (setf (values insts entrypoints) (optimise-breaks insts entrypoints))
    (multiple-value-bind (opcodes args1 args2) (grammar-encode insts)
      (let ((grammar (make-grammar :entrypoints entrypoints :opcodes opcodes :args1 args1 :args2 args2))
            (groups (union symbols *groups-reached*)))
        (loop for group in groups
              do (loop for rule in (group-rules group)
                       do (loop with matches = nil and reduces = nil
                                for inst across (rule-insts rule)
                                do (when (consp inst)
                                     (case (first inst)
                                       ((i-match i-match-r)
                                        (loop for sym in (cddr inst) do (push sym matches)))
                                       ((i-reduce i-reduce-r)
                                        (loop for sym in (cddr inst) do (push sym reduces)))))
                                finally (loop for match in matches
                                              do (loop for reduce in reduces
                                                       do (pushnew reduce (gethash match (grammar-match-potential-reductions grammar))))))))
        (values grammar groups)))))

(defun grammar-match-may-reduce-p (grammar match-symbol reduce-symbol)
  (and (member reduce-symbol (gethash match-symbol (grammar-match-potential-reductions grammar))) t))

(defun coerce-grammar (grammaroid &key (cache t) preferred-id)
  (etypecase grammaroid
    (grammar (return-from coerce-grammar grammaroid))
    (cons
     (loop for symbol in grammaroid
           do (assert (symbolp symbol))))
    (symbol
     (setf grammaroid (list grammaroid))))
  (when cache
    (loop with ret = nil
          for (symbols grammar) in *cache*
          do (when (loop for symbol in grammaroid always (member symbol symbols))
               (cond ((eql (grammar-id grammar) preferred-id)
                      (return-from coerce-grammar grammar))
                     ((not ret)
                      (setf ret grammar))))
          finally (when ret
                    (return-from coerce-grammar ret))))
  (multiple-value-bind (grammar groups) (compile-grammar grammaroid)
    (when cache
      (push (list* grammaroid grammar groups) *cache*))
    grammar))
