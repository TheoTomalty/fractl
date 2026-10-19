(in-package #-evmi "FTX-RUN/STRING" #+evmi "FTX-RUN/TEXT")


;; TODO: remove support for dynamic variables

(deftype !point () #-evmi 'fixnum #+evmi 'point)

(defstruct (trace-node (:constructor nil) (:copier nil))
  (next nil :type (or trace-node null)))
(defstruct (trace-push-node (:include trace-node))
  (name nil :type symbol)
  (data nil))
(defstruct (trace-push-list-node (:include trace-node))
  (name nil :type symbol)
  (data nil))
(defstruct (trace-return-node (:include trace-node))
  (rule nil :type symbol))

(deftype trace-list () `(or trace-node null))

(defun trace-node-data (node)
  (declare (type trace-node node))
  (etypecase node
    (trace-push-node (trace-push-node-data node))
    (trace-push-list-node (trace-push-list-node-data node))
    (trace-return-node nil)))

(defstruct (ast-node (:include ast))
  (symbol      nil :type symbol)
  (args        nil :type list)
  (children    nil :type (or list trace-list))
  #-evmi
  (origin      nil :type fixnum)
  #-evmi
  (offset      nil :type fixnum)
  #+evmi
  (region      nil :type region)
  (dynamic     nil :type list)
  (val         nil)
  #+evmi
  (refcount    1   :type fixnum)
  #+evmi
  (container   nil :type (or dll-node null)))

#+evmi
(defun ast-node-origin (node)
  (ftx::region-left (ast-node-region node)))

#+evmi
(defun ast-node-offset (node)
  (ftx::region-right (ast-node-region node)))

(defun ast-node-foffset (node)
  #-evmi (ast-node-offset node)
  #+evmi (ftx::region-fright (ast-node-region node)))

(defun ast-node-ambiguous-p (node)
  (consp (ast-node-children node)))

#+evmi
(import 'ftx::leaf)

#+evmi
(defstruct committed-extent
  (key         nil)
  (trace       nil :type cons)
  (nodes       nil :type vector)
  (region      nil :type (or null region)))

#+evmi
(defstruct extent
  (deps        nil :type list)
  (reached-eof nil :type boolean)
  (left-leaf   nil :type (or leaf null))
  (left-offset   0 :type fixnum)
  (right-leaf  nil :type (or leaf null))
  (right-offset  0 :type fixnum)
  (key         nil)
  (trace       nil :type cons)
  (nodes       nil :type list)
  ;; Used for committing
  (mark        nil :type boolean))

(macrolet ((defstruct-flat (name &body fields)
             `(progn
                ,@(loop for (field _ . options) in fields and i from 0
                        for flat-accessor = (intern (concatenate 'string (symbol-name name) "-" (symbol-name field) "-AT"))
                        and type = (getf options :type t)
                        append `((declaim (inline ,flat-accessor (setf ,flat-accessor)))
                                 (defun ,flat-accessor (vector offset)
                                   (declare (type simple-vector vector) (type fixnum offset))
                                   (the ,type (aref vector (+ offset ,i))))
                                 (defun (setf ,flat-accessor) (new vector offset)
                                   (declare (type simple-vector vector) (type fixnum offset) (type ,type new))
                                   (setf (aref vector (+ offset ,i)) new)))))))
  (defconstant fibre-size #-evmi 6 #+evmi 9)
  (defstruct-flat fibre
    #+evmi
    (origin-leaf  nil :type leaf)
    (origin  0 :type fixnum)
    #+evmi
    (offset-leaf nil :type leaf)
    (offset  0 :type fixnum)
    (ip      0 :type fixnum)
    (staged  nil)
    (trace   nil :type trace-list)
    (dynamic nil :type list)
    #+evmi
    (extent  nil :type (or extent null))))

(defstruct (susp-fibre (:constructor %make-susp-fibre))
  (refcount 1 :type fixnum)
  #+evmi
  (origin-leaf nil :type (or leaf null))
  (origin  nil :type fixnum)
  (trace   nil :type trace-list)
  (stack   nil :type list)
  (args    nil :type list)
  (return-p nil)
  #+evmi
  (extent  nil :type (or extent null)))

(defparameter *susp-fibre-pool* (make-dynamic-array))

(defun make-susp-fibre (&key #+evmi origin-leaf origin trace stack args return-p #+evmi extent)
  (if (< 0 (length *susp-fibre-pool*))
      (let ((ret (vector-pop *susp-fibre-pool*)))
        (setf (susp-fibre-refcount ret) 1
              #+evmi (susp-fibre-origin-leaf ret) #+evmi origin-leaf
              (susp-fibre-origin ret) origin
              (susp-fibre-stack ret) stack
              (susp-fibre-args ret) args
              (susp-fibre-trace ret) trace
              (susp-fibre-return-p ret) return-p
              #+evmi (susp-fibre-extent ret) #+evmi extent)
        ret)
      (%make-susp-fibre #+evmi :origin-leaf #+evmi origin-leaf :origin origin
                        :trace trace :stack stack :args args :return-p return-p
                        #+evmi :extent #+evmi extent)))

(defun reclaim-susp-fibre (fibre)
  (setf #+evmi (susp-fibre-origin-leaf fibre) #+evmi nil
        (susp-fibre-trace fibre) nil
        (susp-fibre-stack fibre) nil
        (susp-fibre-args fibre) nil
        #+evmi (susp-fibre-extent fibre) #+evmi nil)
  (vector-push-extend fibre *susp-fibre-pool*))

(defstruct (susp (:constructor %make-susp))
  (symbol nil :type symbol)
  (fibre  nil :type (or susp-fibre null))
  (ip     nil :type fixnum)
  (universe 0 :type fixnum)
  (refcount 1 :type fixnum)
  (next   nil :type (or susp null)))

(defparameter *susp-pool* (make-dynamic-array))

(defun make-susp (&key symbol fibre ip universe next)
  (if (< 0 (length *susp-pool*))
      (let ((ret (vector-pop *susp-pool*)))
        (setf (susp-symbol ret) symbol
              (susp-fibre ret) fibre
              (susp-ip ret) ip
              (susp-universe ret) universe
              (susp-refcount ret) 1
              (susp-next ret) next)
        ret)
      (%make-susp :symbol symbol :fibre fibre :ip ip :universe universe :next next)))

(defun susp-incref (susp)
  (incf (susp-refcount susp))
  susp)

(defun reclaim-susp (susp)
  (setf (susp-symbol susp) nil
        (susp-fibre susp) nil
        (susp-next susp) nil)
  (vector-push-extend susp *susp-pool*)
  nil)

(defun susp-decref (susp)
  (assert (< 0 (susp-refcount susp)))
  (when (= 0 (decf (susp-refcount susp)))
    (susp-fibre-decref (susp-fibre susp))
    (reclaim-susp susp)))

(defun susp-fibre-incref (fibre)
  (incf (susp-fibre-refcount fibre))
  fibre)

(defun susp-fibre-decref (fibre)
  (assert (< 0 (susp-fibre-refcount fibre)))
  (when (= 0 (decf (susp-fibre-refcount fibre)))
    (reclaim-susp-fibre fibre)))

#-evmi
(progn
  (defmacro ast-node-incref (node)
    node)
  (defmacro ast-trace-incref (trace)
    trace))

#+evmi
(progn
  (defvar *extents*)
  (defvar *text*)

  (defconstant extent-region-type 'extents)
  (defconstant extent-min-size 3)

  (defun remove-trace (key trace)
    (setf (car trace) (delete key (car trace) :test #'eq :key #'car)))

  (defun update-extent-trace (key trace extent)
    (setf (cdr (assoc key (car trace) :test #'eq)) extent))

  (defun ast-node-incref (node)
    (let ((node (dll-node-data (ast-node-container node))))
      (assert (<= 0 (ast-node-refcount node)))
      (incf (ast-node-refcount node)))
    node)

  (defun ast-trace-incref (trace)
    (loop for subtrace = trace then (trace-node-next subtrace)
          while subtrace
          do (let ((it (trace-node-data subtrace)))
               (when (ast-node-p it)
                 (ast-node-incref it))))
    trace)

  (defun ast-trace-decref (trace)
    (loop for subtrace = trace then (trace-node-next subtrace)
          while subtrace
          do (let ((it (trace-node-data subtrace)))
               (when (ast-node-p it)
                 (ast-node-decref it)))))

  (defun ast-node-decref (node)
    (let* ((dll-node (ast-node-container node))
           (node (dll-node-data dll-node))
           (region (ast-node-region node)))
      (assert (< 0 (ast-node-refcount node)))
      (when (= 0 (decf (ast-node-refcount node)))
        (if (ast-node-ambiguous-p node)
            (loop for child in (ast-node-children node)
                  do (ast-trace-decref (ast-node-children child)))
            (ast-trace-decref (ast-node-children node)))
        (if (consp (region-data region))
            (let ((nodes (delete node (region-data region))))
              (assert nodes)
              (setf (region-data region) (if (null (cdr nodes)) (first nodes) nodes)))
            (delete-region region))
        (delete-dll-node dll-node)))
    nil)

  (defun commit-extents (top-extent)
    (labels ((aux (extent)
               (unless (extent-mark extent)
                 (setf (extent-mark extent) t)
                 (loop with left-leaf = (extent-left-leaf extent) and left-offset = (extent-left-offset extent)
                       and right-leaf = (extent-right-leaf extent) and right-offset = (extent-right-offset extent)
                       for child in (extent-deps extent)
                       do (etypecase child
                            (extent
                             (aux child)
                             (when (extent-left-leaf child)
                               (when (or (not left-leaf)
                                         (ftx::rpoint< (extent-left-leaf child) (extent-left-offset child)
                                                       left-leaf left-offset))
                                 (setf left-leaf (extent-left-leaf child)
                                       left-offset (extent-left-offset child)))
                               (when (or (not right-leaf)
                                         (ftx::rpoint< right-leaf right-offset
                                                       (extent-right-leaf child) (extent-right-offset child)))
                                 (setf right-leaf (extent-right-leaf child)
                                       right-offset (extent-right-offset child)))))
                            (committed-extent
                             (let* ((region (committed-extent-region child))
                                    (left (region-left region))
                                    (right (region-right region)))
                               (when left-leaf
                                 (when (ftx::rpoint< (point-leaf left) (point-offset left)
                                                     left-leaf left-offset)
                                   (setf left-leaf (point-leaf left)
                                         left-offset (point-offset left)))
                                 (when (ftx::rpoint< right-leaf right-offset
                                                     (point-leaf right) (point-offset right))
                                   (setf right-leaf (point-leaf right)
                                         right-offset (point-offset right)))))))
                       finally (setf (extent-left-leaf extent) left-leaf
                                     (extent-left-offset extent) left-offset
                                     (extent-right-leaf extent) right-leaf
                                     (extent-right-offset extent) right-offset)))))
      (loop for extent in *extents*
            do (aux extent)))
    (loop for extent in *extents*
          do (if (and (extent-left-leaf extent)
                      (or (eq extent top-extent)
                          (<= extent-min-size
                              (abs (- (ftx::fpoint-pos
                                       (extent-left-leaf extent)
                                       (extent-left-offset extent))
                                      (ftx::fpoint-pos
                                       (extent-right-leaf extent)
                                       (extent-right-offset extent)))))))
                 (let ((committed-extent (make-committed-extent
                                          :key (extent-key extent)
                                          :trace (extent-trace extent)
                                          :nodes (apply #'vector (extent-nodes extent))))
                       (region (fmake-region :right-inserting
                                             (extent-left-leaf extent)
                                             (extent-left-offset extent)
                                             :left-inserting
                                             (extent-right-leaf extent)
                                             (extent-right-offset extent))))
                   (setf (committed-extent-region committed-extent) region
                         (region-data region) committed-extent)
                   (update-extent-trace (extent-key extent) (extent-trace extent) committed-extent))
                 (delete-extent extent))))

  (defun extent-depend (parent child)
    (push child (extent-deps parent)))

  (defun delete-extent (extent)
    (remove-trace (extent-key extent) (extent-trace extent))
    (loop for node in (extent-nodes extent)
          do (ast-node-decref node)))

  (defun delete-committed-extent (extent)
    (remove-trace (committed-extent-key extent) (committed-extent-trace extent))
    (loop for node across (committed-extent-nodes extent)
          do (ast-node-decref node))
    (delete-region (committed-extent-region extent)))

  (defun point-invalidate (point)
    (let ((regions nil))
      (do-regions-at (region point)
        (when (committed-extent-p (region-data region))
          (push region regions)))
      (loop for region in regions
            do (delete-committed-extent (region-data region)))))

  (defun point-invalidate-between (left right)
    (assert (eql (point-text left) (point-text right)))
    (let ((regions nil))
      (do-regions-between (region left right)
        (when (committed-extent-p (region-data region))
          (push region regions)))
      (loop for region in regions
            do (delete-committed-extent (region-data region))))))

#-evmi
(progn
  (defvar *string*)
  (defvar *states*)

  (macrolet ((def (name fname n)
               `(progn
                  (defun ,name (n)
                    (aref *states* (+ (* n 3) ,n)))
                  (defun (setf ,name) (new n)
                    (setf (aref *states* (+ (* n 3) ,n)) new))
                  (defun ,fname (n)
                    (aref *states* (+ (* n 3) ,n)))
                  (defun (setf ,fname) (new n)
                    (setf (aref *states* (+ (* n 3) ,n)) new)))))
    (def !point-reductions !rpoint-reductions 0)
    (def !point-trace !rpoint-trace 1)
    (def !point-susps !rpoint-susps 2))

  (defmacro !point-offset (point)
    point)

  (defmacro !point-pos (point)
    point)

  (defmacro !point= (o1 o2)
    `(= ,o1 ,o2))
  (defmacro !point<= (o1 o2)
    `(<= ,o1 ,o2))
  (defmacro !rpoint= (o1 o2)
    `(= ,o1 ,o2))
  (defmacro !rpoint<= (o1 o2)
    `(<= ,o1 ,o2))
  
  (defmacro copy-!point (point)
    point)
  
  (defmacro !point-skip (point)
    `(incf ,point))
  (defun !point-peek (point)
    (if (< point (length *string*))
        (aref *string* point)
        nil))
  (defmacro make-!point (offset)
    offset))

#+evmi
(macrolet ((def (alias name)
             `(setf (fdefinition ',alias) (fdefinition ',name))))
  (def !point-reductions point-reductions)
  (def !point-trace point-trace)
  (def !point-susps point-susps)
  (def (setf !point-susps) (setf point-susps))
  (def !rpoint-reductions rpoint-reductions)
  (def !rpoint-trace rpoint-trace)
  (def !rpoint-susps rpoint-susps)
  (def (setf !rpoint-susps) (setf rpoint-susps))
  (def !point-offset point-offset)
  (def !point-pos point-pos)
  (def !point= point=)
  (def !point<= point<=)
  (def !rpoint= rpoint=)
  (def !rpoint<= rpoint<=)
  (def copy-!point copy-point)
  (def !point-skip point-skip)
  (def !point-peek point-peek)
  (def make-!point make-point))

(defvar *evm-instructions* nil)

(defmacro define-evm-instruction (name args &body body)
  `(progn
     (pushnew ',name *evm-instructions*)
     (declaim (inline ,name))
     (defun ,name (state ,@args)
       (declare (type state state))
       ,@body)))

(defmacro call-evm-instruction (name &rest args)
  `(,name state ,@args))

(defparameter *stacks* (make-dynamic-array))

(defun make-stack ()
  (if (= 0 (length *stacks*))
      (make-array 100000)
      (vector-pop *stacks*)))

(defun reclaim-stack (stack)
  (vector-push stack *stacks*))

(defstruct state
  (stack (make-stack) :type simple-vector)
  (universe 0 :type fixnum)
  (base 0 :type fixnum)
  (fp 0 :type fixnum)
  (sp 0 :type fixnum)
  (spmax 0 :type fixnum)
  (astack (make-stack) :type simple-vector)
  (abase 0 :type fixnum)
  (afp 0 :type fixnum)
  (asp 0 :type fixnum)
  (aspmax 0 :type fixnum)
  (reach 0 :type !point)
  (gc-left nil :type !point)
  (gc-right nil :type !point)
  (gc-reachable nil :type list)
  (matches nil :type list)
  (negated nil :type (member t nil))
  (stash nil :type list))

(define-symbol-macro stack  (state-stack  state))
(define-symbol-macro base   (state-base   state))
(define-symbol-macro fp     (state-fp     state))
(define-symbol-macro sp     (state-sp     state))
(define-symbol-macro spmax  (state-spmax  state))
(define-symbol-macro astack (state-astack state))
(define-symbol-macro abase  (state-abase  state))
(define-symbol-macro afp    (state-afp    state))
(define-symbol-macro asp    (state-asp    state))
(define-symbol-macro aspmax (state-aspmax state))
(define-symbol-macro universe (state-universe state))

(defun clear-state (state)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (flet ((clear-stack (stack length)
           (loop for i below length
                 do (setf (svref stack i) 0))))
    (clear-stack stack spmax)
    (clear-stack astack aspmax)))

(defun check-state-clear (state)
  (flet ((check-stack (stack)
           (loop for i below (length stack)
                 do (assert (= 0 (svref stack i))))))
    (check-stack stack)
    (check-stack astack)))

(defun reclaim-state (state)
  (reclaim-stack stack)
  (reclaim-stack astack))

#+evmi
(define-symbol-macro current-fibre-origin-leaf (fibre-origin-leaf-at  (state-stack state) (state-fp state)))
(define-symbol-macro current-fibre-origin  (fibre-origin-at  (state-stack state) (state-fp state)))
#+evmi
(define-symbol-macro current-fibre-offset-leaf (fibre-offset-leaf-at  (state-stack state) (state-fp state)))
(define-symbol-macro current-fibre-offset  (fibre-offset-at  (state-stack state) (state-fp state)))
(define-symbol-macro current-fibre-ip      (fibre-ip-at      (state-stack state) (state-fp state)))
(define-symbol-macro current-fibre-staged  (fibre-staged-at  (state-stack state) (state-fp state)))
(define-symbol-macro current-fibre-trace   (fibre-trace-at   (state-stack state) (state-fp state)))
(define-symbol-macro current-fibre-dynamic (fibre-dynamic-at (state-stack state) (state-fp state)))
#+evmi
(define-symbol-macro current-fibre-extent  (fibre-extent-at  (state-stack state) (state-fp state)))

(defun push-stack (state val)
  (setf (aref stack sp) val)
  (incf sp))

(defun pop-stack (state)
  (setf spmax (max sp spmax))
  (decf sp)
  (aref stack sp))

(defun push-stack-alt (state val)
  (setf (aref astack asp) val)
  (incf asp))

(defun %push-fibre (stack loc #+evmi origin-leaf origin #+evmi offset-leaf offset ip staged trace dynamic #+evmi extent)
  #+evmi
  (setf (fibre-origin-leaf-at stack loc) origin-leaf)
  (setf (fibre-origin-at stack loc) origin)
  #+evmi
  (setf (fibre-offset-leaf-at stack loc) offset-leaf)
  (setf (fibre-offset-at stack loc) offset)
  (setf (fibre-ip-at stack loc) ip)
  (setf (fibre-staged-at stack loc) staged)
  (setf (fibre-trace-at stack loc) trace)
  (setf (fibre-dynamic-at stack loc) dynamic)
  #+evmi
  (setf (fibre-extent-at stack loc) extent)
  (values loc (+ loc fibre-size)))

(defun push-fibre (state #+evmi origin-leaf origin #+evmi offset-leaf offset ip staged trace dynamic #+evmi extent)
  (push-stack state fp)
  (setf (values fp sp)
        (%push-fibre stack sp #+evmi origin-leaf origin #+evmi offset-leaf offset
                     ip staged trace dynamic #+evmi extent)))

(defun push-fibre-alt (state #+evmi origin-leaf origin #+evmi offset-leaf offset ip staged trace dynamic #+evmi extent)
  (push-stack-alt state afp)
  (setf (values afp asp)
        (%push-fibre astack asp #+evmi origin-leaf origin #+evmi offset-leaf offset ip staged trace dynamic #+evmi extent)))

(defun %pop-fibre (stack fp)
  (values (aref stack (1- fp)) (1- fp)))

(defun pop-fibre (state)
  (setf spmax (max sp spmax))
  (setf (values fp sp) (%pop-fibre stack fp)))

(define-evm-instruction i-call-foreign (n sym)
  (declare (type fixnum n) (type symbol sym))
  (push-stack state
              (progv
                  (mapcar #'car current-fibre-dynamic)
                  (mapcar #'cdr current-fibre-dynamic)
                (apply (symbol-function sym)
                       (prog1 (loop for i from (- sp n) below sp collect (aref stack i))
                         (setf (state-sp state) (- sp n))))))
  (incf current-fibre-ip))

(defun trace-equal (t1 t2)
  (if (or (integerp t1) (integerp t2))
      (eql t1 t2)
      (destructuring-bind (u1 ip1 dyn1 . stack1) t1
        (destructuring-bind (u2 ip2 dyn2 . stack2) t2
          (and (= u1 u2)
               (= ip1 ip2)
               (dynamic-equal dyn1 dyn2)
               (equal stack1 stack2))))))

#-evmi
(defun trace-undo-fibre (state offset)
  (declare (type state state))
  (let* ((key (list* universe (fibre-ip-at stack fp) (fibre-dynamic-at stack fp)
                     (loop for i from (+ fp fibre-size) below sp collect (aref stack i)))))
    (if (member key (!rpoint-trace offset) :test #'trace-equal)
        (pop-fibre state)
        (push key (!rpoint-trace offset)))
    nil))

#+evmi
(defun trace-undo-fibre (state leaf offset &optional parent-extent)
  (declare (type state state))
  (let* ((key (let ((ip (fibre-ip-at stack fp))
                    (dyn (fibre-dynamic-at stack fp))
                    (stack (loop for i from (+ fp fibre-size) below sp collect (aref stack i))))
                (if (or dyn stack)
                    (list* universe ip dyn stack)
                    (progn
                      (assert (= universe (ldb (byte 16 0) universe)))
                      (the fixnum (logior (ash ip 16) universe))))))
         (trace (or (rpoint-trace leaf offset)
                    (setf (rpoint-trace leaf offset) (list nil))))
         (extent nil)
         (existing-extent (cdr (assoc key (car trace) :test #'trace-equal))))
    (if existing-extent
        (progn
          (setf extent existing-extent)
          (pop-fibre state))
        (progn
          (setf extent (make-extent :key key :trace trace))
          (push extent *extents*)
          (setf (fibre-extent-at stack fp) extent)
          (push (cons key extent) (car trace))))
    (when parent-extent
      (extent-depend parent-extent extent))
    extent))

(define-evm-instruction i-call (n ips)
  (declare (type fixnum n) (type (vector (unsigned-byte 32)) ips))
  (let (#+evmi (offset-leaf current-fibre-offset-leaf)
        (offset current-fibre-offset)
        (dynamic current-fibre-dynamic)
        #+evmi (extent current-fibre-extent)
        (start (- sp n))
        (end sp))
    (incf current-fibre-ip)
    (loop for ip across ips
          do (push-fibre state #+evmi offset-leaf offset #+evmi offset-leaf offset ip nil nil dynamic #+evmi nil)
             (loop for i from start below end do (push-stack state (aref stack i)))
             ;; TODO: A duplicate call is not elided when it originates
             ;;       from a different universe, but perhaps there is a way
             ;;       of upgrading the previous fibre group to run in the
             ;;       new universe.
             (trace-undo-fibre state #+evmi offset-leaf offset #+evmi extent))))

(defun make-susp-fibre-at (stack n fp sp return-p)
  (make-susp-fibre #+evmi :origin-leaf #+evmi (fibre-origin-leaf-at stack fp)
                   :origin (fibre-origin-at stack fp)
                   :trace (fibre-trace-at stack fp)
                   :stack (loop for i from (+ fp fibre-size) below (- sp n)
                                collect (aref stack i))
                   :args (loop for i from (- sp n) below sp
                                collect (aref stack i))
                   :return-p return-p
                   #+evmi :extent #+evmi (fibre-extent-at stack fp)))

(defmacro dolist-reductions ((var reductions #+evmi &rest #+evmi options) &body body)
  #-evmi
  `(dolist (,var ,reductions) ,@body)
  #+evmi
  `(dll-dolist-maybe (,var ,reductions ,@options) ,@body))

(defmacro dolist-susps ((var susps) &body body)
  `(loop for ,var = ,susps then (susp-next ,var) while ,var do (progn ,@body)))

(defun activate-susp-reduction (state susp reduction &aux (fibre (susp-fibre susp)))
  (multiple-value-bind (#+evmi leaf offset) (ast-node-foffset reduction)
    (push-fibre state
                #+evmi
                (susp-fibre-origin-leaf fibre)
                (susp-fibre-origin fibre)
                #+evmi
                leaf
                offset
                (susp-ip susp)
                reduction
                (susp-fibre-trace fibre) 
                (ast-node-dynamic reduction)
                #+evmi (susp-fibre-extent fibre)))
  (loop for elt in (susp-fibre-stack fibre)
        do (push-stack state elt))
  (when (susp-fibre-return-p fibre)
    (push-stack state (ast-node-val reduction))))

(define-evm-instruction i-match (n syms return-p)
  (declare (type simple-vector syms))
  (let ((fibre (make-susp-fibre-at stack n fp sp return-p))
        #+evmi (offset-leaf current-fibre-offset-leaf)
        (offset current-fibre-offset))
    ;; TODO: encode (sym ip) pairs
    (pop-fibre state)
    (loop for (sym . ips) across syms
          do (loop for ip in ips
                   do (let ((susp (make-susp :symbol sym :fibre (susp-fibre-incref fibre) :ip ip
                                             :universe universe
                                             :next (!rpoint-susps #+evmi offset-leaf offset))))
                        (setf (!rpoint-susps #+evmi offset-leaf offset) susp)
                        (dolist-reductions (reduction (!rpoint-reductions #+evmi offset-leaf offset))
                          (when (and (eq sym (ast-node-symbol reduction)) (equal (susp-fibre-args fibre) (ast-node-args reduction)))
                            (if (multiple-value-bind (#+evmi leaf offset) (ast-node-foffset reduction)
                                  (!rpoint<= #+evmi leaf offset
                                             #+evmi (point-leaf (state-reach state)) (!point-offset (state-reach state))))
                                (activate-susp-reduction state susp reduction)
                                (push (list (susp-incref susp) reduction #+evmi offset-leaf offset)
                                      (state-matches state))))))))
    (susp-fibre-decref fibre)))

(defun trace-node-equal (t1 t2)
  (loop while (and t1 t2)
        do (unless (etypecase t1
                     (trace-push-node
                      (and (trace-push-node-p t2)
                           (eq (trace-push-node-name t1) (trace-push-node-name t2))
                           (equal (trace-push-node-data t1) (trace-push-node-data t2))))
                     (trace-push-list-node
                      (and (trace-push-list-node-p t2)
                           (eq (trace-push-list-node-name t1) (trace-push-list-node-name t2))
                           (equal (trace-push-list-node-data t1) (trace-push-list-node-data t2))))
                     (trace-return-node
                      (and (trace-return-node-p t2)
                           (eq (trace-return-node-rule t1) (trace-return-node-rule t2)))))
             (return nil))
           (setf t1 (trace-node-next t1)
                 t2 (trace-node-next t2))
        finally (return (and (null t1) (null t2)))))

(defun dynamic-equal (dyn1 dyn2)
  (loop for val1 in dyn1
        do (unless (loop for val2 in dyn2 thereis (equal val1 val2))
             (return-from dynamic-equal nil)))
  (loop for val2 in dyn2
        do (unless (loop for val1 in dyn1 thereis (equal val1 val2))
             (return-from dynamic-equal nil)))
  t)

(defun find-reduction (reductions symbol args #+evmi offset-leaf offset dynamic val)
  (dolist-reductions (reduction reductions)
    (when (and (eql symbol (ast-node-symbol reduction))
               (equal args (ast-node-args reduction))
               (multiple-value-bind (#+evmi reduction-leaf reduction-offset) (ast-node-foffset reduction)
                 (!rpoint=
                  #+evmi offset-leaf offset
                  #+evmi reduction-leaf reduction-offset))
               (equal val (ast-node-val reduction))
               (dynamic-equal dynamic (ast-node-dynamic reduction)))
      (return reduction))))

(defun ast-node-matches-p (node children)
  (if (ast-node-ambiguous-p node)
      (loop for child in (ast-node-children node)
            do (when (trace-node-equal children (ast-node-children child))
                 (return child)))
      (and (trace-node-equal children (ast-node-children node)) node)))

(defun merge-ast-node (node1 node2)
  #+evmi
  (incf (ast-node-refcount node1) (ast-node-refcount node2))
  #+evmi
  (setf (ast-node-refcount node2) -1)
  (if (ast-node-ambiguous-p node1)
      (progn
        (setf (cdr (last (ast-node-children node1)))
              (list node2))
        node1)
      (let ((copy (copy-ast-node node1)))
        (setf (ast-node-children node1) (list copy node2))
        #+evmi
        (setf (ast-node-refcount copy) -1)
        node1)))

#+evmi
(defun share-region (l1 o1 l2 o2)
  (dolist-reductions (reduction (!rpoint-reductions #+evmi l1 o1))
    (let ((region (ast-node-region reduction)))
      (when (multiple-value-bind (l o) (ftx::region-fright region)
              (fpoint= l o l2 o2))
        (return-from share-region region))))
  ;; NOTE: right point must be left-inserting so that it is rpoint compatible
  (fmake-region :right-inserting l1 o1 :left-inserting l2 o2))

(define-evm-instruction i-reduce (n symbol return-p)
  (declare (type symbol symbol))
  (let* ((args (loop for i from (+ fp fibre-size) below sp repeat n collect (aref stack i)))
         (children (fibre-trace-at stack fp))
         (val (when return-p (pop-stack state)))
         #+evmi (origin-leaf current-fibre-origin-leaf)
         (origin current-fibre-origin)
         #+evmi (offset-leaf current-fibre-offset-leaf)
         (offset current-fibre-offset)
         (dynamic current-fibre-dynamic)
         #+evmi (extent current-fibre-extent)
         (reduction (find-reduction (!rpoint-reductions #+evmi origin-leaf origin)
                                    symbol args #+evmi offset-leaf offset
                                    dynamic val))
         (node nil))
    (incf current-fibre-ip)
    (if reduction
        (if (setf node (ast-node-matches-p reduction children))
            #-evmi nil #+evmi (ast-node-incref node)
            (merge-ast-node reduction
                            (setf node #-evmi
                                       (make-ast-node :symbol symbol
                                                      :args args
                                                      :origin (ast-node-origin reduction)
                                                      :offset (ast-node-offset reduction)
                                                      :children children
                                                      :dynamic dynamic :val val)
                                       #+evmi
                                       (make-ast-node :symbol symbol
                                                      :args args
                                                      :region (ast-node-region reduction)
                                                      :children (ast-trace-incref children)
                                                      :dynamic dynamic :val val
                                                      :container (ast-node-container reduction)))))
        (progn
          (setf node #-evmi
                     (make-ast-node :symbol symbol :args args :origin origin :offset offset :children children
                                    :dynamic dynamic :val val)
                     #+evmi
                     (let* ((region (share-region origin-leaf origin offset-leaf offset))
                            (node (make-ast-node :symbol symbol :args args :region region :children (ast-trace-incref children)
                                                 :dynamic dynamic :val val)))
                       (typecase (region-data region)
                         (null (setf (region-data region) node))
                         (cons (push node (region-data region)))
                         (t (setf (region-data region) (list node (region-data region)))))
                       node))
          (setf reduction node)
          #-evmi
          (push reduction (!point-reductions origin))
          #+evmi
          (setf (ast-node-container node) (dll-push reduction (rpoint-reductions origin-leaf origin)))
          (dolist-susps (susp (!rpoint-susps #+evmi origin-leaf origin))
            (when (and (eq symbol (susp-symbol susp)) (equal args (susp-fibre-args (susp-fibre susp))))
              (let ((universe-level-diff (- universe (susp-universe susp))))
                (if (= 0 universe-level-diff)
                    (activate-susp-reduction state susp reduction)
                    (push (list (susp-incref susp) reduction #+evmi offset-leaf offset)
                          (state-matches (car (nth (1- universe-level-diff) (state-stash state)))))))))))
    #+evmi
    (push node (extent-nodes extent))))

(define-evm-instruction i-stop ()
  (pop-fibre state))

(defmacro push-trace (name data)
  `(setf current-fibre-trace
         (make-trace-push-node :next current-fibre-trace
                               :name ,name
                               :data ,data)))

(defmacro push-trace-list (name data)
  `(setf current-fibre-trace
         (make-trace-push-list-node :next current-fibre-trace
                                    :name ,name
                                    :data ,data)))

(define-evm-instruction i-trace (sym)
  (declare (type symbol sym))
  (push-trace sym current-fibre-staged)
  (incf current-fibre-ip))

(define-evm-instruction i-trace-list (sym)
  (declare (type symbol sym))
  (push-trace-list sym current-fibre-staged)
  (incf current-fibre-ip))

#-evmi
(defun current-range (state)
  (subseq *string* current-fibre-origin current-fibre-offset))

#+evmi
(defun current-range (state)
  (text-substring *text*
                  (fpoint-pos current-fibre-origin-leaf current-fibre-origin)
                  (fpoint-pos current-fibre-offset-leaf current-fibre-offset)))

(define-evm-instruction i-trace-range (sym)
  (declare (type symbol sym))
  (push-trace sym (current-range state))
  (incf current-fibre-ip))

(define-evm-instruction i-trace-range-list (sym)
  (declare (type symbol sym))
  (push-trace-list sym (current-range state))
  (incf current-fibre-ip))

(define-evm-instruction i-trace-push (sym val)
  (declare (type symbol sym))
  (push-trace sym val)
  (incf current-fibre-ip))

(define-evm-instruction i-trace-push-list (sym val)
  (declare (type symbol sym))
  (push-trace-list sym val)
  (incf current-fibre-ip))

(define-evm-instruction i-trace-return (sym)
  (declare (type symbol sym))
  (setf current-fibre-trace
        (make-trace-return-node :next current-fibre-trace :rule sym))
  (incf current-fibre-ip))

#+evmi
(defun state-read-char (state)
  ;; TODO: bar i-advance from going past region bounds
  ;; TODO: implement i-advance extending region left-wards
  (let* ((extent current-fibre-extent)
         (leaf current-fibre-offset-leaf)
         (offset current-fibre-offset)
         (char nil))
    (if (extent-left-leaf extent)
        (progn
          (when (rpoint< leaf offset (extent-left-leaf extent) (extent-left-offset extent))
            (setf (extent-left-leaf extent) leaf
                  (extent-left-offset extent) offset))
          (setf (values leaf offset char) (rpoint-next leaf offset))
          (when (rpoint< (extent-right-leaf extent) (extent-right-offset extent) leaf offset)
            (setf (extent-right-leaf extent) leaf
                  (extent-right-offset extent) offset))
          (unless char
            (setf (extent-reached-eof extent) t)))
        (progn
          (setf (extent-left-leaf extent) leaf
                (extent-left-offset extent) offset)
          (setf (values leaf offset char) (rpoint-next leaf offset))
          (setf (extent-right-leaf extent) leaf
                (extent-right-offset extent) offset)
          (unless char
            (setf (extent-reached-eof extent) t))))
    (setf current-fibre-offset-leaf leaf
          current-fibre-offset offset)
    (aif char (char-code char) -1)))

#-evmi
(defun state-read-char (state)
  (let ((offset current-fibre-offset))
    (if (< offset (length *string*))
        (progn
          (incf current-fibre-offset)
          (char-code (aref *string* offset)))
        -1)))

(defun fork-fibre (state ip fp sp)
  (push-fibre state
              #+evmi (fibre-origin-leaf-at stack fp)
              (fibre-origin-at stack fp)
              #+evmi (fibre-offset-leaf-at stack fp)
              (fibre-offset-at stack fp)
              ip
              (fibre-staged-at stack fp)
              (fibre-trace-at stack fp)
              (fibre-dynamic-at stack fp)
              #+evmi (fibre-extent-at stack fp))
  (loop for i from (+ fp fibre-size) below sp
        do (push-stack state (aref stack i))))

(defun fork-fibre-alt (state ip fp sp)
  (push-fibre-alt state
                  #+evmi (fibre-origin-leaf-at stack fp)
                  (fibre-origin-at stack fp)
                  #+evmi (fibre-offset-leaf-at stack fp)
                  (fibre-offset-at stack fp)
                  ip
                  (fibre-staged-at stack fp)
                  (fibre-trace-at stack fp)
                  (fibre-dynamic-at stack fp)
                  #+evmi (fibre-extent-at stack fp))
  (loop for i from (+ fp fibre-size) below sp
        do (push-stack-alt state (aref stack i))))

(define-evm-instruction i-char (tbl ranges return-p)
  (let* ((code (state-read-char state))
         (char (if (= -1 code) :eof (code-char code)))
         (ips (encoded-char-map-ips tbl ranges code))
         (fp fp) (sp sp))
    (setf current-fibre-staged char)
    (if ips
        (if (!rpoint<= #+evmi current-fibre-offset-leaf current-fibre-offset
                       #+evmi (point-leaf (state-reach state)) (!point-offset (state-reach state)))
            (progn
              (setf current-fibre-ip (aref ips 0))
              (when return-p
                (push-stack state char))
              (loop for i from 1 below (length ips)
                    do (fork-fibre state (aref ips i) fp sp)))
            (loop for ip across ips
                  do (fork-fibre-alt state ip fp sp)
                     (when return-p
                       (push-stack-alt state char))
                  finally (pop-fibre state)))
        (pop-fibre state))))

(define-evm-instruction i-fork (ips)
  (declare (type (vector (unsigned-byte 32)) ips))
  (incf current-fibre-ip)
  (let ((fp fp) (sp sp))
    (loop for ip across ips
          do (fork-fibre state ip fp sp))))

(define-evm-instruction i-fork-ord (ips)
  (declare (type (vector (unsigned-byte 32)) ips))
  (if (= 0 (length ips))
      (incf current-fibre-ip)
      (let ((ip current-fibre-ip)
            (fp fp) (sp sp))
        (setf current-fibre-ip (aref ips 0))
        (loop for i from 1 below (length ips)
              do (fork-fibre state (aref ips i) fp sp))
        (fork-fibre state (1+ ip) fp sp)))
  (let ((copy (copy-state state)))
    (pop-fibre copy)
    (push (cons copy (length ips)) (state-stash state)))
  (incf universe)
  (let ((point (make-!point #+evmi current-fibre-offset-leaf current-fibre-offset)))
    (psetf (state-reach state) point
           (state-gc-left state) point
           (state-gc-right state) point
           (state-gc-reachable state) nil
           (state-matches state) nil
           (state-negated state) nil
           base (1- fp)
           abase asp)))

(define-evm-instruction i-negate ()
  (setf (state-negated state) t)
  (pop-fibre state))

(define-evm-instruction i-br (ip)
  (declare (type fixnum ip))
  (setf current-fibre-ip ip))

(define-evm-instruction i-bz (ip)
  (declare (type fixnum ip))
  (if (pop-stack state)
      (incf current-fibre-ip)
      (setf current-fibre-ip ip)))

(define-evm-instruction i-pop ()
  (pop-stack state)
  (incf current-fibre-ip))

(define-evm-instruction i-push (val)
  (push-stack state val)
  (incf current-fibre-ip))

(define-evm-instruction i-peek (n)
  (declare (type fixnum n))
  (push-stack state (aref stack (+ fp fibre-size n)))
  (incf current-fibre-ip))

(define-evm-instruction i-poke (n)
  (declare (type fixnum n))
  (setf (aref stack (+ fp fibre-size n)) (pop-stack state))
  (incf current-fibre-ip))

(defun state-advance (state n)
  (declare (ignore state n))
  ;; TODO: implement negative advances
  ;; TODO: implement interaction with gc
  (error "unimplemented")
  ;; (incf current-fibre-offset n)
  ;; #+evmi
  ;; (if (<= 0 n)
  ;;     (loop repeat n do (!mark-skip current-fibre-offset))
  ;;     (loop repeat n do (!mark-skip-prev current-fibre-offset)))
  )

(define-evm-instruction i-advance (n)
  (declare (type fixnum n))
  (state-advance state n)
  (incf current-fibre-ip))

(define-evm-instruction i-advance-dyn ()
  (state-advance state (the fixnum (pop-stack state)))
  (incf current-fibre-ip))

(define-evm-instruction i-push-offset ()
  (push-stack state #+evmi current-fibre-offset-leaf #-evmi nil)
  (push-stack state current-fibre-offset)
  (incf current-fibre-ip))

(define-evm-instruction i-pop-offset ()
  (let ((offset (pop-stack state))
        (leaf (pop-stack state)))
    #-evmi (declare (ignore leaf))
    (when (!rpoint<= #+evmi leaf offset #+evmi (point-leaf (state-gc-left state)) (!point-offset (state-gc-left state)))
      (setf (state-gc-left state) (make-!point #+evmi leaf offset)))
    #+evmi
    (setf current-fibre-offset-leaf leaf)
    (setf current-fibre-offset offset))
  (incf current-fibre-ip))

(define-evm-instruction i-capture ()
  (let ((end-offset (pop-stack state))
        (end-leaf   (pop-stack state))
        (begin-offset (pop-stack state))
        (begin-leaf   (pop-stack state)))
    #-evmi (declare (ignore begin-leaf end-leaf))
    (push-stack state #-evmi (subseq *string* begin-offset end-offset)
                      #+evmi (text-substring *text* (fpoint-pos begin-leaf begin-offset) (fpoint-pos end-leaf end-offset))))
  (incf current-fibre-ip))

(define-evm-instruction i-swap ()
  (let ((val1 (pop-stack state))
        (val2 (pop-stack state)))
    (push-stack state val1)
    (push-stack state val2))
  (incf current-fibre-ip))

(define-evm-instruction i-dup ()
  (let ((val (pop-stack state)))
    (push-stack state val)
    (push-stack state val))
  (incf current-fibre-ip))

(define-evm-instruction i-drop (n)
  (declare (type fixnum n))
  (loop repeat n do (pop-stack state))
  (incf current-fibre-ip))

(define-evm-instruction i-collapse (n)
  (declare (type fixnum n))
  (let ((top (pop-stack state)))
    (loop repeat n do (pop-stack state))
    (push-stack state top))
  (incf current-fibre-ip))

(define-evm-instruction i-dyn-get (sym)
  (declare (type symbol sym))
  (let ((assoc (assoc sym current-fibre-dynamic)))
    (push-stack state
                (if assoc
                    (cdr assoc)
                    (symbol-value sym))))
  (incf current-fibre-ip))

(define-evm-instruction i-dyn-set (sym)
  (declare (type symbol sym))
  (let ((val (pop-stack state)))
    (setf current-fibre-dynamic
          (list* (cons sym val)
                 ;; TODO: Makes a copy of the list even if there are no elements to remove
                 (remove-if (lambda (cons) (eql sym (first cons)))
                            current-fibre-dynamic))))
  (incf current-fibre-ip))

(define-evm-instruction i-noop ()
  (incf current-fibre-ip))

(define-evm-instruction i-nrot ()
  (let ((val1 (pop-stack state))
        (val2 (pop-stack state))
        (val3 (pop-stack state)))
    (push-stack state val1)
    (push-stack state val3)
    (push-stack state val2))
  (incf current-fibre-ip))

(defun count-fibres (stack fp)
  (loop while (< 0 fp)
        collect (fibre-ip-at stack fp)
        do (setf fp (aref stack (1- fp)))))

(defvar *grammar*)

(defun state-gc (state)
  (let ((reachable nil)
        (reachable-susps nil))
    (labels ((pushnew-point (#+evmi leaf offset)
               (unless (loop for point in reachable
                             thereis (!rpoint= #+evmi leaf offset #+evmi (point-leaf point) (!point-offset point)))
                 (push (make-!point #+evmi leaf offset) reachable)))
             (aux (#+evmi leaf offset &optional (symbol nil symbol-p))
               (pushnew-point #+evmi leaf offset)
               (dolist-susps (susp (!rpoint-susps #+evmi leaf offset))
                 (when (and (= universe (susp-universe susp))
                            (not (member susp reachable-susps))
                            (or (not symbol-p) (ftx::grammar-match-may-reduce-p *grammar* symbol (susp-symbol susp))))
                   (push susp reachable-susps)
                   (aux #+evmi (susp-fibre-origin-leaf (susp-fibre susp)) (susp-fibre-origin (susp-fibre susp)) (susp-symbol susp))))))
      (loop with fp = fp
            while (< base fp)
            do (aux #+evmi (fibre-origin-leaf-at stack fp) (fibre-origin-at stack fp))
               (setf fp (aref stack (1- fp))))
      (loop with afp = afp
            while (< abase afp)
            do (aux #+evmi (fibre-origin-leaf-at astack afp) (fibre-origin-at astack afp))
               (setf afp (aref astack (1- afp))))
      (loop for (susp reduction #+evmi leaf offset) in (state-matches state)
            do (pushnew-point #+evmi leaf offset)
               (pushnew susp reachable-susps)
               (aux #+evmi (susp-fibre-origin-leaf (susp-fibre susp)) (susp-fibre-origin (susp-fibre susp)) (susp-symbol susp))))
    (labels ((reclaim (susp full-p)
               (when susp
                 (if (and (= universe (susp-universe susp))
                          (or full-p (not (member susp reachable-susps))))
                     (let ((next (susp-next susp)))
                       (susp-decref susp)
                       (reclaim next full-p))
                     (progn
                       (setf (susp-next susp) (reclaim (susp-next susp) full-p))
                       susp))))
             (maybe-reclaim (offset)
               (setf (!point-susps offset) (reclaim (!point-susps offset)
                                                    (not (member offset reachable #+evmi :test #+evmi #'point=))))))
      (loop for point in (state-gc-reachable state)
            do (maybe-reclaim point))
      (loop for point = (copy-!point (state-gc-left state)) then (!point-skip point)
            do (maybe-reclaim point)
            until (!point= point (state-gc-right state)))
      (setf (state-gc-reachable state) reachable)
      (setf (state-gc-left state) (state-reach state)
            (state-gc-right state) (state-reach state)))))

(defun check-susps-clear ()
  (assert #+evmi
          (= 0 (loop with point = (text-pos-point *text* 0)
                     count (point-susps point)
                     while (point-next point)))
          #-evmi
          (= 0 (loop for i below (1+ (length *string*))
                     count (!point-susps i)))
          nil
          "gc failed to clear all suspensions"))

(defun extract-ast (symbol reductions)
  (aif (sort (remove-if-not (lambda (node) (eql symbol (ast-node-symbol node))) reductions)
             (lambda (r1 r2)
               (multiple-value-bind (#+evmi l1 o1) (ast-node-foffset r1)
                 (multiple-value-bind (#+evmi l2 o2) (ast-node-foffset r2)
                   (!rpoint<= #+evmi l2 o2 #+evmi l1 o1)))))
       (values (car it) t)
       (values nil nil)))

#-evmi
(defmethod evm-run ((string string) grammaroid &rest keys)
  (apply #'evm-run (ftx::make-string-text string) grammaroid keys))

(defmethod evm-run ((text #-evmi ftx::string-text #+evmi text) grammaroid &key (start 0) (symbol nil symbol-p))
  (let* ((symbol (if symbol-p
                     symbol
                     (typecase grammaroid
                       (symbol grammaroid)
                       (cons (car grammaroid))
                       (t (error "Can not infer symbol from grammar")))))
         (prev-id (#-evmi ftx::string-text-grammar-id #+evmi text-grammar-id text))
         (grammar (coerce-grammar grammaroid :preferred-id prev-id))
         (*grammar* grammar)
         (entry-ip (aif (assoc symbol (ftx::grammar-entrypoints grammar))
                        (cdr it)
                        (error "Symbol ~s not found in grammar" symbol)))
         (opcodes (ftx::grammar-opcodes grammar))
         (args1 (ftx::grammar-args1 grammar))
         (args2 (ftx::grammar-args2 grammar))
         #-evmi (*string* (ftx::string-text-string text))
         #-evmi (*states* (ftx::string-text-states text))
         #+evmi (*text* text)
         #+evmi (*extents* nil)
         #+evmi extent
         #+evmi (start (ftx::point-bias-right (text-pos-point text start)))
         (state (make-state :gc-left start :gc-right start :reach start))
         (switch-count 0))
    (declare (type state state) (type fixnum switch-count))
    (let ((new-id (grammar-id grammar)))
      (unless (or (null prev-id)
                  (= prev-id new-id))
        (warn "Grammar used to parse text has changed")
        (#-evmi ftx::string-text-clear-state #+evmi text-clear-state text))
      (setf (#-evmi ftx::string-text-grammar-id #+evmi text-grammar-id text) new-id))
    (push-fibre state #+evmi (point-leaf start) (!point-offset start) #+evmi (point-leaf start) (!point-offset start)
                entry-ip nil nil nil #+evmi nil)
    (#-evmi progn #+evmi setf #+evmi extent
     (trace-undo-fibre state #+evmi (point-leaf start) (!point-offset start)))
    (symbol-macrolet ((arg1 (aref args1 current-fibre-ip))
                      (arg2 (aref args2 current-fibre-ip)))
      (tagbody
       start
         (unless (< base fp)
           (psetf stack astack base abase fp afp sp asp spmax aspmax
                  astack stack abase base afp fp asp sp aspmax spmax)
           (flet ((activate-matches (point)
                    (labels ((nremove (list)
                               (when list
                                 (destructuring-bind (susp reduction . rest) (car list)
                                   (declare (ignore rest))
                                   (if (multiple-value-bind (#+evmi leaf offset) (ast-node-foffset reduction)
                                         (!rpoint= #+evmi (point-leaf point) (!point-offset point) #+evmi leaf offset))
                                       (progn
                                         (activate-susp-reduction state susp reduction)
                                         (susp-decref susp)
                                         (nremove (cdr list)))
                                       (rplacd list (nremove (cdr list))))))))
                      (setf (state-matches state) (nremove (state-matches state)))))
                  (maybe-gc ()
                    (when (= 0 (logand (incf switch-count) #b11111)) ;; <- gc every 32 characters
                      (state-gc state)))
                  (expand-left/right (state point)
                    (unless (and (state-gc-right state) (!point<= point (state-gc-right state)))
                      (setf (state-gc-right state) point))
                    (unless (and (state-gc-left state) (!point<= (state-gc-left state) point))
                      (setf (state-gc-left state) point))))
             (let ((next-point
                     (or (loop with fp = fp and leaf = nil and offset = nil
                               while (< base fp)
                               do (if offset
                                      (assert (!rpoint= #+evmi leaf offset #+evmi (fibre-offset-leaf-at stack fp) (fibre-offset-at stack fp)))
                                      (progn
                                        #+evmi
                                        (setf leaf (fibre-offset-leaf-at stack fp))
                                        (setf offset (fibre-offset-at stack fp))))
                                  (setf fp (aref stack (1- fp)))
                               finally (return (when offset (make-!point #+evmi leaf offset))))
                         (let (#+evmi (min-leaf nil)
                               (min-offset nil))
                           (loop for (susp reduction) in (state-matches state)
                                 do (multiple-value-bind (#+evmi leaf offset) (ast-node-foffset reduction)
                                      (unless (and min-offset (!rpoint<= #+evmi min-leaf min-offset #+evmi leaf offset))
                                        (setf #+evmi min-leaf #+evmi leaf
                                              min-offset offset))))
                           (when min-offset
                             (make-!point #+evmi min-leaf min-offset))))))
               (cond (next-point
                      (activate-matches next-point)
                      (maybe-gc)
                      (expand-left/right state next-point)
                      (setf (state-reach state) next-point))
                     ((state-stash state)
                      (state-gc state)
                      (destructuring-bind (stashed-state . nfibres) (car (state-stash state))
                        (setf (state-spmax stashed-state)
                              (if (eq stack (state-stack stashed-state)) spmax aspmax))
                        (setf (state-aspmax stashed-state)
                              (if (eq astack (state-astack stashed-state)) aspmax spmax))
                        (when (state-negated state)
                          (loop repeat nfibres do (pop-fibre stashed-state)))
                        (setf state stashed-state)
                        (go start)))
                     (t
                      (state-gc state)
                      (clear-state state)
                      ;; TODO: remove these checks for performance
                      ;; (check-state-clear state)
                      ;; (check-susps-clear)
                      (reclaim-state state)
                      (go end))))))
;;          (when *debug-evm*
;;            (format t "~%~s/~s fibre:(~s,~s) gc:(~s, ~s) stashed:~s" (count-fibres stack fp) (count-fibres astack afp)
;;                    current-fibre-origin current-fibre-offset
;;                    (point-pos (state-gc-left state)) (point-pos (state-gc-right state))
;;                    (length (state-stash state)))
;;            (format t "~%~d: ~s(~s, ~s)" current-fibre-ip (gethash (aref opcodes current-fibre-ip) ftx::*evm-instruction-reverse-lookup*) arg1 arg2))
         (case (aref opcodes current-fibre-ip)
           ;; Make sure that instructions are in numeric order
           ((#.ftx::i-call-foreign)     (call-evm-instruction i-call-foreign     arg1 arg2))
           ((#.ftx::i-call)             (call-evm-instruction i-call             arg1 arg2))
           ((#.ftx::i-match)            (call-evm-instruction i-match            arg1 arg2 nil))
           ((#.ftx::i-match-r)          (call-evm-instruction i-match            arg1 arg2 t))
           ((#.ftx::i-reduce)           (call-evm-instruction i-reduce           arg1 arg2 nil))
           ((#.ftx::i-reduce-r)         (call-evm-instruction i-reduce           arg1 arg2 t))
           ((#.ftx::i-reject)           (unimplemented))
           ((#.ftx::i-stop)             (call-evm-instruction i-stop))
           ((#.ftx::i-trace)            (call-evm-instruction i-trace            arg1))
           ((#.ftx::i-trace-list)       (call-evm-instruction i-trace-list       arg1))
           ((#.ftx::i-trace-range)      (call-evm-instruction i-trace-range      arg1))
           ((#.ftx::i-trace-range-list) (call-evm-instruction i-trace-range-list arg1))
           ((#.ftx::i-trace-push)       (call-evm-instruction i-trace-push       arg1 arg2))
           ((#.ftx::i-trace-push-list)  (call-evm-instruction i-trace-push-list  arg1 arg2))
           ((#.ftx::i-trace-return)     (call-evm-instruction i-trace-return     arg1))
           ((#.ftx::i-char)             (call-evm-instruction i-char             arg1 arg2 nil))
           ((#.ftx::i-char-r)           (call-evm-instruction i-char             arg1 arg2 t))
           ((#.ftx::i-fork)             (call-evm-instruction i-fork             arg1))
           ((#.ftx::i-fork-ord)         (call-evm-instruction i-fork-ord         arg1))
           ((#.ftx::i-br)               (call-evm-instruction i-br               arg1))
           ((#.ftx::i-bz)               (call-evm-instruction i-bz               arg1))
           ((#.ftx::i-pop)              (call-evm-instruction i-pop))
           ((#.ftx::i-push)             (call-evm-instruction i-push             arg1))
           ((#.ftx::i-peek)             (call-evm-instruction i-peek             arg1))
           ((#.ftx::i-poke)             (call-evm-instruction i-poke             arg1))
           ((#.ftx::i-advance)          (call-evm-instruction i-advance          arg1))
           ((#.ftx::i-advance-dyn)      (call-evm-instruction i-advance-dyn))
           ((#.ftx::i-push-offset)      (call-evm-instruction i-push-offset))
           ((#.ftx::i-pop-offset)       (call-evm-instruction i-pop-offset))
           ((#.ftx::i-capture)          (call-evm-instruction i-capture))
           ((#.ftx::i-swap)             (call-evm-instruction i-swap))
           ((#.ftx::i-dup)              (call-evm-instruction i-dup))
           ((#.ftx::i-drop)             (call-evm-instruction i-drop             arg1))
           ((#.ftx::i-collapse)         (call-evm-instruction i-collapse         arg1))
           ((#.ftx::i-dyn-get)          (call-evm-instruction i-dyn-get          arg1))
           ((#.ftx::i-dyn-set)          (call-evm-instruction i-dyn-set          arg1))
           ((#.ftx::i-noop)             (call-evm-instruction i-noop))
           ((#.ftx::i-negate)           (call-evm-instruction i-negate))
           ((#.ftx::i-nrot)             (call-evm-instruction i-nrot)))
         (go start)
       end))
    #+evmi (commit-extents extent)
    (extract-ast symbol
                 #-evmi (!point-reductions start)
                 #+evmi (dll-list (point-reductions start)))))

(defmethod eval-ast ((ast-node ast-node) &optional evaluator)
  (when (ast-node-ambiguous-p ast-node)
    (loop for ast-node in (ast-node-children ast-node)
          do (format t "ambiguous: ~a~%  ~s~%  ~s~%" (ast-node-symbol ast-node) ast-node (ast-node-children ast-node)))
    (error "ambiguous parse ~s" (ast-node-symbol ast-node)))
  (loop with eval-fn = nil and rule-params = nil and rule-p = nil and scope = nil and unbound-marker = '#:unbound-marker
        for node = (ast-node-children ast-node) then (trace-node-next node)
        do (etypecase node
             (trace-push-node
              (unless (assoc (trace-push-node-name node) scope)
                (push (cons (trace-push-node-name node) (trace-push-node-data node)) scope)))
             (trace-push-list-node
              (aif (assoc (trace-push-list-node-name node) scope)
                   (push (trace-push-list-node-data node) (cdr it))
                   (push (list (trace-push-list-node-name node) (trace-push-list-node-data node)) scope)))
             (trace-return-node
              (when rule-p
                (error "Too many TRACE-RETURN nodes"))
              (let ((rule (trace-return-node-rule node)))
                (setf eval-fn (etypecase evaluator
                                (function evaluator)
                                (null
                                 (get rule 'ftx::rule-default-evaluator))
                                (symbol
                                 (awhen (or (loop for group in (rule-groups rule)
                                                  thereis (assoc evaluator (get group 'ftx::group-evaluators)))
                                            (assoc evaluator (get rule 'ftx::rule-evaluators)))
                                   (destructuring-bind (rule-evaluator constantp) (cdr it)
                                     (if constantp
                                         (return rule-evaluator)
                                         rule-evaluator)))))
                      rule-params (get rule 'ftx::rule-params)
                      rule-p t)))
             (null
              (return
                (when eval-fn
                  (loop for (var) in rule-params
                        do (unless (assoc var scope)
                             (push (cons var unbound-marker) scope)))
                  (labels ((symbol-value* (name)
                             (if (boundp name) (symbol-value name) unbound-marker))
                           (swap-scope ()
                             (loop for pair in scope
                                   do (if (eql (cdr pair) unbound-marker)
                                          (progn
                                            (setf (cdr pair) (symbol-value* (car pair)))
                                            (makunbound (car pair)))
                                          (setf (symbol-value (car pair)) (shiftf (cdr pair) (symbol-value* (car pair))))))))
                    (swap-scope)
                    (unwind-protect (funcall eval-fn)
                      (swap-scope)))))))))

(defmethod ast-rule ((node ast-node))
  (if (ast-ambiguous-p node)
      (values nil nil)
      (loop for subtrace = (ast-node-children node) then (trace-node-next subtrace)
            while subtrace
            do (when (trace-return-node-p subtrace)
                 (return (values (trace-return-node-rule subtrace) t)))
            finally (return (values nil nil)))))

(defmethod print-object ((node ast-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":symbol ~s :origin ~s :offset ~s"
            (ast-node-symbol node)
            (!point-pos (ast-node-origin node))
            (!point-pos (ast-node-offset node)))))

(defmethod ast-origin ((node ast-node))
  (ast-node-origin node))

(defmethod ast-offset ((node ast-node))
  (ast-node-offset node))

(defmethod ast-string ((node ast-node))
  #-evmi (unimplemented)
  #+evmi (let ((origin (ast-node-origin node))
               (offset (ast-node-offset node)))
           (text-substring (point-text origin) (point-pos origin) (point-pos offset))))

(defmethod ast-length ((node ast-node))
  (- (!point-pos (ast-node-offset node)) (!point-pos (ast-node-origin node))))

(defmethod ast-symbol ((node ast-node))
  (ast-node-symbol node))

(defmethod ast-children ((node ast-node))
  (if (consp (ast-node-children node))
      (ast-node-children node)
      (loop with ret = nil
            for n = (ast-node-children node) then (trace-node-next n)
            while n
            do (let ((it (trace-node-data n)))
                 (when (ast-node-p it)
                   (push it ret)))
            finally (return ret))))

(defmethod ast-ambiguous-p ((node ast-node))
  (ast-node-ambiguous-p node))

#+evmi
(defun print-reductions (text &key (start 0) end)
  (loop with point = (text-pos-point text start)
        for i from start
        while (or (null end) (< i end))
        do (dll-dolist (reduction (point-reductions point))
             (format t "  ~s(~d, ~d)~%" (ast-node-symbol reduction) i (point-pos (ast-node-offset reduction))))
           (let ((char (point-next point)))
             (if char
                 (format t "~d: ~s~%" i char)
                 (progn
                   (format t "~d:~%" i)
                   (return))))))

(defun test-evm (&key (n 1) profile return (grammar 'fractl) file string)
  (assert (and (or file string) (not (and file string))) ()
          "only one file or string may be specified")
  (let* ((string (cond (file (read-file-string file))
                       (string string)))
         (vals nil))
    (when profile (sb-profile:reset))
    (loop repeat n
          do (setf vals
                   (multiple-value-list
                    (let ((arg #-evmi string #+evmi (string-text string)))
                      (if return
                          (evm-parse arg grammar)
                          (evm-run arg grammar))))))
    (when profile (sb-profile:report))
    (when return
      (values-list vals))))

#+evmi
(defun report-memory (text)
  (let ((seen (make-hash-table :test 'eq))
        (n-branches 0)
        (n-array 0)
        (n-cons 0)
        (n-dll-node 0)
        (n-trace-node 0)
        (n-ast-node 0)
        (n-extents 0)
        (n-marks 0)
        (n-regions 0)
        (n-symbols (make-hash-table)))
    (labels ((aux (x)
               (unless (gethash x seen)
                 (setf (gethash x seen) t)
                 (typecase x
                   (ftx::branch (incf n-branches) (aux (ftx::branch-regions x)) (aux (ftx::branch-left x)) (aux (ftx::branch-right x)))
                   (array (incf n-array (1+ (length x))) (loop for y across x do (aux y)))
                   (cons (incf n-cons) (aux (car x)) (aux (cdr x)))
                   (trace-node
                    (typecase x
                      (trace-push-node (incf n-trace-node 4) (aux (trace-push-node-data x)))
                      (trace-push-list-node (incf n-trace-node 4) (aux (trace-push-list-node-data x)))
                      (trace-return-node (incf n-trace-node 3)))
                    (aux (trace-node-next x)))
                   (ast-node
                    (incf n-ast-node)
                    (aux (ast-node-args x))
                    (aux (ast-node-children x))
                    (aux (ast-node-dynamic x))
                    (aux (ast-node-val x))
                    (aux (ast-node-container x))
                    (setf (gethash (ast-node-symbol x) n-symbols)
                          (aif (gethash (ast-node-symbol x) n-symbols) (1+ it) 1)))
                   (fcl::dll
                    (dll-dolist (y x)
                      (incf n-dll-node)
                      (aux y)))
                   (extent (unreachable))
                   (committed-extent (incf n-extents) (aux (committed-extent-key x)) (aux (committed-extent-trace x)) (aux (committed-extent-nodes x)))
                   (mark (incf n-marks))
                   (region (incf n-regions) (aux (region-data x)))))))
      (aux (ftx::text-root text)))
    (let* ((ns (list n-branches n-array n-cons n-dll-node n-trace-node n-ast-node n-extents n-marks n-regions))
           (sizes (list (* 8 12) 8 (* 8 3) (* 8 5) 8 (* 8 9) (* 8 5) (* 8 4) (* 8 7)))
           (total (loop for n in ns and size in sizes sum (* n size)))
           (avgs (loop for n in ns and size in sizes collect (* (/ (coerce (* n size) 'float) total) 100))))
      (apply #'format t "
Branches: ~,,'_:d, ~,,'_:d (%~a)
Array: ~,,'_:d, ~,,'_:d (%~a)
Cons: ~,,'_:d, ~,,'_:d (%~a)
DLL nodes: ~,,'_:d, ~,,'_:d (%~a)
Trace nodes: ~,,'_:d, ~,,'_:d (%~a)
AST nodes: ~,,'_:d, ~,,'_:d (%~a)
Extents: ~,,'_:d, ~,,'_:d (%~a)
Marks: ~,,'_:d, ~,,'_:d (%~a)
Regions: ~,,'_:d, ~,,'_:d (%~a)

Reduction symbols:~:{~%  ~a ~a~}

Total: ~,,'_:d
"
             (append (loop for n in ns and size in sizes and avg in avgs
                           append (list n (* n size) avg))
                     (list (loop for sym being the hash-keys of n-symbols using (hash-value n)
                                 collect (list sym n))
                           total))))))

#+evmi
(defun test-evm-incremental (&key (n 10) profile file string (grammar 'fractl))
  (assert (and (or file string) (not (and file string))) ()
          "only one file or string may be specified")
  (let* ((grammar (coerce-grammar grammar))
         (text (string-text
                (cond (file (read-file-string file))
                      (string string))))
         (length (text-length text))
         (decls (evm-parse text grammar)))
    (declare (ignorable length decls))
    (when profile (sb-profile:reset))
    (loop repeat n
          for pos = (random length)
          ;; for pos from 0 below length
          do (let* ((point (text-pos-point text pos))
                    (char (point-peek point))
                    (next (point-skip (copy-point point))))
               (point-invalidate-between point next)
               (point-delete point)
               (evm-run text grammar)
               (setf point (text-pos-point text pos))
               (point-invalidate point)
               (point-insert point char)
               (evm-run-strict text grammar)))
    (when profile (sb-profile:report))
    nil))
