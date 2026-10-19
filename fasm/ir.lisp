(defpackage #:ir
  (:use #:cl #:utils #:types #:asm #:chi-symbols)
  (:export #:virtual-register #:virtual-register-p #:make-virtual-register
           #:virtual-register-number #:virtual-register-type
           #:constant #:constant-p #:make-constant
           #:constant-value #:constant-type
           
           #:codegen #:op-type
           #:call #:args #:alloca))

(in-package #:ir)


(defstruct virtual-register
  (number 0 :type (or integer symbol))
  (type nil :type (not null)))

(defstruct constant
  (value nil :type integer)
  (type nil :type (not null)))

(defun op-type (op)
  (etypecase op
    (virtual-register (virtual-register-type op))
    (constant (constant-type op))))

(defun op-p (op)
  (typep op '(or virtual-register constant)))

(defmethod print-object ((reg virtual-register) stream)
  (format stream "@~d[~a]" (virtual-register-number reg) (virtual-register-type reg)))

(defmethod print-object ((val constant) stream)
  (format stream "#~d[~a]" (constant-value val) (constant-type val)))

(defun print-insts (insts)
  (loop for inst across insts
        do (if (symbolp inst)
               (format t "~a:~%" inst)
               (format t "	~a ~{~a~^, ~}~%" (first inst) (rest inst)))))

(defparameter *ir-arguments* (make-hash-table))
(defparameter *ir-plist* (make-hash-table))


(defmacro define-ir-inst (names args &key (fallthrough t) expand-types (expand t))
  (let* ((arg-names (mapcar #'first args))
         (type-names (loop for arg-name in arg-names
                           collect (or (second (assoc arg-name expand-types)) (gensym "TYPE"))))
         (name-sym (gensym "NAME")))
    `(loop for ,name-sym in ,(if (symbolp names) `(list ',names) `',names)
         do (progn
              (setf (gethash ,name-sym *ir-arguments*) ',args)
              (setf (gethash ,name-sym *ir-plist*)
                    (list :fallthrough ',fallthrough
                          :expand (let ((name ,name-sym))
                                    (declare (ignorable name))
                                    (lambda (,@arg-names ,@type-names)
                                      (declare (ignorable ,@arg-names ,@type-names))
                                      ,(if (eql expand t)
                                           `(list (list name ,@arg-names))
                                           expand)))))))))

;; calling convention
(define-ir-inst args ((ret :out t :list t :load-volatile t))
  :expand nil)
(define-ir-inst call
    ((sym)
     (ret :out t :list t :load-volatile t)
     (args :in t :list t :store-volatile t))
  :expand `((bl ,sym)))
(defparameter *ret-sym* nil)
(define-ir-inst ret ((args :in t :list t :store-volatile t))
  :fallthrough nil
  :expand `((b ,*ret-sym*)))

;; logical comparisons
(define-ir-inst (eq ne)
    ((ret :out t)
     (x :in t :const-reg t :arith t)
     (y :in t :const-reg t :arith t))
  :expand `((cmp ,x ,y)
            (cset ,ret ,name)))

;; arithmetic ops
(define-ir-inst (madd msub)
    ((ret :out t :arith t)
     (n :in t :const-reg t :arith t)
     (m :in t :const-reg t :arith t)
     (a :in t :const-reg t :arith t)))
(define-ir-inst (add sub udiv)
    ((ret :out t :arith t)
     (n :in t :const-reg t :arith t)
     (m :in t :const-reg t :arith t)))

;; special
(defparameter *alloc-start* nil)
(define-ir-inst alloca ((ret :out t) (arg :alloc t))
  :expand `((add ,ret ,sp ,(+ arg *alloc-start*))))
(defun mov-constant-insts (reg n)
  ;; TODO: implement for arbitrary integers
  (assert (and (< n (expt 2 16)) (>= n (- (expt 2 16)))))
  `((mov ,reg ,n)))
(define-ir-inst mov ((ret :out t) (arg :in t))
  :expand (if (constant-p arg)
              (mov-constant-insts ret (constant-value arg))
              `((mov ,ret ,arg))))
(define-ir-inst coerce ((ret :out t) (arg :in t))
  :expand-types ((ret ret-type) (arg arg-type))
  :expand (list
           (cond
             ((< (size arg-type) (size ret-type))
              ;; extension
              (if (and (widep ret-type) (signedp arg-type))
                  (list 'sxtw ret arg)
                  (list 'mov ret arg)))
             ((or (> (size arg-type) (size ret-type)) (not (eql (signedp arg-type) (signedp ret-type))))
              ;; truncation
              (ecase (size ret-type)
                (1 (list (if (signedp ret-type) 'sxtb 'uxtb) ret (shrink-register arg)))
                (2 (list (if (signedp ret-type) 'sxth 'uxth) ret (shrink-register arg)))
                (4 (list 'mov ret (shrink-register arg)))
                (8 (list 'mov ret arg))))
             (t
              (list 'mov ret arg)))))

;; load/store
(defun load-name-from-type (type)
  (ecase (size type)
    (1 (if (signedp type) 'ldrsb 'ldrb))
    (2 (if (signedp type) 'ldrsh 'ldrh))
    ((4 8) 'ldr)))
(defun store-name-from-type (type)
  (ecase (size type)
    (1 'strb)
    (2 'strh)
    ((4 8) 'str)))
(define-ir-inst str
    ((op :in t :const-reg t)
     (ptr :in t :const-reg t)
     (offset :in t))
  :expand-types ((op op-type) (offset offset-type))
  :expand `((,(store-name-from-type op-type)
              ,op ,ptr ,offset
              ,@(when (and (register-p offset) (not (widep offset-type)))
                  (list (if (signedp offset-type) 'sxtw 'uxtw))))))
(define-ir-inst ldr
    ((op :out t)
     (ptr :in t :const-reg t)
     (offset :in t))
  :expand-types ((op op-type) (offset offset-type))
  :expand `((,(load-name-from-type op-type)
              ,op ,ptr ,offset
              ,@(when (and (register-p offset) (not (widep offset-type)))
                  (list (if (signedp offset-type) 'sxtw 'uxtw))))))

;; branches
(define-ir-inst cbz ((x :in t :const-reg t) (target :link t)))
(define-ir-inst b ((target :link t)) :fallthrough nil)

(defmacro loop-toplevel-arguments (inst (spec arg) &body rest)
  (let ((inst-sym (gensym "INST"))
        (arguments-sym (gensym "ARGUMENTS")))
    `(let ((,inst-sym ,inst))
       (loop for ,arguments-sym in (gethash-strict (first ,inst-sym) *ir-arguments*)
             for ,spec = (rest ,arguments-sym)
             for ,arg in (rest ,inst-sym)
             ,@rest))))

(defun get-arguments (inst predicate)
  (loop for arguments = (gethash-strict (first inst) *ir-arguments*) then (cdr arguments)
        for args = (rest inst) then (cdr args)
        while (and arguments args)
        when (funcall predicate (cdar arguments))
          append (let ((arg (car args)))
                   (if (listp arg) arg (list arg)))
        finally (when (or arguments args)
                  (error "arity mismatch in ~s" inst))))

(defun set-arguments (inst predicate fn)
  (loop for arguments = (gethash-strict (first inst) *ir-arguments*) then (cdr arguments)
        for args = (rest inst) then (cdr args)
        while (and arguments args)
        do (let ((arg (car args)))
             (if (listp arg)
                 (loop for sub-args = arg then (cdr sub-args)
                       while sub-args
                       for sub-arg = (car sub-args)
                       when (funcall predicate (cdar arguments) sub-arg)
                         do (setf (car sub-args) (funcall fn sub-arg)))
                 (when (funcall predicate (cdar arguments) arg)
                   (setf (car args) (funcall fn arg)))))
        finally (when (or arguments args)
                  (error "arity mismatch in ~s" inst))))

(defun branch-links (inst)
  (get-arguments inst (lambda (plist) (getf plist :link))))

(defun input-registers (inst)
  (remove-if-not #'virtual-register-p (get-arguments inst (lambda (plist) (getf plist :in)))))

(defun output-registers (inst)
  (remove-if-not #'virtual-register-p (get-arguments inst (lambda (plist) (getf plist :out)))))

(defparameter *end-indicator* (make-symbol "END"))
(defun ir-plist-get (inst indicator)
  (let ((ret (getf (gethash-strict (first inst) *ir-plist*) indicator *end-indicator*)))
    (when (eql *end-indicator* ret)
      (error "indicator ~s not in plist for ~s" indicator inst))
    ret))

(defun fallthrough-p (inst)
  (ir-plist-get inst :fallthrough))

(defun block-terminator-p (inst)
  (or (not (fallthrough-p inst))
      (not (null (branch-links inst)))))

(defstruct (block :named (:type list))
  (id nil :type integer)
  (start nil :type integer)
  (insts nil :type array)
  (forward-links nil :type list)
  (backward-links nil :type list))

(defun separate-blocks (insts)
  (let ((blocks (loop for i from 0 below (length insts)
                      with block-start = 0
                      with blocks = nil
                      do (labels ((separate-block (index)
                                    (when (not (eql block-start index))
                                      (push (subseq insts block-start index) blocks)
                                      (setf block-start index))))
                           (let ((inst (aref insts i)))
                             (if (symbolp inst)
                                 (separate-block i)
                                 (when (block-terminator-p inst)
                                   (separate-block (1+ i))))))
                      finally (progn
                                (assert (eql block-start (length insts)))
                                (return (apply #'vector (nreverse blocks))))))
        (forward-links (make-hash-table))
        (backward-links (make-hash-table))
        (named-blocks (make-hash-table)))
    (loop for block across blocks
          for i from 0
          do (when (symbolp (aref block 0))
               (setf (gethash (aref block 0) named-blocks) i)))
    (loop for block across blocks
          for last = (aref block (1- (length block)))
          for i from 0
          do (labels ((link (from to)
                        (push to (gethash from forward-links))
                        (push from (gethash to backward-links))))
               (loop for link in (branch-links last)
                     do (link i (gethash link named-blocks)))
               (when (fallthrough-p last)
                 (link i (1+ i)))))
    (map 'vector
         (let ((i -1)
               (start 0))
           (lambda (insts)
             (make-block :id (incf i) :insts insts
                         :start (prog1 start (incf start (length insts)))
                         :forward-links (gethash i forward-links)
                         :backward-links (gethash i backward-links))))
         blocks)))

(defstruct double-cons
  (element nil)
  (next nil :type (or double-cons null))
  (prev nil :type (or double-cons null)))

(defstruct active-list
  (head nil :type (or double-cons null)))

(defun double-cons-insert-before (new-node node)
  (setf (double-cons-next new-node) node
        (double-cons-prev new-node) (double-cons-prev node)
        (double-cons-next (double-cons-prev new-node)) new-node
        (double-cons-prev (double-cons-next new-node)) new-node))

(defun double-cons-delete (node)
  (let ((prev (double-cons-prev node))
        (next (double-cons-next node)))
    (when (not (eq next node))
      (setf (double-cons-prev next) prev
            (double-cons-next prev) next))))

(defun active-list-insert-sorted (active element &key key)
  (let ((new-node (make-double-cons :element element))
        (head (active-list-head active)))
    (if (null head)
        (setf (double-cons-next new-node) new-node
              (double-cons-prev new-node) new-node
              (active-list-head active) new-node)
        (loop for node = head then (double-cons-next node)
              with first = t
              when (or (and (not first) (eq node head))
                       (<= (funcall key element) (funcall key (double-cons-element node))))
                do (progn
                     (double-cons-insert-before new-node node)
                     (when first
                       (setf (active-list-head active) new-node))
                     (return))
              do (setf first nil)))))

(defun active-list-delete-until (active pred)
  (let ((head (active-list-head active)))
    (when head
      (loop with node = head
            until (or (null node) (funcall pred (double-cons-element node)))
            collect (double-cons-element node)
            do (setf node (setf (active-list-head active) (double-cons-delete node)))))))

(defun active-list-last (active)
  (let ((head (active-list-head active)))
    (when head
      (double-cons-element (double-cons-prev head)))))

(defun active-list-delete-last (active)
  (let ((head (active-list-head active)))
    (assert head)
    (setf (active-list-head active) (double-cons-delete (double-cons-prev head)))))

(defparameter *num-registers* 10)
(defparameter *non-volatile-start* 19)

(defun analyse-lifetimes (blocks)
  (let ((uses (make-hash-table))
        (assignments (make-hash-table))
        (types (make-hash-table))
        (spilled-registers nil)
        (allocations (make-hash-table))
        (regs-used nil)
        (stack-top 0))
    (labels ((add-to (reg val table)
               (let ((num (virtual-register-number reg)))
                 (multiple-value-bind (type found) (gethash num types)
                   (if found
                       (assert (ctype-equal type (virtual-register-type reg)))
                       (setf (gethash num types) (virtual-register-type reg))))
                 (push val (gethash num table)))))
      (loop for block across blocks
            do (loop for instr across (block-insts block)
                     for i from 0
                     when (consp instr)
                       do (progn
                            (loop for reg in (input-registers instr)
                                  do (add-to reg (list (block-id block) i) uses))
                            (loop for reg in (output-registers instr)
                                  do (add-to reg (list (block-id block) i) assignments))))
            finally (return (values uses assignments))))
    (labels ((assigned-block-p (block-id num)
               (loop for (id i) in (gethash num assignments)
                     if (= id block-id)
                       do (return t)))
             (assigned-in-block-p (block-id num i)
               (loop for (id j) in (gethash num assignments)
                     if (and (= id block-id)
                             (< j i))
                       do (return j)))
             (last-assigned (block-id num)
               (loop for (id i) in (gethash num assignments)
                     if (= id block-id)
                       maximize i))
             (mark-blocks-backwards (end-blocks visited-blocks block num)
               (loop for block-id in (block-backward-links block)
                     do (cond
                          ((assigned-block-p block-id num)
                           (setf (aref end-blocks block-id) t))
                          ((aref visited-blocks block-id))
                          (t
                           (setf (aref visited-blocks block-id) t)
                           (mark-blocks-backwards end-blocks visited-blocks
                                                  (aref blocks block-id)
                                                  num)))))
             (register-lifetime (num)
               (let (min-lifetime
                     max-lifetime)
                 (labels ((merge-lifetime (min max)
                            (setf min-lifetime
                                  (if min-lifetime
                                      (min min-lifetime min)
                                      min))
                            (setf max-lifetime
                                  (if max-lifetime
                                      (max max-lifetime max)
                                      max))))
                   (loop for (id i) in (gethash num assignments)
                         do (let ((block-start (block-start (aref blocks id))))
                              (merge-lifetime (+ block-start i) (+ block-start (1+ i)))))
                   (loop for (id i) in (gethash num uses)
                         do (let ((in-block (assigned-in-block-p id num i))
                                  (block (aref blocks id)))
                              (if in-block
                                  (merge-lifetime (+ in-block (block-start block))
                                                  (+ (1+ i) (block-start block)))
                                  (let ((end-blocks (make-array (length blocks) :initial-element nil))
                                        (visited-blocks (make-array (length blocks) :initial-element nil)))
                                    (mark-blocks-backwards end-blocks visited-blocks block num)
                                    (loop for visited across visited-blocks
                                          for id from 0
                                          for block = (aref blocks id)
                                          do (when visited
                                               (merge-lifetime
                                                (block-start block)
                                                (+ (block-start block)
                                                   (length (block-insts block))))))
                                    (loop for end-visited across end-blocks
                                          for id from 0
                                          for block = (aref blocks id)
                                          do (when end-visited
                                               (merge-lifetime
                                                (+ (block-start block)
                                                   (last-assigned id num))
                                                (+ (block-start block)
                                                   (length (block-insts block))))))
                                    (merge-lifetime (block-start block)
                                                    (+ (block-start block) i 1)))))))
                 (values min-lifetime max-lifetime))))
      (loop for entry
              in (sort (loop for num being the hash-keys of types
                             collect (list* num (multiple-value-list
                                                 (register-lifetime num))))
                       #'< :key #'second)
            for (num start end) = entry
            with free = (loop for i below *num-registers* collect (+ i *non-volatile-start*))
            with active = (make-active-list)
            do (loop for entry in (active-list-delete-until active (lambda (entry) (<= start (third entry))))
                     do (push (second (gethash (first entry) allocations)) free))
            do (if free
                   (progn
                     (setf (gethash num allocations) (let ((reg (pop free))) (pushnew reg regs-used) (list :register reg)))
                     (active-list-insert-sorted active entry :key #'third))
                   (let ((spill (active-list-last active)))
                     (if (and spill (< end (third spill)))
                         (progn
                           (setf (gethash num allocations) (gethash (first spill) allocations))
                           (remhash (first spill) allocations)
                           (push spill spilled-registers)
                           (active-list-delete-last active)
                           (active-list-insert-sorted active entry :key #'third))
                         (push entry spilled-registers))))))
    (labels ((split-allocation (start end)
               (loop for size in '(1 2 4 . #0=(8 . #0#))
                     while (< start end)
                     when (or (= size 8) (/= 0 (mod start (ash size 1))))
                       collect (prog1 (cons start size)
                                 (incf start size)))))
      (loop for entry in (sort spilled-registers #'< :key #'second)
            for (num start end) = entry
            for type = (gethash num types)
            with stack-top = 0
            with free = nil
            with active = (make-active-list)
            do (loop for entry in (active-list-delete-until active (lambda (entry) (<= start (third entry))))
                     do (push (cons (second (gethash (first entry) allocations)) (size (gethash (first entry) types))) free))
            do (active-list-insert-sorted active entry :key #'third)
            do (loop for (free-cons . prev-cons) = (cons free nil) then (cons (cdr free-cons) free-cons)
                     while free-cons
                     for (offset . size) = (car free-cons)
                     when (<= (size type) size)
                       do (progn
                            (if prev-cons
                                (setf (cdr prev-cons) (cdr free-cons))
                                (pop free))
                            (setf free (nconc (split-allocation (+ offset (size type)) (+ offset size)) free))
                            (setf (gethash num allocations) (list :stack offset))
                            (return))
                     finally (let ((offset (align-extend stack-top (align type))))
                               (setf free (nconc (split-allocation stack-top offset) free))
                               (setf (gethash num allocations) (list :stack offset))
                               (setf stack-top (+ offset (size type)))))))
    (values allocations regs-used stack-top)))

(defparameter *instructions* nil)
(defparameter *allocations* nil)

(defun codegen-append-inst (val)
  (vector-push-extend val *instructions*))

(defun codegen-load/store-non-volatile (reg-nums load)
  (loop with regs = (loop for reg-num in reg-nums
                            collect (make-register :number reg-num :wide t))
          with i = 0
          while (cdr regs)
          do (progn
               (codegen-append-inst `(,(if load 'ldp 'stp) ,(first regs) ,(second regs) ,sp ,(* 8 (+ 2 i))))
               (incf i 2)
               (setf regs (cddr regs)))
          finally (when regs (codegen-append-inst `(,(if load 'ldr 'str) ,(first regs) ,sp ,(* 8 (+ 2 i)))))))

(defmacro with-tmp-register ((ret type) &body body)
  `(let ((,ret (make-virtual-register :number (make-symbol "TMP") :type ,type)))
     ,@body))

(defun expand-insts-phase1 (insts)
  (let ((*instructions* (make-dynamic-array))
        (stack-top 0)
        (stack-align 1))
    (loop for inst-orig across insts
          for inst = (copy-tree inst-orig)
          do (if (consp inst)
                 (let* ((wide (loop for arg in (get-arguments inst (lambda (plist) (getf plist :arith)))
                                      thereis (widep (op-type arg))))
                        (out-map (loop for arg in (get-arguments inst (lambda (plist) (and (getf plist :arith)
                                                                                           (getf plist :out))))
                                       for signed = (signedp (op-type arg))
                                       for out-type = (cond
                                                        ((and wide signed) 'i64)
                                                        (wide 'u64)
                                                        (signed 'i32)
                                                        (t 'u32))
                                       unless (ctype-equal out-type (op-type arg))
                                         collect (with-tmp-register (tmp out-type)
                                                   (cons arg tmp)))))
                   (set-arguments inst
                                  (lambda (plist arg) (declare (ignore arg)) (getf plist :alloc))
                                  (lambda (arg)
                                    (let ((align (align arg)))
                                      (setf stack-top (align-extend stack-top align)
                                            stack-align (max stack-align align)))
                                    (prog1 stack-top
                                      (incf stack-top (size arg)))))
                   (when wide
                     (set-arguments inst
                                    (lambda (plist arg)
                                      (and (getf plist :arith)
                                           (getf plist :in)
                                           (not (widep (op-type arg)))))
                                    (lambda (arg)
                                      (with-tmp-register (tmp (if (signedp (op-type arg)) 'i64 'u64))
                                        (codegen-append-inst (list 'coerce tmp arg))
                                        tmp))))
                   (set-arguments inst
                                  (lambda (plist arg) (declare (ignore arg)) (and (getf plist :arith) (getf plist :out)))
                                  (lambda (arg) (or (cdr (assoc arg out-map :test #'eq)) arg)))
                   (codegen-append-inst inst)
                   (loop for (arg . tmp) in out-map
                         do (codegen-append-inst `(coerce ,arg ,tmp))))
                 (codegen-append-inst inst)))
    (values *instructions* stack-top stack-align)))

(defun shrink-register (reg)
  (make-register :number (register-number reg) :wide nil :special (register-special reg)))

(defparameter *num-tmp-registers* 4)
(defparameter *tmp-registers-start* 15)

(defun codegen (insts)
  (multiple-value-bind (insts alloc-size alloc-align) (expand-insts-phase1 insts)
    (multiple-value-bind (allocations regs-used spill-size)
        (analyse-lifetimes (separate-blocks insts))
      (let* ((*instructions* (make-dynamic-array))
             (spill-start (* 8 (+ 2 (length regs-used))))
             (*alloc-start* (align-extend (+ spill-start spill-size) alloc-align))
             (frame-size (align-extend (+ *alloc-start* alloc-size) 16)))
        (codegen-append-inst `(stp ,x29 ,x30 ,sp ,(- frame-size) !))
        (codegen-append-inst `(mov ,x29 ,sp))
        (codegen-load/store-non-volatile regs-used nil)
        (loop for inst across insts
              with *ret-sym* = (make-symbol "RET")
              do (if (consp inst)
                     (let ((types (loop-toplevel-arguments inst (arg-spec arg)
                                    collect (if (op-p arg) (op-type arg) nil)))
                           (pre-insts (make-dynamic-array))
                           (post-insts (make-dynamic-array))
                           (tmp-register *tmp-registers-start*))

                       (labels ((get-tmp-register ()
                                  (assert (< (- tmp-register *tmp-registers-start*) *num-tmp-registers*))
                                  (prog1 tmp-register
                                    (incf tmp-register))))
                         (let (plist (i 0))
                           (set-arguments
                            inst
                            (lambda (plist-new arg)
                              (declare (ignore arg))
                              (when (not (eq plist plist-new))
                                (setf i 0))
                              (setf plist plist-new) t)
                            (lambda (arg)
                              (let ((store-volatile (getf plist :store-volatile))
                                    (load-volatile (getf plist :load-volatile))
                                    (out (getf plist :out))
                                    (in (getf plist :in)))
                                (prog1
                                    (cond
                                      ((or store-volatile load-volatile)
                                       (assert (<= i 7))
                                       (let* ((type (op-type arg))
                                              (wide (widep type))
                                              (op (make-register :number i :wide wide)))
                                         (etypecase arg
                                           (virtual-register
                                            (let ((allocation (gethash-strict (virtual-register-number arg) allocations)))
                                              (ecase (first allocation)
                                                (:register
                                                 (vector-push-extend
                                                  (if store-volatile
                                                      (list 'mov op (make-register :number (second allocation) :wide wide))
                                                      (list 'mov (make-register :number (second allocation) :wide wide) op))
                                                  (if store-volatile pre-insts post-insts)))
                                                (:stack
                                                 (vector-push-extend
                                                  (list (if store-volatile
                                                            (load-name-from-type type)
                                                            (store-name-from-type type))
                                                        op sp (+ (second allocation) spill-start))
                                                  (if store-volatile pre-insts post-insts))))))
                                           (constant
                                            (assert store-volatile)
                                            (loop for inst in (mov-constant-insts op (constant-value arg))
                                                  do (vector-push-extend inst pre-insts))))
                                         op))
                                      ((or in out)
                                       (let* ((type (op-type arg))
                                              (wide (widep type)))
                                         (etypecase arg
                                           (virtual-register
                                            (let ((allocation (gethash-strict (virtual-register-number arg) allocations)))
                                              (ecase (first allocation)
                                                (:register
                                                 (make-register :number (second allocation) :wide wide))
                                                (:stack
                                                 (let ((op (make-register :number (get-tmp-register) :wide wide)))
                                                   (vector-push-extend
                                                    (list (if in (load-name-from-type type) (store-name-from-type type))
                                                          op sp (+ (second allocation) spill-start))
                                                    (if in pre-insts post-insts))
                                                   op)))))
                                           (constant
                                            (assert in)
                                            (if (getf plist :const-reg)
                                                (let ((op (make-register :number (get-tmp-register) :wide wide)))
                                                  (loop for inst in (mov-constant-insts op (constant-value arg))
                                                        do (vector-push-extend inst pre-insts))
                                                  op)
                                                (constant-value arg))))))
                                      (t arg))
                                  (incf i)))))))
                     
                       (loop for inst across pre-insts
                             do (codegen-append-inst inst))

                       (loop for inst in (apply (ir-plist-get inst :expand)
                                                (append (rest inst) types))
                             do (codegen-append-inst inst))

                       (loop for inst across post-insts
                             do (codegen-append-inst inst)))
                     (codegen-append-inst inst))
              finally (codegen-append-inst *ret-sym*))
        (codegen-load/store-non-volatile regs-used t)
        (codegen-append-inst `(ldp ,x29 ,x30 ,sp ,frame-size ?))
        (codegen-append-inst `(ret))
        *instructions*))))

