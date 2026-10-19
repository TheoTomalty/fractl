(defpackage #:asm
  (:use #:cl #:elf #:utils)
  (:export #:register #:register-p #:register-number #:register-wide #:register-special #:make-register
           #:w-register #:wsp-register #:wzr-register #:x-register #:xsp-register #:xzr-register
           #:wsp #:wzr #:xsp #:xzr #:sp #:zr
           #:w0 #:w1 #:w2 #:w3 #:w4 #:w5 #:w6 #:w7 #:w8 #:w9
           #:w10 #:w11 #:w12 #:w13 #:w14 #:w15 #:w16 #:w17 #:w18 #:w19
           #:w20 #:w21 #:w22 #:w23 #:w24 #:w25 #:w26 #:w27 #:w28 #:w29 #:w30
           #:x0 #:x1 #:x2 #:x3 #:x4 #:x5 #:x6 #:x7 #:x8 #:x9
           #:x10 #:x11 #:x12 #:x13 #:x14 #:x15 #:x16 #:x17 #:x18 #:x19
           #:x20 #:x21 #:x22 #:x23 #:x24 #:x25 #:x26 #:x27 #:x28 #:x29 #:x30
           #:encode-inst #:relocation-code #:link #:instruction-p

           #:R_AARCH64_ABS64 #:R_AARCH64_ABS32 #:R_AARCH64_ABS16
           #:R_AARCH64_PREL64 #:R_AARCH64_PREL32 #:R_AARCH64_PREL16
           #:R_AARCH64_MOVW_UABS_G0 #:R_AARCH64_MOVW_UABS_G0_NC
           #:R_AARCH64_MOVW_UABS_G1 #:R_AARCH64_MOVW_UABS_G1_NC
           #:R_AARCH64_MOVW_UABS_G2 #:R_AARCH64_MOVW_UABS_G2_NC
           #:R_AARCH64_MOVW_UABS_G3 #:R_AARCH64_MOVW_SABS_G0
           #:R_AARCH64_MOVW_SABS_G1 #:R_AARCH64_MOVW_SABS_G2
           #:R_AARCH64_LD_PREL_LO19 #:R_AARCH64_ADR_PREL_LO21
           #:R_AARCH64_ADR_PREL_PG_HI21 #:R_AARCH64_ADR_PREL_PG_HI21_NC
           #:R_AARCH64_ADD_ABS_LO12_NC #:R_AARCH64_LDST8_ABS_LO12_NC
           #:R_AARCH64_LDST16_ABS_LO12_NC #:R_AARCH64_LDST32_ABS_LO12_NC
           #:R_AARCH64_LDST64_ABS_LO12_NC #:R_AARCH64_LDST128_ABS_LO12_NC
           #:R_AARCH64_TSTBR14 #:R_AARCH64_CONDBR19
           #:R_AARCH64_JUMP26 #:R_AARCH64_CALL26
           
           #:adr #:adrp #:add #:adds #:sub #:subs #:cmp #:cmn #:andr #:ands #:eor
           #:orr #:tst #:movz #:movn #:movk #:mov #:bfm #:sbfm #:ubfm #:bfc #:bfi
           #:bfxil #:sbfiz #:sbfx #:ubfiz #:ubfx #:sxtb #:sxth #:sxtw #:uxtb #:uxth #:bc
           #:bcc #:svc #:hvc #:smc #:brk #:hlt #:br #:blr #:ret #:eret #:drps
           #:b #:bl #:cbz #:cbnz #:tbz #:tbnz #:ldr #:ldrsw #:stp #:ldp #:ldpsw
           #:sturb #:ldurb #:ldursb #:sturh #:ldurh #:ldursh #:stur #:ldur #:ldursw #:strb #:ldrb
           #:ldrsb #:strh #:ldrh #:ldrsh #:str #:udiv #:sdiv #:lslv #:lsrv #:asrv #:rorv
           #:rbit #:rev16 #:rev #:rev32 #:clz #:cls #:bic #:orn #:eon #:bics #:neg
           #:negs #:adc #:adcs #:sbc #:sbcs #:ccmn #:ccmp #:csel #:csinc #:csinv #:csneg
           #:cinc #:cset #:cinv #:csetm #:cneg #:madd #:msub #:smaddl #:smsubl #:smulh #:umaddl
           #:umsubl #:umulh #:mul #:mneg #:smull #:smnegl #:umull #:umnegl

           #:ne #:ge #:le #:gt #:lt #:! #:? #:lsl
           #:uxtb #:uxth #:uxtw #:uxtx
           #:sxtb #:sxth #:sxtw #:sxtx))

(in-package #:asm)


(declaim (optimize debug))


(defstruct register
  (number 0 :type integer :read-only t)
  (wide t :type boolean :read-only t)
  (special nil :type symbol :read-only t))

(defun register-sp-p (reg)
  (eql 'sp (register-special reg)))

(defun register-zr-p (reg)
  (eql 'zr (register-special reg)))

(deftype w-register ()
  `(and register
        (not (satisfies register-wide))))
(deftype wsp-register ()
  `(and register
        (not (satisfies register-wide))
        (or (not (satisfies register-special))
            (satisfies register-sp-p))))
(deftype wzr-register ()
  `(and register
        (not (satisfies register-wide))
        (or (not (satisfies register-special))
            (satisfies register-zr-p))))
(deftype x-register ()
  `(and register
        (satisfies register-wide)))
(deftype xsp-register ()
  `(and register
        (satisfies register-wide)
        (or (not (satisfies register-special))
            (satisfies register-sp-p))))
(deftype xzr-register ()
  `(and register
        (satisfies register-wide)
        (or (not (satisfies register-special))
            (satisfies register-zr-p))))

(defparameter xsp (make-register :number 31 :wide t :special 'sp))
(defparameter xzr (make-register :number 31 :wide t :special 'zr))
(defparameter wsp (make-register :number 31 :wide nil :special 'sp))
(defparameter wzr (make-register :number 31 :wide nil :special 'zr))
(defparameter sp xsp)
(defparameter zr xzr)
#.`(progn
     ,@(loop for i from 0 to 30
             append (list
                     `(defparameter ,(intern (format nil "X~d" i))
                        (make-register :number ,i :wide t))
                     `(defparameter ,(intern (format nil "W~d" i))
                        (make-register :number ,i :wide nil)))))

(defmethod print-object ((reg register) stream)
  (if (register-special reg)
      (write-string (symbol-name (register-special reg)) stream)
      (progn
        (if (register-wide reg)
            (write-char #\x stream)
            (write-char #\w stream))
        (format stream "~d" (register-number reg)))))

(defun integer-to-bitvector (n &key size)
  (let (ret)
    (loop while (> n 0)
          do (multiple-value-bind (div rem) (floor n 2)
               (setf n div)
               (push rem ret)))
    (let ((ret-vect (coerce ret '(vector bit))))
      (if size
          (progn
            (assert (<= (length ret-vect) size))
            (concatenate '(vector bit)
                         (make-array (- size (length ret-vect)) :element-type 'bit)
                         ret-vect))
          ret-vect))))

(defun bitvector-to-integer (vect)
  (reduce (lambda (x y) (+ (* 2 x) y)) vect))


(defparameter *relocation-ops* (make-hash-table))
(defparameter *relocation-codes* (make-hash-table))

(defun relocation-code (relocation-type)
  (gethash-strict relocation-type *relocation-codes*))

(defmacro define-relocation (name code bin bout operation &body body)
  (declare (ignore bin bout))
  `(progn
     (setf (gethash ',name *relocation-codes*) ,code)
     (setf (gethash ',name *relocation-ops*)
           (cons (lambda (s a p)
                   (declare (ignorable s a p))
                   ,operation)
                 (lambda (i x)
                   (declare (ignorable i x))
                   ,@body)))))

(defun link (type i s a p)
  (destructuring-bind (val-fn . encode-fn)
      (gethash type *relocation-ops*)
    (funcall encode-fn i (funcall val-fn s a p))))

(defun bits (x end start)
  (ash (logand (ash (1- (ash 1 (1+ (- end start))))
                    start)
               x)
       (- start)))

(defparameter *page-size* 4096)

(defun page (x)
  (logand x (lognot (1- *page-size*))))

(defun check-int (x exp)
  (assert (and (<= (- (expt 2 exp)) x)
               (< x (expt 2 exp)))))

(defun check-uint (x exp)
  (assert (and (<= 0 x)
               (< x (expt 2 exp)))))

(defun check-int-uint (x exp)
  (assert (and (<= (- (expt 2 exp)) x)
               (< x (expt 2 (1+ exp))))))

;; (defstruct (relocation (:type list))
;;   (type nil :type (and symbol (not null)))
;;   (symbol nil :type symbol)
;;   (offset 0 :type integer)
;;   (addend 0 :type integer))

(defparameter *encoders* (make-hash-table))
(defparameter *decoders* (make-hash-table))
(defparameter *bases* (make-hash-table))

(defmacro define-inst-type (name &body fields)
  `(progn
     (setf (gethash ',name *encoders*) (make-hash-table))
     (setf (gethash ',name *decoders*) (make-hash-table))
     ,@(loop for field in (reverse fields)
             with offset = 0
             with base = 0
             append (destructuring-bind (field-name field-size) field
                      (prog1
                          (etypecase field-name
                            (symbol
                             (let* ((mask-unshifted (1- (ash 1 field-size)))
                                    (mask (ash mask-unshifted offset))
                                    (mask-inv (lognot mask))
                                    (decode-name (intern (format nil "DECODE-~a-~a" (symbol-name name) (symbol-name field-name))))
                                    (encode-name (intern (format nil "ENCODE-~a-~a" (symbol-name name) (symbol-name field-name)))))
                               `((defun ,decode-name (i)
                                   (ash (logand i ,mask)
                                        ,(- offset)))
                                 (defun ,encode-name (i x)
                                   (logior (logand i ,mask-inv)
                                           (ash (logand x ,mask-unshifted) ,offset)))
                                 (setf (gethash ',field-name (gethash ',name *encoders*))
                                       #',encode-name)
                                 (setf (gethash ',field-name (gethash ',name *decoders*))
                                       #',decode-name))))
                            (integer
                             (assert (and (<= 0 field-name)
                                          (< field-name (ash 1 field-size))))
                             (setf base (logior base (ash field-name offset)))
                             nil))
                        (incf offset field-size)))
               into defuns
             finally (progn
                       (assert (= offset 32))
                       (return `((setf (gethash ',name *bases*) ',base)
                                 ,@defuns))))))

(defun encode-pc-rel-addressing-imm (i x)
  (encode-pc-rel-addressing-immhi
   (encode-pc-rel-addressing-immlo i (bits x 1 0))
   (bits x 20 2)))

(defun decode-pc-rel-addressing-imm (i)
  (logand (ash (decode-pc-rel-addressing-immhi i) 2)
          (decode-pc-rel-addressing-immlo i)))

(defparameter *instructions* (make-hash-table))

(defun instruction-p (sym)
  (nth-value 1 (gethash sym *instructions*)))

(defun (setf instruction) (val name i)
  (let ((array (gethash name *instructions*)))
    (unless array
      (setf array (make-array (1+ i) :initial-element nil :adjustable t))
      (setf (gethash name *instructions*) array))
    (unless (< i (length array))
      (adjust-array array (1+ i) :initial-element nil))
    (setf (aref array i) val)))

(defmacro with-inst-type (name &body body)
  `(let ((encoders (gethash ',name *encoders*))
         (decoders (gethash ',name *decoders*))
         (base (gethash ',name *bases*)))
     (assert encoders)
     (assert decoders)
     (assert base)
     ,@body))

(defparameter *encoder* nil)

(defstruct encoder
  (args nil :type list)
  (inst 0 :type fixnum)
  (links nil :type list)
  (encoders nil :type hash-table)
  (decoders nil :type hash-table))

(define-condition abort-encoder (error) ())

(defmacro encoder-assert (condition)
  `(unless ,condition (error 'abort-encoder)))

(defun encoder-set-field (name value)
  (setf (encoder-inst *encoder*)
          (funcall (gethash name (encoder-encoders *encoder*))
                   (encoder-inst *encoder*)
                   value)))

(defun encoder-pop-arg (&optional type)
  (let ((ret (pop (encoder-args *encoder*))))
    (encoder-assert
     (and ret (or (not type)
                  (typep ret type))))
    ret))

(defun encoder-more-args-p ()
  (not (null (encoder-args *encoder*))))

(defun encoder-encode-reg (field specifier)
  (let ((arg (encoder-pop-arg 'register))
        (test-wide (ecase specifier
                     ((wsp xsp wzr xzr) t)
                     ((sp zr) nil)))
        (wide (case specifier
                ((xsp xzr) t)))
        (special (case specifier
                   ((wsp xsp sp) 'sp)
                   ((wzr xzr zr) 'zr))))
    (encoder-assert
     (and (or (not test-wide)
              (eql (register-wide arg) wide))
          (<= 0 (register-number arg))
          (or (< (register-number arg) 31)
              (and (= (register-number arg) 31)
                   (eql (register-special arg)
                        special)))))
    (encoder-set-field field (register-number arg))))

(defun encoder-encode-const (sign-specifier size field &optional (shift 1))
  (declare (optimize (debug 3)))
  (let ((signedp (ecase sign-specifier
                   ((u) nil)
                   ((i) t)))
        (arg (encoder-pop-arg 'integer)))
    (encoder-assert
     (if signedp
         (and (<= (- (ash 1 (1- size))) arg)
              (< arg (ash 1 (1- size))))
         (and (<= 0 arg)
              (< arg (ash 1 size)))))
    (encoder-assert (= 0 (mod arg shift)))
    (setf arg (/ arg shift))
    (encoder-set-field field arg)))

(defun encoder-encode-bitmask (max-size)
  (let* ((bitmask (encoder-pop-arg 'integer))
         (vect (integer-to-bitvector bitmask :size max-size))
         (segment (loop named loop
                        for size = 2 then (ash size 1)
                        while (>= max-size size)
                        do (let ((segments (loop for i from 0 below (/ max-size size)
                                                 collect (subseq vect
                                                                 (* i size)
                                                                 (* (1+ i) size)))))
                             (unless (some (lambda (x) (not (equal x (car segments))))
                                           (cdr segments))
                               (return-from loop (car segments))))))
         (size (length segment))
         (inv (not (zerop (aref segment 0))))
         (segment (if inv (bit-not segment) segment))
         (start (position-if-not #'zerop segment)))
    (encoder-assert (<= 0 bitmask))
    (encoder-assert start) ;; Check that there is at least one 1
    (let* ((end (1+ (position-if-not #'zerop segment :from-end t)))
           (sub-segment (subseq segment start end)))
      (encoder-assert (not (some #'zerop sub-segment)))
      ;; Check that the range bookended by 1's contains only 1's
      (let* ((ones (if inv
                       (- size (length sub-segment))
                       (length sub-segment)))
             (immr (if inv
                       (- size start)
                       (- size end)))
             (N (if (eql 64 size) 1 0))
             (imms (if (eql 64 size)
                       (1- ones)
                       (logior (- #b1000000 (* size 2))
                               (1- ones)))))
        (ecase max-size
          (32 nil)
          (64 (encoder-set-field 'N N)))
        (encoder-set-field 'immr immr)
        (encoder-set-field 'imms imms)))))

(defun encoder-encode-shift1 (field)
  (when (encoder-more-args-p)
    (let ((first (encoder-pop-arg 'symbol))
          (second (encoder-pop-arg 'integer)))
      (unless (and (eql 'lsl first)
                   (or (eql 0 second)
                       (eql 12 second)))
        (error 'abort-encoder))
      (encoder-set-field field (/ second 12)))))

(defun encoder-encode-shift2 (field max)
  (when (encoder-more-args-p)
    (let ((first (encoder-pop-arg 'symbol))
          (second (encoder-pop-arg 'integer)))
      (unless (and (eql 'lsl first)
                   (= 0 (mod second 16))
                   (< (/ second 16) (/ max 16)))
        (error 'abort-encoder))
      (encoder-set-field field (/ second 16)))))

(defun encoder-encode-shift3 (field0 field1 size)
  (when (encoder-more-args-p)
    (let ((shift (encoder-pop-arg 'symbol))
          (amount (encoder-pop-arg `(integer 0 ,(1- size)))))
      (encoder-set-field field0 (ecase shift
                                  (lsl #b00)
                                  (lsr #b01)
                                  (asr #b10)
                                  (ror #b11)))
      (encoder-set-field field1 amount))))

(defun encoder-encode-shift4 (field0 field1 size)
  (when (encoder-more-args-p)
    (let ((shift (encoder-pop-arg 'symbol))
          (amount (encoder-pop-arg `(integer 0 ,(1- size)))))
      (encoder-set-field field0 (ecase shift
                                  (lsl #b00)
                                  (lsr #b01)
                                  (asr #b10)))
      (encoder-set-field field1 amount))))

(defun encoder-encode-condition (field)
  (let ((arg (encoder-pop-arg 'symbol)))
    (encoder-set-field
     field
     (case arg
       (eq #b0000)
       (ne #b0001)
       (ge #b1010)
       (lt #b1011)
       (gt #b1100)
       (le #b1101)
       (t (error 'abort-encoder))))))

(defun invert-condition (c)
  (ecase c
    (eq 'ne)
    (ne 'eq)
    (ge 'lt)
    (lt 'ge)
    (gt 'le)
    (le 'gt)))

(defun encoder-encode-link (relocation-type)
  (let ((arg (encoder-pop-arg)))
    (typecase arg
      (symbol (push (list relocation-type arg)
                    (encoder-links *encoder*)))
      (integer (setf (encoder-inst *encoder*)
                     (funcall (cdr (gethash relocation-type *relocation-ops*))
                              (encoder-inst *encoder*)
                              arg)))
      (t (error 'abort-encoder)))))

(defun encoder-encode-test-reg-bit (field0 field1 field2)
  (let ((reg (encoder-pop-arg 'register))
        (bit (encoder-pop-arg 'integer)))
    (encoder-assert (<= 0 bit))
    (if (register-wide reg)
        (encoder-assert (< bit 64))
        (encoder-assert (< bit 32)))
    (push reg (encoder-args *encoder*))
    (encoder-encode-reg field0 'zr)
    (encoder-set-field field1 (if (>= bit 32) 1 0))
    (encoder-set-field field2 (mod bit 32))))

(defun encoder-encode-pre/post (field &optional default)
  (if (null (encoder-args *encoder*))
      (when default
        (encoder-set-field field default))
      (let ((arg (encoder-pop-arg 'symbol)))
        (case arg
          (! (encoder-set-field field #b11))
          (? (encoder-set-field field #b01))
          (t (error 'abort-encoder))))))

(defun encoder-encode-reg-extend1 (field shift)
  (let ((reg (encoder-pop-arg 'register))
        (S 0)
        (option #b011)
        (optionalp nil))
    (when (encoder-more-args-p)
      (multiple-value-setq (option optionalp)
        (ecase (encoder-pop-arg 'symbol)
          (uxtw (values #b010 t))
          (lsl  (values #b011 nil))
          (sxtw (values #b110 t))
          (sxtx (values #b111 t))))
      (when (or (not optionalp) (encoder-more-args-p))
        (when (or (< 0 (encoder-pop-arg `(or (integer 0 0)
                                             (integer ,shift ,shift))))
                  (= 0 shift))
          (setf S 1))))
    (encoder-assert (eql (register-wide reg)
                         (logbitp 0 option)))
    (push reg (encoder-args *encoder*))
    (encoder-encode-reg field 'zr)
    (encoder-set-field 'option option)
    (encoder-set-field 'S S)))

(defun encoder-encode-reg-extend2 (field size)
  (let ((reg (encoder-pop-arg 'register))
        (option (ecase size (32 #b010) (64 #b011)))
        (optionalp nil))
    (when (encoder-more-args-p)
      (multiple-value-setq (option optionalp)
        (ecase (encoder-pop-arg 'symbol)
          (lsl (values option nil))
          (uxtb (values #b000 t))
          (uxth (values #b001 t))
          (uxtw (values #b010 t))
          (uxtx (values #b011 t))
          (sxtb (values #b100 t))
          (sxth (values #b101 t))
          (sxtw (values #b110 t))
          (sxtx (values #b111 t))))
      (when (or (not optionalp) (encoder-more-args-p))
        (encoder-set-field 'imm3 (encoder-pop-arg '(integer 0 4)))))
    (encoder-assert (eql (register-wide reg)
                         (and (logbitp 0 option)
                              (logbitp 1 option))))
    (push reg (encoder-args *encoder*))
    (encoder-encode-reg field 'zr)
    (encoder-set-field 'option option)))

(defun encoder-encode-mov-const (imm16 hw size &key inverted)
  (let ((arg (encoder-pop-arg 'integer)))
    (when inverted
      (setf arg (lognot arg)))
    (setf arg (logand arg (1- (ash 1 size))))
    (loop for i from 0 below (/ size 16)
          for shift = 0 then (1+ shift)
          while (and (/= 0 (logandc2 arg (1- (ash 1 16))))
                     (= 0 (logand arg (1- (ash 1 16)))))
          do (setf arg (ash arg -16))
          finally (progn
                    (encoder-assert (= 0 (logandc2 arg (1- (ash 1 16)))))
                    (encoder-set-field imm16 arg)
                    (encoder-set-field hw shift)))))

(defmacro define-inst (name i fields &body body)
  `(setf (instruction ',name ,i)
         (lambda (&rest args)
           (let ((*encoder* (make-encoder :args args :inst base :encoders encoders :decoders decoders)))
             ,@(loop for pair in fields
                     collect (destructuring-bind (field val) pair
                               `(encoder-set-field ',field ,val)))
             (macrolet ,(loop for name in '(reg const bitmask shift1 shift2 shift3 shift4 condition link test-reg-bit pre/post reg-extend1 reg-extend2 mov-const)
                              for long-name = (intern (format nil "ENCODER-ENCODE-~a" (symbol-name name)))
                              collect `(,name (&rest args)
                                              `(apply #',',long-name ',args)))
               ,@body
               (encoder-assert (null (encoder-args *encoder*)))
               (values (encoder-inst *encoder*)
                       (encoder-links *encoder*)))))))

(defmacro define-alias (name i args &body body)
  `(setf (instruction ',name ,i)
         (lambda (,@(loop for arg in args
                          collect (if (consp arg)
                                      (first arg)
                                      arg)))
           ,@(loop for arg in args
                   when (consp arg)
                     collect (destructuring-bind (arg-name arg-type) arg
                               `(encoder-assert
                                 (typep ,arg-name
                                        ,(if (and (listp arg-type)
                                                  (eql 'sb-int:quasiquote (first arg-type)))
                                             arg-type
                                             `',arg-type)))))
           (apply #'encode-inst (progn ,@body)))))

(defun encode-inst (name &rest args)
  (let ((array (gethash name *instructions*)))
    (unless array
      (error "no such instruction: ~s" name))
    (loop for fn across array
          do (handler-case (return (apply fn args))
               (abort-encoder ()))
          finally (error 'abort-encoder))))


(define-inst-type pc-rel-addressing
  (op 1) (immlo 2) (#b10000 5) (immhi 19) (Rd 5))
(define-inst-type add/subtract-immediate
  (sf 1) (op 1) (S 1) (#b100010 6) (sh 1) (imm12 12) (Rn 5) (Rd 5))
(define-inst-type logical-immediate
  (sf 1) (opc 2) (#b100100 6) (N 1) (immr 6) (imms 6) (Rn 5) (Rd 5))
(define-inst-type move-wide-immediate
  (sf 1) (opc 2) (#b100101 6) (hw 2) (imm16 16) (Rd 5))
(define-inst-type bitfield
  (sf 1) (opc 2) (#b100110 6) (N 1) (immr 6) (imms 6) (Rn 5) (Rd 5))
(define-inst-type conditional-branch-immediate
  (#b0101010 7) (o1 1) (imm19 19) (o0 1) (cond 4))
(define-inst-type exception-generation
  (#b11010100 8) (opc 3) (imm16 16) (op2 3) (LL 2))
(define-inst-type unconditional-branch-register
  (#b1101011 7) (opc 4) (op2 5) (op3 6) (Rn 5) (op4 5))
(define-inst-type unconditional-branch-immediate
  (op 1) (#b00101 5) (imm26 26))
(define-inst-type compare-and-branch-immediate
  (sf 1) (#b011010 6) (op 1) (imm19 19) (Rt 5))
(define-inst-type test-and-branch-immediate
  (b5 1) (#b011011 6) (op 1) (b40 5) (imm14 14) (Rt 5))
(define-inst-type load-register-literal
  (opc 2) (#b011 3) (V 1) (#b00 2) (imm19 19) (Rt 5))
(define-inst-type load/store-register-pair
  (opc 2) (#b101 3) (V 1) (ind 3) (L 1) (imm7 7) (Rt2 5) (Rn 5) (Rt 5))
(define-inst-type load/store-register-register
  (size 2) (#b111 3) (V 1) (#b00 2) (opc 2) (#b1 1) (Rm 5) (option 3) (S 1) (#b10 2) (Rn 5) (Rt 5))
(define-inst-type load/store-register-unsigned-immediate
  (size 2) (#b111 3) (V 1) (#b01 2) (opc 2) (imm12 12) (Rn 5) (Rt 5))
(define-inst-type load/store-register-unscaled-immediate
  (size 2) (#b111 3) (V 1) (#b00 2) (opc 2) (#b0 1) (imm9 9) (ind 2) (Rn 5) (Rt 5))
(define-inst-type data-processing-2-source
  (sf 1) (#b0 1) (S 1) (#b11010110 8) (Rm 5) (opcode 6) (Rn 5) (Rd 5))
(define-inst-type data-processing-1-source
  (sf 1) (#b1 1) (S 1) (#b11010110 8) (opcode2 5) (opcode 6) (Rn 5) (Rd 5))
(define-inst-type logical-shifted-register
  (sf 1) (opc 2) (#b01010 5) (shift 2) (N 1) (Rm 5) (imm6 6) (Rn 5) (Rd 5))
(define-inst-type add/subtract-shifted-register
  (sf 1) (op 1) (S 1) (#b01011 5) (shift 2) (#b0 1) (Rm 5) (imm6 6) (Rn 5) (Rd 5))
(define-inst-type add/subtract-extended-register
  (sf 1) (op 1) (S 1) (#b01011 5) (opt 2) (#b1 1) (Rm 5) (option 3) (imm3 3) (Rn 5) (Rd 5))
(define-inst-type add/subtract-with-carry
  (sf 1) (op 1) (S 1) (#b11010000 8) (Rm 5) (#b000000 6) (Rn 5) (Rd 5))
(define-inst-type conditional-compare-register
  (sf 1) (op 1) (S 1) (#b11010010 8) (Rm 5) (cond 4) (#b0 1) (o2 1) (Rn 5) (o3 1) (nzcv 4))
(define-inst-type conditional-compare-immediate
  (sf 1) (op 1) (S 1) (#b11010010 8) (imm5 5) (cond 4) (#b1 1) (o2 1) (Rn 5) (o3 1) (nzcv 4))
(define-inst-type conditional-select
  (sf 1) (op 1) (S 1) (#b11010100 8) (Rm 5) (cond 4) (op2 2) (Rn 5) (Rd 5))
(define-inst-type data-processing-3-source
  (sf 1) (op54 2) (#b11011 5) (op31 3) (Rm 5) (o0 1) (Ra 5) (Rn 5) (Rd 5))

(define-relocation R_AARCH64_ABS64 257 0 8 (+ s a) (check-int-uint x 63) x)
(define-relocation R_AARCH64_ABS32 258 0 4 (+ s a) (check-int-uint x 31) x)
(define-relocation R_AARCH64_ABS16 259 0 2 (+ s a) (check-int-uint x 15) x)
(define-relocation R_AARCH64_PREL64 260 0 8 (- (+ s a) p) (check-int-uint x 63) x)
(define-relocation R_AARCH64_PREL32 261 0 4 (- (+ s a) p) (check-int-uint x 31) x)
(define-relocation R_AARCH64_PREL16 262 0 2 (- (+ s a) p) (check-int-uint x 15) x)

(define-relocation R_AARCH64_MOVW_UABS_G0 263 4 4 (+ s a)
  (check-uint x 16)
  (encode-move-wide-immediate-imm16 i (bits x 15 0)))
(define-relocation R_AARCH64_MOVW_UABS_G0_NC 264 4 4 (+ s a)
  (encode-move-wide-immediate-imm16 i (bits x 15 0)))
(define-relocation R_AARCH64_MOVW_UABS_G1 265 4 4 (+ s a)
  (check-uint x 32)
  (encode-move-wide-immediate-imm16 i (bits x 31 16)))
(define-relocation R_AARCH64_MOVW_UABS_G1_NC 266 4 4 (+ s a)
  (encode-move-wide-immediate-imm16 i (bits x 31 16)))
(define-relocation R_AARCH64_MOVW_UABS_G2 267 4 4 (+ s a)
  (check-uint x 48)
  (encode-move-wide-immediate-imm16 i (bits x 47 32)))
(define-relocation R_AARCH64_MOVW_UABS_G2_NC 268 4 4 (+ s a)
  (encode-move-wide-immediate-imm16 i (bits x 47 32)))
(define-relocation R_AARCH64_MOVW_UABS_G3 269 4 4 (+ s a)
  (check-uint x 64)
  (encode-move-wide-immediate-imm16 i (bits x 63 48)))

(define-relocation R_AARCH64_MOVW_SABS_G0 270 4 4 (+ s a)
  (check-int x 16)
  (if (<= 0 x)
      (encode-move-wide-immediate-imm16
       (encode-move-wide-immediate-opc i #b10)
       (bits x 15 0))
      (encode-move-wide-immediate-imm16
       (encode-move-wide-immediate-opc i #b00)
       (not (bits x 15 0)))))
(define-relocation R_AARCH64_MOVW_SABS_G1 271 4 4 (+ s a)
  (check-int x 32)
  (if (<= 0 x)
      (encode-move-wide-immediate-imm16
       (encode-move-wide-immediate-opc i #b10)
       (bits x 31 16))
      (encode-move-wide-immediate-imm16
       (encode-move-wide-immediate-opc i #b00)
       (not (bits x 31 16)))))
(define-relocation R_AARCH64_MOVW_SABS_G2 272 4 4 (+ s a)
  (check-int x 48)
  (if (<= 0 x)
      (encode-move-wide-immediate-imm16
       (encode-move-wide-immediate-opc i #b10)
       (bits x 47 32))
      (encode-move-wide-immediate-imm16
       (encode-move-wide-immediate-opc i #b00)
       (not (bits x 47 32)))))

(define-relocation R_AARCH64_LD_PREL_LO19 273 4 4 (- (+ s a) p)
  (check-int x 20)
  (encode-load-register-literal-imm19 i (bits x 20 2)))
(define-relocation R_AARCH64_ADR_PREL_LO21 274 4 4 (- (+ s a) p)
  (check-int x 20)
  (encode-pc-rel-addressing-imm i (bits x 20 0)))
(define-relocation R_AARCH64_ADR_PREL_PG_HI21 275 4 4 (- (page (+ s a)) (page p))
  (check-int x 32)
  (encode-pc-rel-addressing-imm i (bits x 32 12)))
(define-relocation R_AARCH64_ADR_PREL_PG_HI21_NC 276 4 4 (- (page (+ s a)) (page p))
  (encode-pc-rel-addressing-imm i (bits x 32 12)))
(define-relocation R_AARCH64_ADD_ABS_LO12_NC 277 4 4 (+ s a)
  (encode-add/subtract-immediate-imm12 i (bits x 11 0)))
(define-relocation R_AARCH64_LDST8_ABS_LO12_NC 278 4 4 (+ s a)
  (encode-load/store-register-unsigned-immediate-imm12 i (bits x 11 0)))
(define-relocation R_AARCH64_LDST16_ABS_LO12_NC 284 4 4 (+ s a)
  (encode-load/store-register-unsigned-immediate-imm12 i (bits x 11 1)))
(define-relocation R_AARCH64_LDST32_ABS_LO12_NC 285 4 4 (+ s a)
  (encode-load/store-register-unsigned-immediate-imm12 i (bits x 11 2)))
(define-relocation R_AARCH64_LDST64_ABS_LO12_NC 286 4 4 (+ s a)
  (encode-load/store-register-unsigned-immediate-imm12 i (bits x 11 3)))
(define-relocation R_AARCH64_LDST128_ABS_LO12_NC 299 4 4 (+ s a)
  (encode-load/store-register-unsigned-immediate-imm12 i (bits x 11 4)))

(define-relocation R_AARCH64_TSTBR14 279 4 4 (- (+ s a) p)
  (check-int x 15)
  (encode-test-and-branch-immediate-imm14 i (bits x 15 2)))
(define-relocation R_AARCH64_CONDBR19 280 4 4 (- (+ s a) p)
  (check-int x 20)
  (encode-conditional-branch-immediate-imm19 i (bits x 20 2)))
(define-relocation R_AARCH64_JUMP26 282 4 4 (- (+ s a) p)
  ;; Used for ~b~ instructions
  (check-int x 27)
  (encode-unconditional-branch-immediate-imm26 i (bits x 27 2)))
(define-relocation R_AARCH64_CALL26 283 4 4 (- (+ s a) p)
  ;; Used for ~bl~ instructions
  (check-int x 27)
  (encode-unconditional-branch-immediate-imm26 i (bits x 27 2)))

(with-inst-type pc-rel-addressing
  (define-inst adr 0 ((op 0))
    (reg Rd xzr) (link R_AARCH64_ADR_PREL_LO21))
  (define-inst adrp 0 ((op 1))
    (reg Rd xzr) (link R_AARCH64_ADR_PREL_PG_HI21)))

(with-inst-type add/subtract-immediate
  (define-inst add 0 ((sf 0) (op 0) (S 0))
    (reg Rd wsp) (reg Rn wsp) (const u 12 imm12) (shift1 sh))
  (define-inst add 1 ((sf 1) (op 0) (S 0))
    (reg Rd xsp) (reg Rn xsp) (const u 12 imm12) (shift1 sh))
  (define-inst adds 0 ((sf 0) (op 0) (S 1))
    (reg Rd wzr) (reg Rn wsp) (const u 12 imm12) (shift1 sh))
  (define-inst adds 1 ((sf 1) (op 0) (S 1))
    (reg Rd xzr) (reg Rn xsp) (const u 12 imm12) (shift1 sh))
  (define-inst sub 0 ((sf 0) (op 1) (S 0))
    (reg Rd wsp) (reg Rn wsp) (const u 12 imm12) (shift1 sh))
  (define-inst sub 1 ((sf 1) (op 1) (S 0))
    (reg Rd xsp) (reg Rn xsp) (const u 12 imm12) (shift1 sh))
  (define-inst subs 0 ((sf 0) (op 1) (S 1))
    (reg Rd wzr) (reg Rn wsp) (const u 12 imm12) (shift1 sh))
  (define-inst subs 1 ((sf 1) (op 1) (S 1))
    (reg Rd xzr) (reg Rn xsp) (const u 12 imm12) (shift1 sh))
  (define-inst cmp 0 ((sf 0) (op 1) (S 1) (Rd #b11111))
    (reg Rn wsp) (const u 12 imm12) (shift1 sh))
  (define-inst cmp 1 ((sf 1) (op 1) (S 1) (Rd #b11111))
    (reg Rn xsp) (const u 12 imm12) (shift1 sh))
  (define-inst cmn 0 ((sf 0) (op 0) (S 1) (Rd #b11111))
    (reg Rn wsp) (const u 12 imm12) (shift1 sh))
  (define-inst cmp 1 ((sf 1) (op 0) (S 1) (Rd #b11111))
    (reg Rn xsp) (const u 12 imm12) (shift1 sh)))

(with-inst-type logical-immediate
  (define-inst andr 0 ((sf 0) (opc #b00) (N 0))
    (reg Rd wsp) (reg Rn wzr) (bitmask 32))
  (define-inst andr 1 ((sf 1) (opc #b00))
    (reg Rd xsp) (reg Rn xzr) (bitmask 64))
  (define-inst ands 0 ((sf 0) (opc #b11) (N 0))
    (reg Rd wzr) (reg Rn wzr) (bitmask 32))
  (define-inst ands 1 ((sf 1) (opc #b11))
    (reg Rd xzr) (reg Rn xzr) (bitmask 64))
  (define-inst eor 0 ((sf 0) (opc #b10) (N 0))
    (reg Rd wsp) (reg Rn wzr) (bitmask 32))
  (define-inst eor 1 ((sf 1) (opc #b10))
    (reg Rd xsp) (reg Rn xzr) (bitmask 64))
  (define-inst orr 0 ((sf 0) (opc #b01) (N 0))
    (reg Rd wsp) (reg Rn wzr) (bitmask 32))
  (define-inst orr 1 ((sf 1) (opc #b01))
    (reg Rd xsp) (reg Rn xzr) (bitmask 64))
  (define-inst tst 0 ((sf 0) (opc #b11) (N 0) (Rd #b11111))
    (reg Rn wzr) (bitmask 32))
  (define-inst tst 1 ((sf 1) (opc #b11) (Rd #b11111))
    (reg Rn xzr) (bitmask 64)))

(with-inst-type move-wide-immediate
  (define-inst movz 0 ((sf 0) (opc #b10))
    (reg Rd wzr) (const u 16 imm16) (shift2 hw 32))
  (define-inst movz 1 ((sf 1) (opc #b10))
    (reg Rd xzr) (const u 16 imm16) (shift2 hw 64))
  (define-inst movn 0 ((sf 0) (opc #b00))
    (reg Rd wzr) (const u 16 imm16) (shift2 hw 32))
  (define-inst movn 1 ((sf 1) (opc #b00))
    (reg Rd xzr) (const u 16 imm16) (shift2 hw 64))
  (define-inst movk 0 ((sf 0) (opc #b11))
    (reg Rd wzr) (const u 16 imm16) (shift2 hw 32))
  (define-inst movk 1 ((sf 1) (opc #b11))
    (reg Rd xzr) (const u 16 imm16) (shift2 hw 64))
  (define-inst mov 0 ((sf 0) (opc #b00))
    (reg Rd wzr) (mov-const imm16 hw 32 :inverted t))
  (define-inst mov 1 ((sf 1) (opc #b00))
    (reg Rd xzr) (mov-const imm16 hw 64 :inverted t))
  (define-inst mov 2 ((sf 0) (opc #b10))
    (reg Rd wzr) (mov-const imm16 hw 32))
  (define-inst mov 3 ((sf 1) (opc #b10))
    (reg Rd xzr) (mov-const imm16 hw 64)))

(define-alias mov 4 (rd imm)
  (list 'orr rd (if (register-wide rd) xzr wzr) imm))
(define-alias mov 5 (rd rn)
  (list 'add rd rn 0))

(with-inst-type bitfield
  (define-inst bfm 0 ((sf 0) (opc #b01) (N 0))
    (reg Rd wzr) (reg Rn wzr) (const u 6 immr) (const u 6 imms))
  (define-inst bfm 1 ((sf 1) (opc #b01) (N 1))
    (reg Rd xzr) (reg Rn xzr) (const u 6 immr) (const u 6 imms))
  (define-inst sbfm 0 ((sf 0) (opc #b00) (N 0))
    (reg Rd wzr) (reg Rn wzr) (const u 6 immr) (const u 6 imms))
  (define-inst sbfm 1 ((sf 1) (opc #b00) (N 1))
    (reg Rd xzr) (reg Rn xzr) (const u 6 immr) (const u 6 imms))
  (define-inst ubfm 0 ((sf 0) (opc #b10) (N 0))
    (reg Rd wzr) (reg Rn wzr) (const u 6 immr) (const u 6 imms))
  (define-inst ubfm 1 ((sf 1) (opc #b10) (N 1))
    (reg Rd xzr) (reg Rn xzr) (const u 6 immr) (const u 6 imms))
  (define-alias bfc 0 ((wd wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'bfm wd wzr (mod (- lsb) 32) (1- width)))
  (define-alias bfc 1 ((xd xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'bfm xd xzr (mod (- lsb) 64) (1- width)))
  (define-alias bfi 0 ((wd wzr-register) (wn wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'bfm wd wn (mod (- lsb) 32) (1- width)))
  (define-alias bfi 1 ((xd xzr-register) (xn xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'bfm xd xn (mod (- lsb) 64) (1- width)))
  (define-alias bfxil 0 ((wd wzr-register) (wn wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'bfm wd wn lsb (1- (+ lsb width))))
  (define-alias bfxil 1 ((xd xzr-register) (xn xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'bfm xd xn lsb (1- (+ lsb width))))
  (define-alias sbfiz 0 ((wd wzr-register) (wn wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'sbfm wd wn (mod (- lsb) 32) (1- width)))
  (define-alias sbfiz 1 ((xd xzr-register) (xn xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'sbfm xd xn (mod (- lsb) 64) (1- width)))
  (define-alias sbfx 0 ((wd wzr-register) (wn wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'sbfm wd wn lsb (1- (+ lsb width))))
  (define-alias sbfx 1 ((xd xzr-register) (xn xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'sbfm xd xn lsb (1- (+ lsb width))))
  (define-alias ubfiz 0 ((wd wzr-register) (wn wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'ubfm wd wn (mod (- lsb) 32) (1- width)))
  (define-alias ubfiz 1 ((xd xzr-register) (xn xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'ubfm xd xn (mod (- lsb) 64) (1- width)))
  (define-alias ubfx 0 ((wd wzr-register) (wn wzr-register) (lsb (integer 0 31)) (width `(integer 1 ,(- 32 lsb))))
    (list 'ubfm wd wn lsb (1- (+ lsb width))))
  (define-alias ubfx 1 ((xd xzr-register) (xn xzr-register) (lsb (integer 0 63)) (width `(integer 1 ,(- 64 lsb))))
    (list 'ubfm xd xn lsb (1- (+ lsb width))))
  (define-inst sxtb 0 ((sf 0) (opc #b00) (N 0) (immr 0) (imms 7))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst sxtb 1 ((sf 1) (opc #b00) (N 1) (immr 0) (imms 7))
    (reg Rd xzr) (reg Rn wzr))
  (define-inst sxth 0 ((sf 0) (opc #b00) (N 0) (immr 0) (imms 15))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst sxth 1 ((sf 1) (opc #b00) (N 1) (immr 0) (imms 15))
    (reg Rd xzr) (reg Rn wzr))
  (define-inst sxtw 0 ((sf 1) (opc #b00) (N 1) (immr 0) (imms 31))
    (reg Rd xzr) (reg Rn wzr))
  (define-inst uxtb 0 ((sf 0) (opc #b10) (N 0) (immr 0) (imms 7))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst uxth 0 ((sf 0) (opc #b10) (N 0) (immr 0) (imms 15))
    (reg Rd wzr) (reg Rn wzr)))

(with-inst-type conditional-branch-immediate
  (define-inst bc 0 ((o1 0) (o0 0))
    (condition cond) (link R_AARCH64_CONDBR19))
  (define-inst bcc 0 ((o1 0) (o0 1))
    (condition cond) (link R_AARCH64_CONDBR19)))

(with-inst-type exception-generation
  (define-inst svc 0 ((opc #b000) (op2 #b000) (LL #b01))
    (const u 16 imm16))
  (define-inst hvc 0 ((opc #b000) (op2 #b000) (LL #b10))
    (const u 16 imm16))
  (define-inst smc 0 ((opc #b000) (op2 #b000) (LL #b11))
    (const u 16 imm16))
  (define-inst brk 0 ((opc #b001) (op2 #b000) (LL #b00))
    (const u 16 imm16))
  (define-inst hlt 0 ((opc #b010) (op2 #b000) (LL #b00))
    (const u 16 imm16)))

(with-inst-type unconditional-branch-register
  (define-inst br 0 ((opc #b0000) (op2 #b11111))
    (reg Rn xzr))
  (define-inst blr 0 ((opc #b0001) (op2 #b11111))
    (reg Rn xzr))
  (define-inst ret 0 ((opc #b0010) (op2 #b11111) (Rn 30)))
  (define-inst ret 1 ((opc #b0010) (op2 #b11111))
    (reg Rn xzr))
  (define-inst eret 0 ((opc #b0100) (op2 #b11111) (Rn 31)))
  (define-inst drps 0 ((opc #b0101) (op2 #b11111) (Rn 31))))

(with-inst-type unconditional-branch-immediate
  (define-inst b 0 ((op 0))
    (link R_AARCH64_JUMP26))
  (define-inst bl 0 ((op 1))
    (link R_AARCH64_CALL26)))

(with-inst-type compare-and-branch-immediate
  (define-inst cbz 0 ((sf 0) (op 0))
    (reg Rt wzr) (link R_AARCH64_CONDBR19))
  (define-inst cbz 1 ((sf 1) (op 0))
    (reg Rt xzr) (link R_AARCH64_CONDBR19))
  (define-inst cbnz 0 ((sf 0) (op 1))
    (reg Rt wzr) (link R_AARCH64_CONDBR19))
  (define-inst cbnz 1 ((sf 1) (op 1))
    (reg Rt xzr) (link R_AARCH64_CONDBR19)))

(with-inst-type test-and-branch-immediate
  (define-inst tbz 0 ((op 0))
    (test-reg-bit Rt b5 b40) (link R_AARCH64_TSTBR14))
  (define-inst tbnz 0 ((op 1))
    (test-reg-bit Rt b5 b40) (link R_AARCH64_TSTBR14)))

(with-inst-type load-register-literal
  (define-inst ldr 0 ((opc 0) (V 0))
    (reg Rt wzr) (link R_AARCH64_LD_PREL_LO19))
  (define-inst ldr 1 ((opc 1) (V 0))
    (reg Rt xzr) (link R_AARCH64_LD_PREL_LO19))
  (define-inst ldrsw 0 ((opc 2) (V 0))
    (reg Rt xzr) (link R_AARCH64_LD_PREL_LO19)))

(with-inst-type load/store-register-pair
  (define-inst stp 0 ((opc #b00) (V 0) (L 0))
    (reg Rt wzr) (reg Rt2 wzr) (reg Rn xsp) (const i 9 imm7 4) (pre/post ind #b010))
  (define-inst stp 1 ((opc #b10) (V 0) (L 0))
    (reg Rt xzr) (reg Rt2 xzr) (reg Rn xsp) (const i 10 imm7 8) (pre/post ind #b010))
  (define-inst ldp 0 ((opc #b00) (V 0) (L 1))
    (reg Rt wzr) (reg Rt2 wzr) (reg Rn xsp) (const i 9 imm7 4) (pre/post ind #b010))
  (define-inst ldp 1 ((opc #b10) (V 0) (L 1))
    (reg Rt xzr) (reg Rt2 xzr) (reg Rn xsp) (const i 10 imm7 8) (pre/post ind #b010))
  (define-inst ldpsw 0 ((opc #b01) (V 0) (L 1))
    (reg Rt xzr) (reg Rt2 xzr) (reg Rn xsp) (const i 10 imm7 8) (pre/post ind #b010)))

(with-inst-type load/store-register-unscaled-immediate
  (define-inst sturb 0 ((size #b00) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldurb 0 ((size #b00) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldursb 0 ((size #b00) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldursb 1 ((size #b00) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst sturh 0 ((size #b01) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldurh 0 ((size #b01) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldursh 0 ((size #b01) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldursh 1 ((size #b01) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst stur 0 ((size #b10) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldur 0 ((size #b10) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldursw 0 ((size #b10) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst stur 1 ((size #b11) (V 0) (opc #b00))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst ldur 1 ((size #b11) (V 0) (opc #b01))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9))
  (define-inst strb 0 ((size #b00) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrb 0 ((size #b00) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrsb 0 ((size #b00) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrsb 1 ((size #b00) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst strh 0 ((size #b01) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrh 0 ((size #b01) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrsh 0 ((size #b01) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrsh 1 ((size #b01) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst str 0 ((size #b10) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldr 2 ((size #b10) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldrsw 1 ((size #b10) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst str 1 ((size #b11) (V 0) (opc #b00))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind))
  (define-inst ldr 3 ((size #b11) (V 0) (opc #b01))
    (reg Rt xzr) (reg Rn xsp) (const i 9 imm9) (pre/post ind)))

(with-inst-type load/store-register-register
  (define-inst strb 1 ((size #b00) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 0))
  (define-inst ldrb 1 ((size #b00) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 0))
  (define-inst ldrsb 2 ((size #b00) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (reg-extend1 Rm 0))
  (define-inst ldrsb 3 ((size #b00) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 0))
  (define-inst strh 1 ((size #b01) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 1))
  (define-inst ldrh 1 ((size #b01) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 1))
  (define-inst ldrsh 2 ((size #b01) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (reg-extend1 Rm 1))
  (define-inst ldrsh 3 ((size #b01) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 1))
  (define-inst str 2 ((size #b10) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 2))
  (define-inst ldr 4 ((size #b10) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (reg-extend1 Rm 2))
  (define-inst ldrsw 2 ((size #b10) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (reg-extend1 Rm 2))
  (define-inst str 3 ((size #b11) (V 0) (opc #b00))
    (reg Rt xzr) (reg Rn xsp) (reg-extend1 Rm 3))
  (define-inst ldr 5 ((size #b11) (V 0) (opc #b01))
    (reg Rt xzr) (reg Rn xsp) (reg-extend1 Rm 3)))

(with-inst-type load/store-register-unsigned-immediate
  (define-inst strb 2 ((size #b00) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const u 12 imm12 1))
  (define-inst ldrb 2 ((size #b00) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const u 12 imm12 1))
  (define-inst ldrsb 4 ((size #b00) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const u 12 imm12 1))
  (define-inst ldrsb 5 ((size #b00) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (const u 12 imm12 1))
  (define-inst strh 2 ((size #b01) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const u 13 imm12 2))
  (define-inst ldrh 2 ((size #b01) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const u 13 imm12 2))
  (define-inst ldrsh 4 ((size #b01) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const u 13 imm12 2))
  (define-inst ldrsh 5 ((size #b01) (V 0) (opc #b11))
    (reg Rt wzr) (reg Rn xsp) (const u 13 imm12 2))
  (define-inst str 4 ((size #b10) (V 0) (opc #b00))
    (reg Rt wzr) (reg Rn xsp) (const u 14 imm12 4))
  (define-inst ldr 6 ((size #b10) (V 0) (opc #b01))
    (reg Rt wzr) (reg Rn xsp) (const u 14 imm12 4))
  (define-inst ldrsw 3 ((size #b10) (V 0) (opc #b10))
    (reg Rt xzr) (reg Rn xsp) (const u 14 imm12 4))
  (define-inst str 5 ((size #b11) (V 0) (opc #b00))
    (reg Rt xzr) (reg Rn xsp) (const u 15 imm12 8))
  (define-inst ldr 7 ((size #b11) (V 0) (opc #b01))
    (reg Rt xzr) (reg Rn xsp) (const u 15 imm12 8)))

(with-inst-type data-processing-2-source
  (define-inst udiv 0 ((sf 0) (opcode #b0010))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst udiv 1 ((sf 1) (opcode #b0010))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst sdiv 0 ((sf 0) (opcode #b0011))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst sdiv 1 ((sf 1) (opcode #b0011))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst lslv 0 ((sf 0) (opcode #b1000))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst lslv 1 ((sf 1) (opcode #b1000))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst lsrv 0 ((sf 0) (opcode #b1001))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst lsrv 1 ((sf 1) (opcode #b1001))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst asrv 0 ((sf 0) (opcode #b1010))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst asrv 1 ((sf 1) (opcode #b1010))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst rorv 0 ((sf 0) (opcode #b1011))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst rorv 1 ((sf 1) (opcode #b1011))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr)))

(with-inst-type data-processing-1-source
  (define-inst rbit 0 ((sf 0) (opcode #b000))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst rbit 1 ((sf 1) (opcode #b000))
    (reg Rd xzr) (reg Rn xzr))
  (define-inst rev16 0 ((sf 0) (opcode #b001))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst rev16 1 ((sf 1) (opcode #b001))
    (reg Rd xzr) (reg Rn xzr))
  (define-inst rev 0 ((sf 0) (opcode #b010))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst rev32 0 ((sf 1) (opcode #b010))
    (reg Rd xzr) (reg Rn xzr))
  (define-inst rev 1 ((sf 1) (opcode #b011))
    (reg Rd xzr) (reg Rn xzr))
  (define-inst clz 0 ((sf 0) (opcode #b100))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst clz 1 ((sf 1) (opcode #b100))
    (reg Rd xzr) (reg Rn xzr))
  (define-inst cls 0 ((sf 0) (opcode #b101))
    (reg Rd wzr) (reg Rn wzr))
  (define-inst cls 1 ((sf 1) (opcode #b101))
    (reg Rd xzr) (reg Rn xzr)))

(with-inst-type logical-shifted-register
  (define-inst andr 2 ((sf 0) (opc #b00) (N 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst andr 3 ((sf 1) (opc #b00) (N 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst bic 0 ((sf 0) (opc #b00) (N 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst bic 1 ((sf 1) (opc #b00) (N 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst orr 2 ((sf 0) (opc #b01) (N 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst orr 3 ((sf 1) (opc #b01) (N 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst orn 0 ((sf 0) (opc #b01) (N 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst orn 1 ((sf 1) (opc #b01) (N 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst eor 2 ((sf 0) (opc #b10) (N 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst eor 3 ((sf 1) (opc #b10) (N 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst eon 0 ((sf 0) (opc #b10) (N 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst eon 1 ((sf 1) (opc #b10) (N 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst ands 2 ((sf 0) (opc #b11) (N 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst ands 3 ((sf 1) (opc #b11) (N 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64))
  (define-inst bics 0 ((sf 0) (opc #b11) (N 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift3 shift imm6 32))
  (define-inst bics 1 ((sf 1) (opc #b11) (N 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift3 shift imm6 64)))

(with-inst-type add/subtract-shifted-register
  (define-inst add 2 ((sf 0) (op 0) (S 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst add 3 ((sf 1) (op 0) (S 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst adds 2 ((sf 0) (op 0) (S 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst adds 3 ((sf 1) (op 0) (S 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst sub 2 ((sf 0) (op 1) (S 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst sub 3 ((sf 1) (op 1) (S 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst subs 2 ((sf 0) (op 1) (S 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst subs 3 ((sf 1) (op 1) (S 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst cmn 2 ((sf 0) (op 0) (S 1) (Rd 31))
    (reg Rn wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst cmn 3 ((sf 1) (op 0) (S 1) (Rd 31))
    (reg Rn xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst cmp 2 ((sf 0) (op 1) (S 1) (Rd 31))
    (reg Rn wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst cmp 3 ((sf 1) (op 1) (S 1) (Rd 31))
    (reg Rn xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst neg 0 ((sf 0) (op 1) (S 0) (Rn 31))
    (reg Rd wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst neg 1 ((sf 1) (op 1) (S 0) (Rn 31))
    (reg Rd xzr) (reg Rm xzr) (shift4 shift imm6 64))
  (define-inst negs 0 ((sf 0) (op 1) (S 1) (Rn 31))
    (reg Rd wzr) (reg Rm wzr) (shift4 shift imm6 32))
  (define-inst negs 1 ((sf 1) (op 1) (S 1) (Rn 31))
    (reg Rd xzr) (reg Rm xzr) (shift4 shift imm6 64)))

(with-inst-type add/subtract-extended-register
  (define-inst add 4 ((sf 0) (op 0) (S 0))
    (reg Rd wsp) (reg Rn wsp) (reg-extend2 Rm 32))
  (define-inst add 5 ((sf 1) (op 0) (S 0))
    (reg Rd xsp) (reg Rn xsp) (reg-extend2 Rm 64))
  (define-inst adds 4 ((sf 0) (op 0) (S 1))
    (reg Rd wzr) (reg Rn wsp) (reg-extend2 Rm 32))
  (define-inst adds 5 ((sf 1) (op 0) (S 1))
    (reg Rd xzr) (reg Rn xsp) (reg-extend2 Rm 64))
  (define-inst sub 4 ((sf 0) (op 1) (S 0))
    (reg Rd wsp) (reg Rn wsp) (reg-extend2 Rm 32))
  (define-inst sub 5 ((sf 1) (op 1) (S 0))
    (reg Rd xsp) (reg Rn xsp) (reg-extend2 Rm 64))
  (define-inst subs 4 ((sf 0) (op 1) (S 1))
    (reg Rd wzr) (reg Rn wsp) (reg-extend2 Rm 32))
  (define-inst subs 5 ((sf 1) (op 1) (S 1))
    (reg Rd xzr) (reg Rn xsp) (reg-extend2 Rm 64))
  (define-inst cmn 4 ((sf 0) (op 0) (S 1) (Rd 31))
    (reg Rn wsp) (reg-extend2 Rm 32))
  (define-inst cmn 5 ((sf 1) (op 0) (S 1) (Rd 31))
    (reg Rn xsp) (reg-extend2 Rm 64))
  (define-inst cmp 4 ((sf 0) (op 1) (S 1) (Rd 31))
    (reg Rn wsp) (reg-extend2 Rm 32))
  (define-inst cmp 5 ((sf 1) (op 1) (S 1) (Rd 31))
    (reg Rn xsp) (reg-extend2 Rm 64)))

(with-inst-type add/subtract-with-carry
  (define-inst adc 0 ((sf 0) (op 0) (S 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst adc 1 ((sf 1) (op 0) (S 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst adcs 0 ((sf 0) (op 0) (S 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst adcs 1 ((sf 1) (op 0) (S 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst sbc 0 ((sf 0) (op 1) (S 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst sbc 1 ((sf 1) (op 1) (S 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst sbcs 0 ((sf 0) (op 1) (S 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr))
  (define-inst sbcs 1 ((sf 1) (op 1) (S 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr)))

(with-inst-type conditional-compare-register
  (define-inst ccmn 0 ((sf 0) (op 0) (S 1))
    (reg Rn wzr) (reg Rm wzr) (const u 4 nzcv) (condition cond))
  (define-inst ccmn 1 ((sf 1) (op 0) (S 1))
    (reg Rn xzr) (reg Rm xzr) (const u 4 nzcv) (condition cond))
  (define-inst ccmp 0 ((sf 0) (op 1) (S 1))
    (reg Rn wzr) (reg Rm wzr) (const u 4 nzcv) (condition cond))
  (define-inst ccmp 1 ((sf 1) (op 1) (S 1))
    (reg Rn xzr) (reg Rm xzr) (const u 4 nzcv) (condition cond)))

(with-inst-type conditional-compare-immediate
  (define-inst ccmn 0 ((sf 0) (op 0) (S 1))
    (reg Rn wzr) (const u 5 imm5) (const u 4 nzcv) (condition cond))
  (define-inst ccmn 1 ((sf 1) (op 0) (S 1))
    (reg Rn xzr) (const u 5 imm5) (const u 4 nzcv) (condition cond))
  (define-inst ccmp 0 ((sf 0) (op 1) (S 1))
    (reg Rn wzr) (const u 5 imm5) (const u 4 nzcv) (condition cond))
  (define-inst ccmp 1 ((sf 1) (op 1) (S 1))
    (reg Rn xzr) (const u 5 imm5) (const u 4 nzcv) (condition cond)))

(with-inst-type conditional-select
  (define-inst csel 0 ((sf 0) (op 0) (op2 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (condition cond))
  (define-inst csel 1 ((sf 1) (op 0) (op2 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (condition cond))
  (define-inst csinc 0 ((sf 0) (op 0) (op2 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (condition cond))
  (define-inst csinc 1 ((sf 1) (op 0) (op2 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (condition cond))
  (define-inst csinv 0 ((sf 0) (op 1) (op2 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (condition cond))
  (define-inst csinv 1 ((sf 1) (op 1) (op2 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (condition cond))
  (define-inst csneg 0 ((sf 0) (op 1) (op2 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (condition cond))
  (define-inst csneg 1 ((sf 1) (op 1) (op2 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (condition cond))
  (define-alias cinc 0 (rd rn (cond symbol))
    (list 'csinc rd rn rn (invert-condition cond)))
  (define-alias cset 0 ((rd register) (cond symbol))
    (let ((zr (if (register-wide rd) xzr wzr)))
      (list 'csinc rd zr zr (invert-condition cond))))
  (define-alias cinv 0 (rd rn (cond symbol))
    (list 'csinv rd rn rn (invert-condition cond)))
  (define-alias csetm 0 ((rd register) (cond symbol))
    (let ((zr (if (register-wide rd) xzr wzr)))
      (list 'csinv rd zr zr (invert-condition cond))))
  (define-alias cneg 0 (rd rn (cond symbol))
    (list 'csneg rd rn rn (invert-condition cond))))

(with-inst-type data-processing-3-source
  (define-inst madd 0 ((sf 0) (op31 #b000) (o0 0))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (reg Ra wzr))
  (define-inst madd 1 ((sf 1) (op31 #b000) (o0 0))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (reg Ra xzr))
  (define-inst msub 0 ((sf 0) (op31 #b000) (o0 1))
    (reg Rd wzr) (reg Rn wzr) (reg Rm wzr) (reg Ra wzr))
  (define-inst msub 1 ((sf 1) (op31 #b000) (o0 1))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr) (reg Ra xzr))
  (define-inst smaddl 0 ((sf 1) (op31 #b001) (o0 0))
    (reg Rd xzr) (reg Rn wzr) (reg Rm wzr) (reg Ra xzr))
  (define-inst smsubl 0 ((sf 1) (op31 #b001) (o0 1))
    (reg Rd xzr) (reg Rn wzr) (reg Rm wzr) (reg Ra xzr))
  (define-inst smulh 0 ((sf 1) (op31 #b010) (o0 0) (Ra #b11111))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-inst umaddl 0 ((sf 1) (op31 #b101) (o0 0))
    (reg Rd xzr) (reg Rn wzr) (reg Rm wzr) (reg Ra xzr))
  (define-inst umsubl 0 ((sf 1) (op31 #b101) (o0 1))
    (reg Rd xzr) (reg Rn wzr) (reg Rm wzr) (reg Ra xzr))
  (define-inst umulh 0 ((sf 1) (op31 #b110) (o0 0) (Ra #b11111))
    (reg Rd xzr) (reg Rn xzr) (reg Rm xzr))
  (define-alias mul 0 (rd rn rm)
    (list 'madd rd rn rm (if (register-wide rd) xzr wzr)))
  (define-alias mneg 0 (rd rn rm)
    (list 'msub rd rn rm (if (register-wide rd) xzr wzr)))
  (define-alias smull 0 (rd rn rm)
    (list 'smaddl rd rn rm xzr))
  (define-alias smnegl 0 (rd rn rm)
    (list 'smsubl rd rn rm xzr))
  (define-alias umull 0 (rd rn rm)
    (list 'umaddl rd rn rm xzr))
  (define-alias umnegl 0 (rd rn rm)
    (list 'umsubl rd rn rm xzr)))

;; ==== ;;
;; Test ;;
;; ==== ;;

;; (defparameter sys-exit #x5d)
;; (defparameter sys-read #x3f)
;; (defparameter sys-write #x40)
;; (defparameter sys-open #x40)
;; (defparameter sys-close #x39)


;; (defun test ()
;;   (let ((src '((defun main () void
;;                  (set (@ (as (* u8) #x9000000))
;;                       48)))))
;;     (declare (ignore src))
;;     (link (asm
;;             (mov x0 #x40200000)
;;             (mov sp x0)
;;             (bl 'test)
;;             (movz w0 #x0008)
;;             (movk w0 #x84000000)
;;             (hvc 0)
;;             test))))

;; (defun test-file ()
;;   (with-open-file (out "./test" :direction :output
;;                                 :if-exists :supersede
;;                                 :element-type '(unsigned-byte 8))
;;     (write-sequence (test) out)))
