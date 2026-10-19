(defpackage #:assemble
  (:use #:cl #:utils #:elf #:asm)
  (:export #:asm #:loaded-object #:make-loaded-object
           #:symbol-sap

           #:.align #:.ascii #:.asciz #:.bss #:.byte
           #:.data #:.global #:.hword #:.local #:.popsection
           #:.previous #:.pushsection #:.rodata #:.section
           #:.size #:.subsection #:.text #:.type #:.word
           #:.xword #:.zero))


(in-package #:assemble)


(defparameter *assembler-directives*
  (make-hash-table))
(defparameter *assembler* nil)
(defparameter *array-increment* 16)

(defstruct (assembler (:constructor %make-assembler))
  (sections (make-hash-table) :type hash-table)
  (section-headers (make-hash-table) :type hash-table)
  (section-stack nil :type cons)
  (symbols (make-hash-table :test 'equal) :type hash-table)
  (relocations (make-hash-table) :type hash-table)
  ;; TODO: trim this in assembler state
  (strtab (make-large-byte-array) :type array)
  (strtab-map (make-hash-table :test 'equal) :type hash-table)
  (shstrtab (make-large-byte-array) :type array)
  (shstrtab-map (make-hash-table :test 'equal) :type hash-table))

(defstruct assembler-relocation
  (type nil :type (and symbol (not null)))
  (offset 0 :type (integer 0))
  (symbol nil :type (and symbol (not null)))
  (addend 0 :type integer)
  (subsection 0 :type (integer 0)))

(defstruct assembler-symbol
  (name nil :type (and symbol (not null)))
  (value 0 :type (integer 0))
  (size 0 :type (integer 0))
  (type :notype :type (member :notype :object :function :section
                              :file :common :tls))
  (bind :local :type (member :local :global :weak))
  (visibility :default :type (member :default))
  (section nil :type symbol)
  (subsection 0 :type (integer 0)))

(defun assembler-symbol-to-string (symbol)
  (etypecase symbol
    (symbol (string-downcase (symbol-name symbol)))
    (string symbol)))

(defun assembler-strtab-add (symbol)
  (or (gethash symbol (assembler-strtab-map *assembler*))
      (let* ((strtab (assembler-strtab *assembler*))
             (offset (length strtab)))
        (loop for i from offset
              for ch across (assembler-symbol-to-string symbol)
              do (large-byte-array-set-byte strtab i (char-int ch))
              finally (large-byte-array-set-byte strtab (length strtab) 0))
        (setf (gethash symbol (assembler-strtab-map *assembler*))
              offset))))

(defun assembler-shstrtab-add (symbol)
  (or (gethash symbol (assembler-shstrtab-map *assembler*))
      (let* ((shstrtab (assembler-shstrtab *assembler*))
             (offset (length shstrtab)))
        (loop for i from offset
              for ch across (assembler-symbol-to-string symbol)
              do (large-byte-array-set-byte shstrtab i (char-int ch))
              finally (large-byte-array-set-byte shstrtab (length shstrtab) 0))
        (setf (gethash symbol (assembler-shstrtab-map *assembler*))
              offset))))

(defun assembler-make-symbol (name &rest keys)
  (check-type name symbol)
  (assembler-strtab-add name)
  (setf (gethash name (assembler-symbols *assembler*))
        (apply #'make-assembler-symbol :name name keys)))

(defun assembler-make-section (name &rest keys)
  (check-type name symbol)
  (let ((sh-name (assembler-shstrtab-add name)))
    (setf (gethash name (assembler-section-headers *assembler*))
          (apply #'make-elf64-shdr :sh-name sh-name :sh-addralign 1 keys)))
  (setf (gethash name (assembler-sections *assembler*))
        (let ((array (make-dynamic-array)))
          (vector-push-extend (make-large-byte-array) array)
          array)))

(defun assembler-ensure-section (name &rest keys)
  (or (gethash name (assembler-sections *assembler*))
      (apply #'assembler-make-section name keys)))

(defun assembler-ensure-symbol (name &rest keys)
  (or (gethash name (assembler-symbols *assembler*))
      (apply #'assembler-make-symbol name keys)))

(defun make-assembler ()
  (let ((*assembler* (%make-assembler :section-stack (list (cons '.text 0)))))
    (large-byte-array-set-byte (assembler-shstrtab *assembler*) 0 0)
    (large-byte-array-set-byte (assembler-strtab *assembler*) 0 0)
    (assembler-make-section '.text :sh-type sht-progbits :sh-flags (logior shf-execinstr shf-alloc))
    (assembler-make-section '.data :sh-type sht-progbits :sh-flags (logior shf-write shf-alloc))
    (assembler-make-section '.bss :sh-type sht-nobits :sh-flags (logior shf-write shf-alloc))
    (assembler-make-section '.rodata :sh-type sht-progbits :sh-flags shf-alloc)
    *assembler*))

(defmacro define-assembler-directive (name args &body body)
  `(setf (gethash ',name *assembler-directives*)
         (lambda ,args ,@body)))

(defmacro with-current-section ((name &optional header) &body body)
  (if header
      `(multiple-value-bind (,name ,header) (assembler-current-section)
         ,@body)
      `(let ((,name (assembler-current-section)))
         ,@body)))

(defun assembler-current-section ()
  (destructuring-bind (section . subsection)
      (first (assembler-section-stack *assembler*))
    (values (aref (gethash section (assembler-sections *assembler*))
                  subsection)
            (gethash section (assembler-section-headers *assembler*)))))

(defun assembler-expand-subsections (name subsection)
  (let ((section (gethash name (assembler-sections *assembler*))))
    (loop while (<= (length section) subsection)
          do (vector-push-extend (make-large-byte-array) section))))

(defun assembler-push-section (name &optional (subsection 0))
  (check-type name (and symbol (not null)))
  (check-type subsection (integer 0))
  (assembler-ensure-section name)
  (when subsection
    (assembler-expand-subsections name subsection))
  (push (cons name (or subsection 0))
        (assembler-section-stack *assembler*)))

(defun assembler-align (alignment &optional (value 0))
  (check-type alignment (integer 0))
  (with-current-section (section header)
    (setf (elf64-shdr-sh-addralign header)
          (max (elf64-shdr-sh-addralign header)
               alignment))
    (let* ((length (length section))
           (new-length (align-extend length alignment)))
      (loop for i from length below new-length
            do (large-byte-array-set-byte section i value)))))

(defun assembler-write-string (string &key terminate)
  (check-type string string)
  (with-current-section (section)
    (assembler-align 8)
    (loop for i from (length section)
          for ch across string
          do (large-byte-array-set-byte section i (char-int ch))
          finally (when terminate
                    (large-byte-array-set-byte section (length section) 0)))))

(defun assembler-section-align (assembler section-name)
  (elf64-shdr-sh-addralign (gethash-strict section-name (assembler-section-headers assembler))))

(defun assembler-subsection-offsets (assembler)
  (declare (optimize (debug 3)))
  (let ((tbl (make-hash-table)))
    (loop for section-name being the hash-keys of (assembler-sections assembler)
            using (hash-value subsections)
          for align = (assembler-section-align assembler section-name)
          for offsets = (make-array (1+ (length subsections)))
          do (setf (gethash section-name tbl) offsets)
          do (loop for subsection across subsections
                   for i from 0
                   with offset = 0
                   do (setf offset (align-extend offset align))
                   do (setf (aref offsets i) offset)
                   do (incf offset (length subsection))
                   finally (setf (aref offsets (length subsections)) offset)))
    tbl))

(define-assembler-directive .align (alignment &optional (value 0))
  (assembler-align alignment value))
(define-assembler-directive .ascii (&rest strings)
  (loop for string in strings
        do (assembler-write-string string)))
(define-assembler-directive .asciz (&rest strings)
  (loop for string in strings
        do (assembler-write-string string :terminate t)))
(define-assembler-directive .bss (&optional (subsection 0))
  (assembler-push-section '.bss subsection))
(define-assembler-directive .byte (&rest bytes)
  (with-current-section (section)
    (loop for i from (length section)
          for byte in bytes
          do (large-byte-array-set-byte section i byte))))
(define-assembler-directive .data (&optional (subsection 0))
  (assembler-push-section '.data subsection))
(define-assembler-directive .global (&rest names)
  (loop for name in names
        do (let ((symbol (assembler-ensure-symbol name)))
             (setf (assembler-symbol-bind symbol) :global))))
(define-assembler-directive .hword (&rest hwords)
  (with-current-section (section)
    (loop for hword in hwords
          do (set-little-endian section 2 (length section) hword))))
(define-assembler-directive .local (&rest names)
  (loop for name in names
        do (let ((symbol (assembler-ensure-symbol name)))
             (setf (assembler-symbol-bind symbol) :local))))
(define-assembler-directive .popsection ()
  (pop (assembler-section-stack *assembler*)))
(define-assembler-directive .previous ()
  (let* ((first (pop (assembler-section-stack *assembler*)))
         (second (pop (assembler-section-stack *assembler*))))
    (assert second)
    (setf (assembler-section-stack *assembler*)
          (cons second (cons first (assembler-section-stack *assembler*))))))
(define-assembler-directive .pushsection (name &optional (subsection 0))
  (assembler-push-section name subsection))
(define-assembler-directive .rodata (&optional (subsection 0))
  (assembler-push-section '.rodata subsection))
(define-assembler-directive .section (name)
  (assembler-push-section name))
(define-assembler-directive .size (name size)
  (let ((symbol (assembler-ensure-symbol name)))
    (setf (assembler-symbol-size symbol) size)))
(define-assembler-directive .subsection (name)
  (let ((section-name (car (first (assembler-section-stack *assembler*)))))
    (assembler-push-section section-name name)))
(define-assembler-directive .text (&optional (subsection 0))
  (assembler-push-section '.text subsection))
(define-assembler-directive .type (name type)
  (let ((symbol (assembler-ensure-symbol name)))
    (setf (assembler-symbol-type symbol) type)))
(define-assembler-directive .word (&rest words)
  (with-current-section (section)
    (loop for word in words
          do (set-little-endian section 4 (length section) word))))
(define-assembler-directive .xword (&rest words)
  (with-current-section (section)
    (loop for word in words
          do (set-little-endian section 8 (length section) word))))
(define-assembler-directive .zero (size)
  (with-current-section (section)
    (loop for i from (length section)
          for _ below size
          do (large-byte-array-set-byte section i 0))))

(defstruct (segment (:constructor %make-segment))
  (sap nil :type (or sb-sys:system-area-pointer null))
  (boxed-freed-p nil :type cons)
  (length 0 :type (integer 0)))

(defun make-segment (sap length)
  (let* ((boxed-freed-p (list nil))
         (segment (%make-segment :sap sap :length length :boxed-freed-p boxed-freed-p)))
    (sb-ext:finalize segment (lambda ()
                               (when (not (car boxed-freed-p))
                                 (setf (car boxed-freed-p) t)
                                 (sb-posix:munmap sap length))))))

(defun delete-segment (segment)
  (when (and segment (not (car (segment-boxed-freed-p segment))))
    (setf (car (segment-boxed-freed-p segment)) t)
    (sb-posix:munmap (segment-sap segment) (segment-length segment))))

(defun make-segments (descriptors)
  (let ((total-size 0)
        (page-size (sb-posix:getpagesize))
        (flags (logior sb-posix:map-private sb-posix:map-anon)))
    (loop for descriptor in descriptors
          do (destructuring-bind (&key size &allow-other-keys) descriptor
               (when (> size 0)
                 (incf total-size (align-extend size page-size)))))
    (let ((sap (when (> total-size 0)
                 (sb-posix:mmap nil total-size sb-posix:prot-none flags -1 0))))
      (when sap
        (sb-posix:munmap sap total-size))
      (loop for descriptor in descriptors
            collect (destructuring-bind (&key size read write exec) descriptor
                      (when (> size 0)
                        (setq size (align-extend size page-size))
                        (let ((prot (logior
                                     (if read sb-posix:prot-read 0)
                                     (if write sb-posix:prot-write 0)
                                     (if exec sb-posix:prot-exec 0))))
                          (prog1
                              (make-segment
                               (sb-posix:mmap sap size prot (logior flags sb-posix:map-fixed) -1 0)
                               size)
                            (setf sap (sb-sys:sap+ sap size))))))))))

(defstruct loaded-object
  (segments nil :type list)
  (symbols (make-hash-table) :type hash-table))

(defun delete-loaded-object (obj)
  (loop for segment in (loaded-object-segments obj)
        do (delete-segment segment)))

(defun lookup-symbol (symbol bind local-symbols &aux local)
  (multiple-value-bind (val found) (gethash symbol local-symbols)
    (setf local found)
    (ecase bind
      (:local)
      (:global
       (when (not found)
         (setf val (get symbol 'symbol-sap))
         (setf found (and val t)))))
    (if found
        (values val local)
        (error "link error: could not find symbol ~s" symbol))))

(defun os-flush-icache (sap length)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "os_flush_icache"
                          (function sb-alien:void
                                    sb-sys:system-area-pointer
                                    sb-alien:unsigned-long))
   sap length))

(defun os-mprotect (sap length prot)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "mprotect"
                          (function sb-alien:int
                                    sb-sys:system-area-pointer
                                    sb-alien:size-t
                                    sb-alien:int))
   sap length prot))

(defun assembler-load (assembler)
  (declare (optimize (debug 3)))
  (let ((local-symbols (make-hash-table))
        (write-segment-size 0)
        (write-sections (make-dynamic-array))
        (exec-segment-size 0)
        (exec-sections (make-dynamic-array))
        (section-starts (make-hash-table))
        (relocations nil)
        (plt (make-dynamic-array))
        (got (make-dynamic-array))
        (subsection-offsets (assembler-subsection-offsets assembler)))
    (loop for section being the hash-keys of (assembler-section-headers assembler)
            using (hash-value header)
          for subsections = (gethash section (assembler-sections assembler))
          do (let* ((flags (elf64-shdr-sh-flags header))
                    (align (elf64-shdr-sh-addralign header))
                    (size (aref (gethash section subsection-offsets)
                                (length subsections)))
                    (type (cond
                            ((= 0 (logand flags shf-alloc))
                             nil)
                            ((/= 0 (logand flags shf-write))
                             (if (/= 0 (logand flags shf-execinstr))
                                 (error "can not allocate both write and exec permissions")
                                 :write))
                            (t :exec))))
               (ecase type
                 ((nil))
                 ((:write)
                  (vector-push-extend section write-sections)
                  (setf write-segment-size (align-extend write-segment-size align))
                  (setf (gethash section section-starts) (cons type write-segment-size))
                  (incf write-segment-size size))
                 ((:exec)
                  (vector-push-extend section exec-sections)
                  (setf exec-segment-size (align-extend exec-segment-size align))
                  (setf (gethash section section-starts) (cons type exec-segment-size))
                  (incf exec-segment-size size)))))
    (loop for symbol-name being the hash-keys of (assembler-symbols assembler)
            using (hash-value symbol)
          when (assembler-symbol-section symbol)
            do (setf (gethash symbol-name local-symbols)
                     (let ((section (assembler-symbol-section symbol)))
                       (destructuring-bind (segment-type . offset)
                           (gethash-strict section section-starts)
                         (cons segment-type (+ offset (assembler-symbol-value symbol)
                                               (aref (gethash section subsection-offsets)
                                                     (assembler-symbol-subsection symbol))))))))
    (loop for section being the hash-keys of (assembler-relocations assembler)
            using (hash-value assembler-relocations)
          for (section-segment-type . section-offset) = (gethash-strict section section-starts)
          do (loop for relocation in assembler-relocations
                   do (with-slots (type offset symbol addend subsection) relocation
                        (multiple-value-bind (val local)
                            (lookup-symbol symbol (assembler-symbol-bind (gethash-strict symbol (assembler-symbols assembler)))
                                           local-symbols)
                          (if local
                              (destructuring-bind (segment-type . symbol-offset) val
                                (push (list type addend
                                            section-segment-type
                                            (+ section-offset offset
                                               (aref (gethash section subsection-offsets) subsection))
                                            segment-type symbol-offset)
                                      relocations))
                              (ecase type
                                ((R_AARCH64_CALL26 R_AARCH64_JUMP26)
                                 (push (list type addend
                                             section-segment-type
                                             (+ section-offset offset
                                               (aref (gethash section subsection-offsets) subsection))
                                             :plt (length plt))
                                       relocations)
                                 (push (list 'R_AARCH64_ADR_PREL_PG_HI21 addend
                                             :plt (length plt)
                                             :got (length got))
                                       relocations)
                                 (vector-push-extend (encode-inst 'adrp x16 0) plt)
                                 (push (list 'R_AARCH64_LDST64_ABS_LO12_NC addend
                                             :plt (length plt)
                                             :got (length got))
                                       relocations)
                                 (vector-push-extend (encode-inst 'ldr x17 x16 0) plt)
                                 (vector-push-extend (encode-inst 'br x17) plt)
                                 (vector-push-extend val got))))))))
    (setf exec-segment-size (align-extend exec-segment-size 4))
    (setf write-segment-size (align-extend write-segment-size 8))
    (let ((plt-offset exec-segment-size)
          (got-offset write-segment-size))
      (incf write-segment-size (* (length got) 8))
      (incf exec-segment-size (* (length plt) 4))
      (let ((segments (make-segments (list (list :size exec-segment-size
                                                 :read t :write t :exec t)
                                           (list :size write-segment-size
                                                 :read t :write t))))
            (finished nil))
        (unwind-protect
             (let* ((loaded-object (make-loaded-object :segments segments
                                                       :symbols local-symbols))
                    (exec-segment (first segments))
                    (write-segment (second segments))
                    (exec-sap (when exec-segment (segment-sap exec-segment)))
                    (write-sap (when write-segment (segment-sap write-segment))))
               (loop for section being the hash-keys of (assembler-sections assembler)
                       using (hash-value subsections)
                     for (link-type . section-start) = (or (gethash section section-starts)
                                                           (cons nil 0))
                     for sap = (ecase link-type (:exec exec-sap) (:write write-sap) ((nil) nil))
                     when sap
                       do (loop for subsection across subsections
                                for i from 0
                                for start = (+ section-start (aref (gethash-strict section subsection-offsets) i))
                                do (loop for byte across subsection
                                         for i from 0
                                         do (setf (sb-sys:sap-ref-8 sap (+ start i)) byte))))
               (loop for inst across plt
                     for i from 0
                     do (setf (sb-sys:sap-ref-32 exec-sap (+ plt-offset (* i 4))) inst))
               (loop for addr across got
                     for i from 0
                     do (setf (sb-sys:sap-ref-64 write-sap (+ got-offset (* i 8))) addr))
               (labels ((resolve (link-type arg)
                          (ecase link-type
                            (:exec (values (sb-sys:sap+ exec-sap arg) exec-segment))
                            (:write (values (sb-sys:sap+ write-sap arg) write-segment))
                            (:plt (values (sb-sys:sap+ exec-sap (+ plt-offset (* arg 4))) exec-segment))
                            (:got (values (sb-sys:sap+ write-sap (+ got-offset (* arg 8))) write-segment)))))
                 (loop for (type addend src-type src-arg dst-type dst-arg) in relocations
                       do (let ((src-sap (resolve src-type src-arg)))
                            (setf (sb-sys:sap-ref-32 src-sap 0)
                                  (link type
                                        (sb-sys:sap-ref-32 src-sap 0)
                                        (sb-sys:sap-int (resolve dst-type dst-arg))
                                        addend
                                        (sb-sys:sap-int src-sap)))))
                 (when exec-sap
                   (os-mprotect exec-sap exec-segment-size
                                (logior sb-posix:prot-read sb-posix:prot-exec))
                   (os-flush-icache exec-sap exec-segment-size))
                 (loop for symbol being the hash-keys of local-symbols
                         using (hash-value (type . arg))
                       for bind = (assembler-symbol-bind (gethash-strict symbol (assembler-symbols assembler)))
                       do (multiple-value-bind (sap segment) (resolve type arg)
                            (setf (gethash symbol local-symbols) sap)
                            (ecase bind
                              (:local)
                              (:global
                               (setf (get symbol 'symbol-sap) sap)
                               (setf (get symbol 'symbol-segment) segment))))))
               (setf finished t)
               loaded-object)
          (when (not finished)
            (loop for segment in segments
                  do (ignore-errors
                      (delete-segment segment)))))))))

;; (defun test ()
;;   (let (;; (string-sym (make-symbol "STRING"))
;;         )
;;     (assemble-internal
;;      `#())
          ;; (assemble-internal `#((.text)
;;                                 (.global rodata_int)
;;                                 (.rodata)
;;                                 (.align 4)
;;                                 (.type rodata_int :object)
;;                                 (.size rodata_int 4)
;;                                 rodata_int
;;                                 (.word 5)

;;                                 (.data)
;;                                 (.align 8)
;;                                 (.type data_int :object)
;;                                 (.size data_int 8)
;;                                 data_int
;;                                 (.xword 6)

;;                                 (.global bss_int)
;;                                 (.bss)
;;                                 (.align 4)
;;                                 (.type bss_int :object)
;;                                 (.size bss_int 4)
;;                                 bss_int
;;                                 (.zero 4)

;;                                 (.rodata)
;;                                 (.align 8)
;;                                 ,string-sym
;;                                 (.asciz "rodata: %d, data: %d, bss: %d, arg: %d
;; ")
                
;;                                 (.text)
;;                                 (.align 4)
;;                                 (.global main)
;;                                 (.type main :function)
;;                                 main
;;                                 (stp ,x29 ,x30 ,sp -16 !)
;;                                 (mov ,x29 ,sp)
;;                                 (adr ,x0 ,string-sym)
;;                                 (mov ,w1 1)
;;                                 (mov ,w2 2)
;;                                 (mov ,w3 3)
;;                                 (mov ,w4 4)
;;                                 (bl printf)
;;                                 (bl main)
;;                                 (mov ,w0 0)
;;                                 (ldp ,x29 ,x30 ,sp 16 ?)
;;                                 (ret)
;;                                 (.size main 44)))
          ;; ))

(defun compile-asm (decl)
  (loop for inst in (rest decl)
        do (etypecase inst
             (symbol (with-current-section (section)
                       (destructuring-bind (section-name . subsection) (first (assembler-section-stack *assembler*))
                         (let ((symbol (assembler-ensure-symbol inst)))
                           (setf (assembler-symbol-value symbol) (length section)
                                 (assembler-symbol-section symbol) section-name
                                 (assembler-symbol-subsection symbol) subsection)))))
             (cons (let ((directive (gethash (first inst) *assembler-directives*)))
                     (if directive
                         (apply directive (rest inst))
                         (with-current-section (section)
                           (multiple-value-bind (inst-code relocations) (apply #'encode-inst inst)
                             (loop for relocation in relocations
                                   do (destructuring-bind (type symbol) relocation
                                        (destructuring-bind (section-name . subsection)
                                            (first (assembler-section-stack *assembler*))
                                          (assembler-ensure-symbol symbol)
                                          (push (make-assembler-relocation :type type :symbol symbol
                                                                           :offset (length section)
                                                                           :subsection subsection)
                                                (gethash section-name
                                                         (assembler-relocations *assembler*))))))
                             (set-little-endian section 4 (length section) inst-code)))))))))

(defparameter *compiler-module*
  (base:make-compiler-module
   :phase-decl-functions (list (list 'evm:post-compile-phase 'asm 'compile-asm))
   :phase-functions (list (cons 'evm:post-compile-phase
                                (lambda ()
                                  (let ((symbol (intern "*LOADED-OBJECT*")))
                                    (make-special-variable symbol)
                                    (set symbol (assembler-load *assembler*))))))
   :variables (list (cons '*assembler* 'make-assembler))))

(defun assemble-elf (assembler)
    ;; Merge subsections (merge subsections into one, flat section of bytes while adhering to alignment requirements of the section.
    ;; Also, increment every relocation in that section by the subsection offset)
    (loop for section-name being the hash-keys of (assembler-sections assembler)
            using (hash-value subsections)
          do (if (= 1 (length subsections))
                 (setf (gethash section-name (assembler-sections assembler))
                       (aref subsections 0))
                 (let ((array (make-large-byte-array))
                       (alignment (elf64-shdr-sh-addralign
                                   (gethash section-name (assembler-section-headers assembler))))
                       (subsection-offsets (make-array (length subsections))))
                   (loop for subsection across subsections
                         for i from 0
                         do (progn
                              (large-byte-array-align array (length array) alignment)
                              (setf (aref subsection-offsets i) (length array))
                              (loop for byte across subsection
                                    do (large-byte-array-set-byte array (length array) byte))))
                   (loop for relocation in (gethash section-name (assembler-relocations assembler))
                         do (incf (assembler-relocation-offset relocation)
                                  (aref subsection-offsets
                                        (assembler-relocation-subsection relocation))))
                   (setf (gethash section-name (assembler-sections assembler))
                         array))))
    ;; Add section names as symbols (adds a symbol pointing to the beginning of each section with the same name as the section,
    ;; only need this when compiling to elf and when local relocations have been renamed to the target section plus addend)
    (loop for section-name being the hash-keys of (assembler-sections assembler)
          do (assembler-make-symbol section-name :type :section :section section-name))
    ;; Remove uninterned symbols (delete certain symbols from the symbols map, replacing any relocations pointing to that symbol
    ;; with an equivalent relocation pointing to the symbol's section plus addend corresponding to the offset of the symbol
    ;; in the section)
    (loop for symbol-name in (loop for symbol being the hash-keys of (assembler-symbols assembler)
                                   when (not (symbol-package symbol))
                                     collect symbol)
          do (let ((symbol (gethash symbol-name (assembler-symbols assembler))))
               (remhash symbol-name (assembler-symbols assembler))
               (loop for relocations being the hash-values of (assembler-relocations assembler)
                     do (loop for relocation in relocations
                              when (eq (assembler-relocation-symbol relocation)
                                       symbol-name)
                                do (progn
                                     (incf (assembler-relocation-addend relocation)
                                           (assembler-symbol-value symbol))
                                     (setf (assembler-relocation-symbol relocation)
                                           (assembler-symbol-section symbol)))))))
    (let ((ehdr (make-elf64-ehdr :e-type et-rel))
          (section-headers (make-array *array-increment* :adjustable t :fill-pointer 0))
          (array (make-large-byte-array))
          (symbols (apply #'vector
                          (sort (loop for symbol being the hash-values of (assembler-symbols assembler)
                                      collect symbol)
                                (lambda (a b)
                                  (declare (ignore b))
                                  (or (eq :section (assembler-symbol-type a))
                                      (eq :local (assembler-symbol-bind a)))))))
          symbols-global-offset
          (symbols-map (make-hash-table))
          (sections-map (make-hash-table)))
      (loop for symbol across symbols
            for i from 1
            do (setf (gethash (assembler-symbol-name symbol) symbols-map) i)
            when (and (not symbols-global-offset)
                      (eq :global (assembler-symbol-bind symbol)))
              do (setf symbols-global-offset i)
            finally (when (not symbols-global-offset)
                      (setf symbols-global-offset (1+ (length symbols)))))
      (large-byte-array-align array 1 elf64-ehdr-size) ; hack to write the first area with zeros
      ;; Null section
      (vector-push-extend (make-elf64-shdr) section-headers *array-increment*)
      ;; Assembler sections
      (loop for section-name being the hash-keys of (assembler-sections assembler)
              using (hash-value section)
            for section-header = (gethash section-name (assembler-section-headers assembler))
            with relocation-headers = (make-array *array-increment* :adjustable t :fill-pointer 0)
            for alignment = (elf64-shdr-sh-addralign section-header)
            do (let ((start (align-extend (length array) alignment))
                     (header-index (length section-headers)))
                 (large-byte-array-align array (length array) alignment)
                 (loop for byte across section
                       do (large-byte-array-set-byte array (length array) byte))
                 (setf (elf64-shdr-sh-offset section-header) start)
                 (setf (elf64-shdr-sh-size section-header) (- (length array) start))
                 (vector-push-extend section-header section-headers *array-increment*)
                 (setf (gethash section-name sections-map) header-index)
                 (large-byte-array-align array (length array) 8)
                 (let ((relocations (gethash section-name (assembler-relocations assembler))))
                   (when relocations
                     (let ((rela-header (make-elf64-shdr :sh-name (assembler-shstrtab-add
                                                                   (format nil ".rela~a" (assembler-symbol-to-string section-name)))
                                                         :sh-type sht-rela :sh-offset (length array)
                                                         :sh-flags shf-info-link
                                                         :sh-size (* elf64-rela-size (length relocations))
                                                         :sh-entsize elf64-rela-size
                                                         :sh-info header-index
                                                         :sh-addralign 8)))
                       (loop for relocation in relocations
                             do (with-slots (type offset symbol addend) relocation
                                  (encode-elf64-rela
                                   array (length array)
                                   (make-elf64-rela :r-offset offset
                                                    :r-info (elf64-r-info (gethash symbol symbols-map)
                                                                          (relocation-code type))
                                                    :r-addend addend))))
                       (vector-push-extend rela-header relocation-headers *array-increment*)
                       (vector-push-extend rela-header section-headers *array-increment*)))))
            finally (loop for header across relocation-headers
                          do (setf (elf64-shdr-sh-link header) (length section-headers))))
      ;; Symbol table
      (large-byte-array-align array (length array) 8)
      (let ((start (length array)))
        (encode-elf64-sym array (length array) (make-elf64-sym))
        (loop for symbol across symbols
              do (with-slots (name value size type bind visibility section subsection) symbol
                   (encode-elf64-sym array (length array)
                                     (make-elf64-sym :st-name (gethash name (assembler-strtab-map assembler))
                                                     :st-info (elf64-st-info (ecase bind
                                                                               (:local stb-local)
                                                                               (:global stb-global)
                                                                               (:weak stb-weak))
                                                                             (ecase type
                                                                               (:notype stt-notype)
                                                                               (:object stt-object)
                                                                               (:function stt-func)
                                                                               (:section stt-section)
                                                                               (:file stt-file)
                                                                               (:common stt-common)
                                                                               (:tls stt-tls)))
                                                     :st-other (ecase visibility (:default stv-default))
                                                     :st-size size
                                                     :st-shndx (if section
                                                                   (gethash section sections-map)
                                                                   shn-undef)
                                                     :st-value value))))
        (vector-push-extend (make-elf64-shdr :sh-name (assembler-shstrtab-add '.symtab)
                                             :sh-type sht-symtab :sh-offset start
                                             :sh-size (* (1+ (length symbols)) elf64-sym-size) :sh-entsize elf64-sym-size
                                             :sh-link (1+ (length section-headers))
                                             :sh-info symbols-global-offset
                                             :sh-addralign 8)
                            section-headers *array-increment*))
      ;; String table
      (large-byte-array-align array (length array) 8)
      (let ((start (length array)))
        (vector-push-extend (make-elf64-shdr :sh-name (assembler-shstrtab-add '.strtab)
                                             :sh-type sht-strtab :sh-offset start
                                             :sh-size (length (assembler-strtab assembler))
                                             :sh-addralign 1)
                            section-headers *array-increment*)
        (loop for byte across (assembler-strtab assembler)
              do (large-byte-array-set-byte array (length array) byte)))
      ;; Section string table
      (large-byte-array-align array (length array) 8)
      (setf (elf64-ehdr-e-shstrndx ehdr) (length section-headers))
      (let ((start (length array)))
        (vector-push-extend (make-elf64-shdr :sh-name (assembler-shstrtab-add '.shstrtab)
                                             :sh-type sht-strtab :sh-offset start
                                             :sh-size (length (assembler-shstrtab assembler))
                                             :sh-addralign 1)
                            section-headers *array-increment*)
        (loop for byte across (assembler-shstrtab assembler)
              do (large-byte-array-set-byte array (length array) byte)))
      ;; Section table
      (large-byte-array-align array (length array) 8)
      (setf (elf64-ehdr-e-shoff ehdr) (length array))
      (setf (elf64-ehdr-e-shnum ehdr) (length section-headers))
      (loop for header across section-headers
            do (encode-elf64-shdr array (length array) header))
      (encode-elf64-ehdr array 0 ehdr)
      array))
