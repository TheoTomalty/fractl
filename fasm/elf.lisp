(defpackage #:elf
  (:use #:cl #:types #:chi-symbols #:utils)
  (:export #:set-little-endian #:make-large-byte-array #:large-byte-array-set-byte #:large-byte-array-align
           #:elfclassnone #:elfclass32 #:elfclass64 #:elfdatanone #:elfdata2lsb #:elfdata2msb #:elfosabi-none #:elfosabi-sysv
           #:et-none #:et-rel #:et-exec #:et-dyn #:et-core #:et-loproc #:et-hiproc
           #:em-aarch64 #:ev-none #:ev-current #:shn-undef
           #:sht-null #:sht-progbits #:sht-symtab #:sht-strtab #:sht-rela #:sht-hash #:sht-dynamic #:sht-note
           #:sht-nobits #:sht-rel #:sht-shlib #:sht-dynsym #:sht-init-array #:sht-fini-array #:sht-preinit-array #:sht-group
           #:sht-symtab-shndx #:sht-relr #:sht-num #:sht-loos #:sht-gnu-attributes #:sht-gnu-hash #:sht-gnu-liblist #:sht-checksum #:sht-losunw
           #:sht-sunw-move #:sht-sunw-comdat #:sht-sunw-syminfo #:sht-gnu-verdef #:sht-gnu-verneed #:sht-gnu-versym #:sht-hisunw
           #:sht-hios #:sht-loproc #:sht-hiproc #:sht-louser #:sht-hiuser
           #:shf-write #:shf-alloc #:shf-execinstr #:shf-merge #:shf-strings #:shf-info-link #:shf-link-order #:shf-os-nonconforming #:shf-group
           #:shf-tls #:shf-compressed #:shf-maskos #:shf-maskproc #:shf-gnu-retain #:shf-ordered #:shf-exclude
           #:elf64-st-info #:stb-local #:stb-global #:stb-weak
           #:stt-notype #:stt-object #:stt-func #:stt-section #:stt-file #:stt-common #:stt-tls #:stn-undef
           #:stv-default #:stv-internal #:stv-hidden #:stv-protected #:elf64-r-info
           #:pt-null #:pt-load #:pt-dynamic #:pt-interp #:pt-note #:pt-shlib #:pt-phdr #:pt-tls
           #:pf-x #:pf-w #:pf-r #:pf-maskos #:pf-maskproc
           #:elf64-rela-r-offset #:elf64-rela-r-info #:elf64-rela-r-addend #:elf64-sym-st-name #:elf64-sym-st-info #:elf64-sym-st-other
           #:elf64-sym-st-shndx #:elf64-sym-st-value #:elf64-sym-st-size #:elf64-phdr-p-type
           #:elf64-phdr-p-flags #:elf64-phdr-p-offset #:elf64-phdr-p-vaddr #:elf64-phdr-p-paddr #:elf64-phdr-p-filesz #:elf64-phdr-p-memsz
           #:elf64-phdr-p-align #:elf64-shdr-sh-name #:elf64-shdr-sh-type #:elf64-shdr-sh-flags #:elf64-shdr-sh-addr #:elf64-shdr-sh-offset
           #:elf64-shdr-sh-size #:elf64-shdr-sh-link #:elf64-shdr-sh-info #:elf64-shdr-sh-addralign #:elf64-shdr-sh-entsize #:elf64-ehdr-ei-mag0
           #:elf64-ehdr-ei-mag1 #:elf64-ehdr-ei-mag2 #:elf64-ehdr-ei-mag3 #:elf64-ehdr-ei-class #:elf64-ehdr-ei-data #:elf64-ehdr-ei-version
           #:elf64-ehdr-ei-osabi #:elf64-ehdr-ei-abiversion #:elf64-ehdr-e-type #:elf64-ehdr-e-machine #:elf64-ehdr-e-version #:elf64-ehdr-e-entry
           #:elf64-ehdr-e-phoff #:elf64-ehdr-e-shoff #:elf64-ehdr-e-flags #:elf64-ehdr-e-ehsize #:elf64-ehdr-e-phentsize #:elf64-ehdr-e-phnum
           #:elf64-ehdr-e-shentsize #:elf64-ehdr-e-shnum #:elf64-ehdr-e-shstrndx
           #:elf64-rela #:elf64-sym #:elf64-phdr #:elf64-shdr #:elf64-ehdr
           #:elf64-rela-size #:elf64-sym-size #:elf64-phdr-size #:elf64-shdr-size #:elf64-ehdr-size
           #:make-elf64-rela #:make-elf64-sym #:make-elf64-phdr #:make-elf64-shdr #:make-elf64-ehdr
           #:encode-elf64-rela #:encode-elf64-sym #:encode-elf64-phdr #:encode-elf64-shdr #:encode-elf64-ehdr))

(in-package #:elf)


(defun set-little-endian (array size offset int)
  (assert (<= 0 int))
  (assert (< int (expt 2 (* 8 size))))
  (loop
    for n from 0 below size
    do (progn
         (large-byte-array-set-byte array (+ offset n) (logand int (1- (ash 1 8))))
         (setf int (ash int -8)))))

(defun make-large-byte-array ()
  (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun large-byte-array-set-byte (array offset byte)
  (loop while (< (length array) offset)
        do (vector-push-extend 0 array))
  (if (= (length array) offset)
      (vector-push-extend byte array)
      (setf (aref array offset) byte)))

(defun large-byte-array-align (array offset alignment)
  (loop while (< (length array) offset)
        do (vector-push-extend 0 array))
  (loop for i from offset below (align-extend offset alignment)
        do (vector-push-extend 0 array)))

(defparameter elf64-ehdr-size #x40)
(defparameter elf64-phdr-size #x38)
(defparameter elf64-shdr-size #x40)
(defparameter elf64-sym-size #x18)
(defparameter elf64-rela-size #x18)

(defmacro define-elf-struct (name &body fields)
  (let ((encode-sym (intern (format nil "ENCODE-~a" (symbol-name name)))))
    `(progn
       (defstruct ,name
         ,@(loop for field in fields
                 when (consp field)
                   append (destructuring-bind (name type value &key fixed) field
                            (when (not fixed)
                              `((,name ,value :type ,type))))))
       (defun ,encode-sym (array offset struct)
         ,@(loop for field in fields
                 collect (if (consp field)
                             (destructuring-bind (name type value &key fixed) field
                               `(let ((alignment (align ',type))
                                      (size (size ',type)))
                                  (large-byte-array-align array offset alignment)
                                  (setf offset (align-extend offset alignment))
                                  (set-little-endian array size offset ,(if fixed
                                                                            value
                                                                            `(slot-value struct ',name)))
                                  (incf offset size)))
                             `(progn
                                (large-byte-array-align array offset ,field)
                                (setf offset ,field))))))))

;; Values for ei-class
(defparameter elfclassnone 0)                  ; Invalid class
(defparameter elfclass32 1)                    ; 32-bit objects
(defparameter elfclass64 2)                    ; 64-bit objects

;; Values for ei-data
(defparameter elfdatanone 0)                   ; Invalid data encoding
(defparameter elfdata2lsb 1)                   ; 2's complement, little endian
(defparameter elfdata2msb 2)                   ; 2's complement, big endian

;; Values for ei-osabi
(defparameter elfosabi-none 0)                 ; UNIX System V ABI
(defparameter elfosabi-sysv 0)                 ; Alias
;; ...

;; Values for e-type (object file type)
(defparameter et-none 0)                       ; No file type
(defparameter et-rel 1)                        ; Relocatable file
(defparameter et-exec 2)                       ; Executable file
(defparameter et-dyn 3)                        ; Shared object file
(defparameter et-core 4)                       ; Core file
(defparameter et-loproc #xff00)                ; Processor-specific
(defparameter et-hiproc #xffff)                ; Processor-specific

;; Values for e-machine (architecture)
;; ...
(defparameter em-aarch64 183)                  ; ARM AARCH64
;; ...

;; Values for e-version and ei-version (version)
(defparameter ev-none 0)
(defparameter ev-current 1)

;; Special section indices
(defparameter shn-undef 0)

;; Values for sh-type
(defparameter sht-null 0)                      ; Section header table entry unused
(defparameter sht-progbits 1)                  ; Program data
(defparameter sht-symtab 2)                    ; Symbol table
(defparameter sht-strtab 3)                    ; String table
(defparameter sht-rela 4)                      ; Relocation entries with addends
(defparameter sht-hash 5)                      ; Symbol hash table
(defparameter sht-dynamic 6)                   ; Dynamic linking information
(defparameter sht-note 7)                      ; Notes
(defparameter sht-nobits 8)                    ; Program space with no data (bss)
(defparameter sht-rel 9)                       ; Relocation entries, no addends
(defparameter sht-shlib 10)                    ; Reserved
(defparameter sht-dynsym 11)                   ; Dynamic linker symbol table
(defparameter sht-init-array 14)               ; Array of constructors
(defparameter sht-fini-array 15)               ; Array of destructors
(defparameter sht-preinit-array 16)            ; Array of pre-constructors
(defparameter sht-group 17)                    ; Section group
(defparameter sht-symtab-shndx 18)             ; Extended section indices
(defparameter sht-relr 19)                     ; RELR relative relocations
(defparameter sht-num 20)                      ; Number of defined types.
(defparameter sht-loos #x60000000)             ; Start OS-specific.
(defparameter sht-gnu-attributes #x6ffffff5)   ; Object attributes.
(defparameter sht-gnu-hash #x6ffffff6)         ; GNU-style hash table.
(defparameter sht-gnu-liblist #x6ffffff7)      ; Prelink library list
(defparameter sht-checksum #x6ffffff8)         ; Checksum for DSO content.
(defparameter sht-losunw #x6ffffffa)           ; Sun-specific low bound.
(defparameter sht-sunw-move #x6ffffffa)
(defparameter sht-sunw-comdat #x6ffffffb)
(defparameter sht-sunw-syminfo #x6ffffffc)
(defparameter sht-gnu-verdef #x6ffffffd)       ; Version definition section.
(defparameter sht-gnu-verneed #x6ffffffe)      ; Version needs section.
(defparameter sht-gnu-versym #x6fffffff)       ; Version symbol table.
(defparameter sht-hisunw #x6fffffff)           ; Sun-specific high bound.
(defparameter sht-hios #x6fffffff)             ; End OS-specific type
(defparameter sht-loproc #x70000000)           ; Start of processor-specific
(defparameter sht-hiproc #x7fffffff)           ; End of processor-specific
(defparameter sht-louser #x80000000)           ; Start of application-specific
(defparameter sht-hiuser #x8fffffff)           ; End of application-specific

;; Values for sh-flags
(defparameter shf-write (ash 1 0))             ; Writable
(defparameter shf-alloc (ash 1 1))             ; Occupies memory during execution
(defparameter shf-execinstr (ash 1 2))         ; Executable
(defparameter shf-merge (ash 1 4))             ; Might be merged
(defparameter shf-strings (ash 1 5))           ; Contains nul-terminated strings
(defparameter shf-info-link (ash 1 6))         ; `sh-info' contains SHT index
(defparameter shf-link-order (ash 1 7))        ; Preserve order after combining
(defparameter shf-os-nonconforming (ash 1 8))  ; Non-standard OS specific handling required
(defparameter shf-group (ash 1 9))             ; Section is member of a group.
(defparameter shf-tls (ash 1 10))              ; Section hold thread-local data.
(defparameter shf-compressed (ash 1 11))       ; Section with compressed data.
(defparameter shf-maskos #x0ff00000)           ; OS-specific.
(defparameter shf-maskproc #xf0000000)         ; Processor-specific
(defparameter shf-gnu-retain (ash 1 21))       ; Not to be GCed by linker.
(defparameter shf-ordered (ash 1 30))          ; Special ordering requirement (Solaris).
(defparameter shf-exclude (ash 1 31))          ; Section is excluded unless referenced or allocated (Solaris).

;; How to extract and insert information held in the st-info field.
(defun elf64-st-info (bind type)
  (check-type bind (integer 0 #xf))
  (check-type type (integer 0 #xf))
  (logior (ash bind 4) type))

;; Values for st-bind subfield of st-info (symbol binding).
(defparameter stb-local 0)                     ; Local symbol
(defparameter stb-global 1)                    ; Global symbol
(defparameter stb-weak 2)                      ; Weak symbol

;; Values for st-type subfield of st-info (symbol type).
(defparameter stt-notype 0)                    ; Symbol type is unspecified
(defparameter stt-object 1)                    ; Symbol is a data object
(defparameter stt-func 2)                      ; Symbol is a code object
(defparameter stt-section 3)                   ; Symbol associated with a section
(defparameter stt-file 4)                      ; Symbol's name is file name
(defparameter stt-common 5)                    ; Symbol is a common data object
(defparameter stt-tls 6)                       ; Symbol is thread-local data object

;; Special symbol table indices
(defparameter stn-undef 0)

;; Symbol visibility specification encoded in the st-other field
(defparameter stv-default 0)                   ; Default symbol visibility rules
(defparameter stv-internal 1)                  ; Processor specific hidden class
(defparameter stv-hidden 2)                    ; Sym unavailable in other modules
(defparameter stv-protected 3)                 ; Not preemptible, not exported

;; How to extract and insert information held in the r-info field.
(defun elf64-r-info (sym type)
  (check-type sym (integer 0 #xffffffff))
  (check-type type (integer 0 #xffffffff))
  (logior (ash sym 32) type))

;; Values for p-type (segment type)
(defparameter pt-null 0)                       ; Program header table entry unused
(defparameter pt-load 1)                       ; Loadable program segment
(defparameter pt-dynamic 2)                    ; Dynamic linking information
(defparameter pt-interp 3)                     ; Program interpreter
(defparameter pt-note 4)                       ; Auxiliary information
(defparameter pt-shlib 5)                      ; Reserved
(defparameter pt-phdr 6)                       ; Entry for header table itself
(defparameter pt-tls 7)                        ; Thread-local storage segment

;; Values for p-flags (segment flags)
(defparameter pf-x 1)                          ; Segment is executable
(defparameter pf-w 2)                          ; Segment is writable
(defparameter pf-r 4)                          ; Segment is readable
(defparameter pf-maskos #x0ff00000)            ; OS-specific
(defparameter pf-maskproc #xf0000000)          ; Processor-specific


(define-elf-struct elf64-rela
  (r-offset u64 0)                             ; Address
  (r-info u64 0)                               ; Relocation type and symbol index
  (r-addend i64 0))                            ; Addend

(define-elf-struct elf64-sym
  (st-name u32 0)                              ; Symbol name (string table index)
  (st-info u8 0)                               ; Symbol type and binding
  (st-other u8 stv-default)                    ; Symbol visibility
  (st-shndx u16 stn-undef)                     ; Section index
  (st-value u64 0)                             ; Symbol value
  (st-size u64 0))                             ; Symbol size

(define-elf-struct elf64-phdr
  (p-type u32 0)                               ; Segment type
  (p-flags u32 0)                              ; Segment flags
  (p-offset u64 0)                             ; Segment file offset
  (p-vaddr u64 0)                              ; Segment virtual address
  (p-paddr u64 0)                              ; Segment physical address
  (p-filesz u64 0)                             ; Segment size in file
  (p-memsz u64 0)                              ; Segment size in memory
  (p-align u64 0))                             ; Entry size if section holds table

(define-elf-struct elf64-shdr
  (sh-name u32 stn-undef)                      ; Section name (string tbl index)
  (sh-type u32 sht-null)                       ; Section type
  (sh-flags u64 0)                             ; Section flags
  (sh-addr u64 0)                              ; Section virtual addr at execution
  (sh-offset u64 0)                            ; Section file offset
  (sh-size u64 0)                              ; Section size in bytes
  (sh-link u32 sht-null)                       ; Link to another section
  (sh-info u32 0)                              ; Additional section information
  (sh-addralign u64 0)                         ; Section alignment
  (sh-entsize u64 0))                          ; Segment alignment

(define-elf-struct elf64-ehdr
  (ei-mag0 u8 #x7f :fixed t)                   ; File identification
  (ei-mag1 u8 #x45 :fixed t)
  (ei-mag2 u8 #x4c :fixed t)
  (ei-mag3 u8 #x46 :fixed t)
  (ei-class u8 elfclass64 :fixed t)            ; File class
  (ei-data u8 elfdata2lsb :fixed t)            ; Data encoding
  (ei-version u8 ev-current :fixed t)          ; File version
  (ei-osabi u8 elfosabi-sysv :fixed t)         ; OS ABI identification
  (ei-abiversion u8 0 :fixed t)                ; ABI Version
  #x10
  (e-type u16 0)                               ; Object file tyep
  (e-machine u16 em-aarch64 :fixed t)          ; Architecture
  (e-version u32 ev-current :fixed t)          ; Object file version
  (e-entry u64 0)                              ; Entry point virtual address
  (e-phoff u64 0)                              ; Program header table file offset
  (e-shoff u64 0)                              ; Section header table file offset
  (e-flags u32 0)                              ; Processor-specific flags
  (e-ehsize u16 elf64-ehdr-size :fixed t)      ; ELF header size in bytes
  (e-phentsize u16 elf64-phdr-size :fixed t)   ; Program header table entry size
  (e-phnum u16 0)                              ; Program header table entry count
  (e-shentsize u16 elf64-shdr-size :fixed t)   ; Section header table entry size
  (e-shnum u16 0)                              ; Section header table entry count
  (e-shstrndx u16 shn-undef))                  ; Section header string table index

