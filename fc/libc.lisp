(defpackage #:libc
  (:use #:cl #:assemble)
  (:export #:printf #:malloc))

(in-package #:libc)

(macrolet ((define-extern-symbol (name string)
             `(setf (get ',name 'symbol-sap)
                    (sb-sys:find-foreign-symbol-address ,string))))
  (define-extern-symbol printf "printf")
  (define-extern-symbol malloc "malloc"))
