(in-package "FTX")


(defmacro phi-test (name &body (expected string))
  `(test ,name
    (let ((*package* (find-package "FTX")))
      (test-equal ',expected (evm-parse ,string 'phi-main)))))

(test-suite phi
  (phi-test symbol-1 symbol "symbol")
  (phi-test symbol-2 symbol "cl-user::symbol")
  (phi-test symbol-3 symbol "cl:symbol")
  (phi-test symbol-4 :symbol ":symbol")

  (phi-test parens-0 (values) "()")
  (phi-test parens-1 x "(x)")
  (phi-test parens-2 (x y) "(x y)")
  (phi-test parens-3 (values x y) "(x, y)")

;;   (phi-test braces-0 nil "{}")
  (phi-test braces-1 x "{x}")
  (phi-test braces-2 x "{x;}")
  (phi-test braces-3 (progn nil nil) "{;;}")
  (phi-test braces-4 (progn x y) "{x; y}")
  (phi-test braces-5 (progn x y) "{
    x
    y
}")
  (phi-test braces-6 (progn x y) "{
    x;
    y;
}")
  (phi-test braces-7 (progn (x z) (y z)) "{
    x z
    y
      z
}")

  (phi-test brackets-0 (list) "[]")
  (phi-test brackets-1 (list x) "[x]")
  (phi-test brackets-2 (list x y) "[x, y]")
  (phi-test brackets-3 (list x y) "[x, y,]")
  (phi-test brackets-4 (list :x y) "[x: y]")
  (phi-test brackets-5 (list* x y z) "[x, y, ...z]")
  (phi-test brackets-6 (append (list x) y (list z)) "[x, ...y, z]")
;;   (phi-test brackets-7 (append (list w x) y (list z)) "[w, x, ...y, z]")

  (phi-test angles-0 (vector) "⟨⟩")
  (phi-test angles-1 (vector x) "⟨x⟩")
  (phi-test angles-2 (vector x y) "⟨x, y⟩")

  (phi-test funcall-0 (f) "f()")
  (phi-test funcall-1 (f x) "f(x)")
  (phi-test funcall-2 (f x) "f x")
  (phi-test funcall-3 (f x y) "f(x, y)")
  (phi-test funcall-4 (f x y) "f x y")
  (phi-test funcall-5 (apply #'f x y) "f(x, ...y)")
  (phi-test funcall-6 (apply #'f x (append y (list z))) "f(x, ...y, z)")
  (phi-test funcall-7 (f :x y) "f(x: y)")
  (phi-test funcall-8 (funcall f x) "(f)(x)")
  ;; (phi-test funcall-9 (funcall f x) "(f) (x)")

  (phi-test nth-1 (first x) "x[0]")
  (phi-test nth-2 (second x) "x[1]")
  (phi-test nth-3 (third x) "x[2]")
  (phi-test nth-4 (nth x y) "y[x]")

  (phi-test aref-0 (aref x) "x⟨⟩")
  (phi-test aref-1 (aref x y) "x⟨y⟩")
  (phi-test aref-2 (aref x y z) "x⟨y, z⟩")

  (phi-test gethash-1 (gethash x y) "y⦗x⦘")

  (phi-test quote-1 'x "'x")
  (phi-test quote-2 '(list x) "'(list x)")
  (phi-test quote-3 `x "`x")
  (phi-test quote-4 `(list x) "`(list x)")
  (phi-test quote-5 #'f "#'f")
  (phi-test quote-6 #'(setf f) "#'(setf f)")

  (phi-test return-1 (return (x y z)) "return x y z")
  
  (phi-test defun-1 (defun test ()) "
func test():")
  (phi-test defun-2 (defun test (x) (print x) (+ x 1)) "
func test(x):
    print(x)
    x + 1")
  (phi-test defun-3 (defmethod test :around ((x type) y) x) "
method[:around] test(x: type, y):
    x")
  (phi-test defun-4 (defgeneric test (x y)
                      (:argument-precedence-order x y)
                      (declare (optimize (debug 3)))
                      (:documentation "Documentation")
                      (:method-combination standard)
                      (:generic-function-class standard-generic-function)
                      (:method-class standard-method)
                      (:method :around ((x test) y) x)) "
generic test(x, y):
    argument-precedence-order (x, y)
    #[optimize(debug(3))]
    \"Documentation\"
    method-combination standard
    generic-function-class standard-generic-function
    method-class standard-method
    method[:around] (x: test, y):
        x")
  
  (phi-test macro-1 (defmacro test (x) (print x) (+ x 1)) "
macro test(x):
    print(x)
    x + 1")
  (phi-test macro-2 (define-symbol-macro test (+ x 1)) "
macro test = x + 1")
  
  (phi-test declare-1 (declare) "#[]")
  (phi-test declare-2 (declare (ignore x)) "#[ignore(x)]")
  
  (phi-test funcall-alt-1 (when x y) "when x y")
  (phi-test funcall-alt-2 (when x y) "
when x:
    y")
  (phi-test funcall-alt-3 (when x y z) "
when x:
    y
    z")
  (phi-test funcall-alt-4 (values x (progn y z)) "
values x:
    y
    z")
  
  (phi-test control-1 (unwind-protect x (print y)) "
defer print(y)
x")
  (phi-test control-2 (unwind-protect (progn x y) z w) "
defer:
    z
    w
x
y")
  (phi-test control-3 (handler-case (progn x y)
                        (error1 () z w)
                        (error2 (e) q r)
                        (error3 (e) e)
                        (error4 (e))
                        (:no-error (x y) (f x y))) "
handle error1:
    z
    w
and error2 as e:
    q
    r
and error3 as e = e
and error4 as e
else (x, y):
    f(x, y)
x
y")
  (phi-test control-4 (restart-case (progn x y)
                        (restart1 () z w)
                        (restart2 (q r) q r)) "
restart restart1():
    z
    w
and restart2(q, r):
    q
    r
x
y")
  
  (phi-test defs-1 (defpackage "TEST"
                     (:nicknames "N1" "N2")
                     (:documentation "Documentation")
                     (:use "U1" "U2")
                     (:shadow "S1" "S2")
                     (:shadowing-import-from "P1" "SI1" "SI2")
                     (:import-from "P1" "I1" "I2")
                     (:export "E1" "E2")
                     (:size 10)) "
package test:
    nicknames n1, n2
    \"Documentation\"
    use u1, u2
    shadow s1, s2
    from p1 shadowing import si1, si2
    from p1 import i1, i2
    export e1, e2
    size 10
")
  (phi-test defs-2 (defclass test (p1 p2)
                     ((s1)
                      (s2 :type t1)
                      (s3 :initform f1)
                      (s4 :type t1 :initform f1)
                      (s5 :reader test-s5 :writer (setf test-s5)))
                     (:documentation "Documentation")
                     (:metaclass m1)) "
class test(p1, p2):
    \"Documentation\"
    metaclass m1

    slot s1
    slot s2: t1
    slot s3 = f1
    slot s4: t1 = f1
    slot[reader: test-s5, writer: (setf test-s5)]
        s5")
  (phi-test defs-3 (define-condition test (p1 p2)
                     ((s1)
                      (s2 :type t1)
                      (s3 :initform f1)
                      (s4 :type t1 :initform f1)
                      (s5 :reader test-s5 :writer (setf test-s5)))
                     (:documentation "Documentation")
                     (:report r1)) "
condition test(p1, p2):
    \"Documentation\"
    report r1

    slot s1
    slot s2: t1
    slot s3 = f1
    slot s4: t1 = f1
    slot[reader: test-s5, writer: (setf test-s5)]
        s5")
  (phi-test defs-4 (defstruct (test (:conc-name "TEST-")
                                :constructor
                                (:constructor make-test)
                                (:constructor make-test (s1 s2))
                                (:copier copy-test)
                                (:predicate test-p)
                                (:include p1
                                 (sp0 nil)
                                 (sp1 nil :read-only t)
                                 (sp2 nil :type t2)
                                 (sp3 f2)
                                 (sp4 f2 :type t2))
                                (:print-object print-test)
                                (:print-function print-test)
                                (:type list)
                                :named
                                (:initial-offset 5))
                     "Documentation"
                     (s0 nil)
                     (s1 nil :read-only t)
                     (s2 nil :type t1)
                     (s3 f1)
                     (s4 f1 :type t1)) "
struct test:
    \"Documentation\"
    conc-name test-
    constructor
    constructor make-test
    constructor make-test (s1, s2)
    copier copy-test
    predicate test-p
    include p1 (sp0, readonly sp1, sp2: t2, sp3 = f2, sp4: t2 = f2)
    print-object print-test
    print-function print-test
    type list
    named
    initial-offset 5
    slot s0
    readonly slot s1
    slot s2: t1
    slot s3 = f1
    slot s4: t1 = f1")
  
  (phi-test tagbody-1 (tagbody* begin (print 1) (go begin)) "{
    @begin
    print(1)
    go begin
}")
  (phi-test let-1 (let ((x 1)) (+ x 1)) "
let x = 1
x + 1")
  (phi-test let-2 (labels ((f (x) 1)) (+ (f x) 1)) "
let f(x) = 1
f(x) + 1")
  (phi-test macrolet-1 (symbol-macrolet ((x 1)) (+ x 1)) "
macrolet x = 1
x + 1")
  (phi-test macrolet-2 (macrolet ((f (x) 1)) (+ (f x) 1)) "
macrolet f(x) = 1
f(x) + 1")
  
  (phi-test function-1 (lambda (x) (+ x 1)) "x => x + 1")
  (phi-test function-2 (lambda (x) (+ x 1)) "(x) => x + 1")
  (phi-test function-3 (lambda () (+ x 1)) "() => x + 1")
  
  (phi-test case-1 (ecase (f x) (x 1) ((y z) 2) ((t) 3)) "
case f(x):
    x -> 1
    (y, z) -> 2
    t -> 3")
  (phi-test case-2 (case (f x) (x 1) ((y z) 2) ((t) 3) (t 4)) "
case f(x):
    x -> 1
    (y, z) -> 2
    t -> 3
    4")
  (phi-test case-3 (etypecase (f x) (x 1) ((or y z) 2) (t 3)) "
typecase f(x):
    x -> 1
    (y or z) -> 2
    t -> 3")
  (phi-test case-4 (typecase (f x) (x 1) ((or y z) 2) (t 3) (t 4)) "
typecase f(x):
    x -> 1
    (y or z) -> 2
    t -> 3
    4"))
