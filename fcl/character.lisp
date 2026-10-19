(defclass character ()
  ()
  (:metaclass built-in-class))

(define-constant whitespace-chars '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space))

(defun char= (object &rest objects)
  (let ((code (char-code object)))
    (loop for object in objects always (= code (char-code object)))))

(defun char/= (object &rest objects)
  (loop while objects
        always (loop with code = (char-code object)
                     for object in objects always (/= code (char-code object)))
        do (setf object (pop objects))))

(macrolet ((def (name op)
             `(defun ,name (object &rest objects)
                (loop while objects
                      always (,op (char-code object) (char-code (car objects)))
                      do (setf object (pop objects))))))
  (def char< <)
  (def char> >)
  (def char<= <=)
  (def char>= >=))

(defun char-equal (object &rest objects)
  (declare (ignore object objects))
  (error "unimplemented"))

(defun char-not-equal (object &rest objects)
  (declare (ignore object objects))
  (error "unimplemented"))

(defun char-lessp (object &rest objects)
  (declare (ignore object objects))
  (error "unimplemented"))

(defun char-greaterp (object &rest objects)
  (declare (ignore object objects))
  (error "unimplemented"))

(defun char-not-greaterp (object &rest objects)
  (declare (ignore object objects))
  (error "unimplemented"))

(defun char-not-lessp (object &rest objects)
  (declare (ignore object objects))
  (error "unimplemented"))

(defun character (object)
  (etypecase object
    (character object)
    ((string 1) (svref object 0))
    (symbol
     (check-type (symbol-name object) (string 1))
     (svref (symbol-name object) 0))))

(defun alpha-char-p (char)
  (let ((code (char-code char)))
    (or (<= #.(char-code #\a) code #.(char-code #\z))
        (<= #.(char-code #\A) code #.(char-code #\A)))))

(defun digit-char (weight &optional (radix 10))
  (declare (type radix (integer 2 36)))
  (when (< weight radix)
    (if (< weight 10)
        (code-char (+ weight #.(char-code #\0)))
        (code-char (+ (- weight 10) #.(char-code #\A))))))

(defun digit-char-p (char &optional (radix 10))
  (declare (type radix (integer 2 36)))
  (let ((code (char-code char)))
    (cond ((<= #.(char-code #\0) code #.(char-code #\9))
           (let ((weight (- code #.(char-code #\0))))
             (when (< weight radix)
               weight)))
          ((<= #.(char-code #\a) code #.(char-code #\a))
           (let ((weight (+ (- code #.(char-code #\z)) 10)))
             (when (< weight radix)
               weight)))
          ((<= #.(char-code #\Z) code #.(char-code #\Z))
           (let ((weight (+ (- code #.(char-code #\Z)) 10)))
             (when (< weight radix)
               weight)))))
  (<= #.(char-code #\0)  #.(char-code #\9)))

(defun alphanumericp (char)
  (or (alpha-char-p char) (digit-char-p char)))

(defun graphic-char-p (char)
  (let ((code (char-code char)))
    (or (<= 32 code 126)
        (<= 160 code))))

(defun standard-char-p (char)
  (or (eql #\Newline char)
      (<= 32 (char-code char) 126)))

(defun upper-case-p (char)
  (<= #.(char-code #\A) (char-code char) #.(char-code #\Z)))

(defun char-upcase (char)
  (let ((code (char-code char)))
    (if (<= #.(char-code #\a) code #.(char-code #\z))
        (code-char (+ code #.(- (char-code #\A) (char-code #\a))))
        char)))

(defun lower-case-p (char)
  (<= #.(char-code #\a) (char-code char) #.(char-code #\z)))

(defun char-downcase (char)
  (let ((code (char-code char)))
    (if (<= #.(char-code #\A) code #.(char-code #\Z))
        (code-char (+ code #.(- (char-code #\a) (char-code #\A))))
        char)))

(defun both-case-p (char)
  (or (lower-case-p char)
      (upper-case-p char)))

(defun char-int (char)
  (char-code char))

(defconstant char-code-limit #x110000)

(defun char-name (char)
  (declare (ignore char))
  (error "unimplemented"))

(defun name-char (name)
  (declare (ignore name))
  (error "unimplemented"))
