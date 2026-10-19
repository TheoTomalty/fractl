(define-condition print-not-readable (error)
  ((object
    :initarg :object
    :reader print-not-readable-object)))

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  (let ((stream-sym (make-symbol "STREAM"))
        (object-sym (make-symbol "OBJECT")))
    `(let ((,object-sym ,object)
           (,stream-sym ,stream))
       (when *print-readably*
         (error 'print-not-readable :object ,object-sym))
       (write-string "#<" ,stream-sym)
       (when ,type
         (prin1 (type-of ,object-sym) ,stream-sym))
       (progn ,@body)
       (when ,identity
         (unless (eql #\Space (%stream-prev-char ,stream-sym))
           (write-string " " ,stream-sym))
         (write-string "{" ,stream-sym)
         (prin1 (%value-id ,object-sym) ,stream-sym)
         (write-string "}" ,stream-sym))
       (write-string "#>" ,stream-sym))))

(defun default-print (object stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defgeneric print-object (object stream)
  (:method ((object standard-object))
    (default-print object stream)))

(defvar *print-array* t)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *print-case* :upcase)
(defvar *print-circle* nil) ;; TODO
(defvar *print-escape* t)
(defvar *print-gensym* t)
(defvar *print-level*) ;; TODO
(defvar *print-length*) ;; TODO
(defvar *print-lines*) ;; TODO
(defvar *print-miser-width*) ;; TODO
(defvar *print-pprint-dispatch*) ;; TODO
(defvar *print-pretty*) ;; TODO
(defvar *print-readably* nil)
(defvar *print-right-margin*) ;; TODO

(define-constant radix-chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun print-positive-integer (i base stream)
  (let (chars)
    (loop while (<= 0 i)
          do (multiple-value-bind (i-new mod) (truncate i base)
               (push (aref radix-chars mod) chars)
               (setf i i-new)))
    (loop for ch in chars do (write-char ch stream))))

(defun print-radix (base stream)
  (when *print-radix*
    (case base
      (2 (write-string "#b" stream))
      (8 (write-string "#o" stream))
      (16 (write-string "#x" stream))
      (t
       (write-string "#" stream)
       (print-positive-integer base 10 stream)
       (write-string "r" stream)))))

(defmethod print-object ((i integer) stream)
  (when (= 0 i)
    (write-char #\0 stream)
    (return-from print-object))
  (let ((base *print-base*))
    (check-type base (integer 2 36))
    (print-radix base stream)
    (when (< i 0)
      (write-char #\- stream))
    (print-positive-integer (abs i) base stream)))

(defmethod print-object ((r ratio) stream)
  (let ((base *print-base*))
    (check-type base (integer 2 36))
    (print-radix base stream)
    (when (< r 0)
      (write-char #\- stream))
    (print-positive-integer (abs (numerator r)) base stream)
    (write-char #\/ stream)
    (print-positive-integer (denominator r) base stream)))

(defmethod print-object ((f float) stream)
  (error "unimplemented"))

(defmethod print-object ((c complex) stream)
  (write-string "#C(" stream)
  (print-object (realpart c) stream)
  (write-char #\Space stream)
  (print-object (imagpart c) stream))

(defun printer-escaping-p ()
  (or *print-readably* *print-escape*))

(defmethod print-object ((char character) stream)
  (if (printer-escaping-p)
      (if (graphic-char-p char)
          (progn
            (write-string "#\\" stream)
            (write-char char stream))
          (error "unimplemented")) ;; Print name like #\Newline
      (write-char char stream)))

(defun potential-number-p (name)
  (when (eql 0 (length name))
    (return-from potential-number-p nil))
  (let (one-digit-p
        one-radix-digit-p
        decimal-points-p)
    (and (loop for ch across name
               with prev-ch
               always (or (and (digit-char-p ch) (setf one-digit-p t))
                          (and (digit-char-p ch *read-base*) (setf one-radix-digit-p t))
                          (and (eql #\. ch) (setf decimal-points-p t))
                          (member ch '(#\+ #\- #\/ #\^ #\_ ))
                          (and (alpha-char-p ch) (when prev-ch (not (alpha-char-p prev-ch)))))
               do (setf prev-ch ch))
         (if decimal-points-p one-digit-p one-radix-digit-p)
         (let ((ch (aref name 0)))
           (or (digit-char-p ch *read-base*)
               (member ch '(#\+ #\- #\. #\^ #\_))))
         (not (member (aref name (1- (length name))) '(#\+ #\-))))))

(define-constant constituent-chars "a-zA-Z0-9!$%&*+\\-./<=>?@[]^_{}~")

(defun constituent-char-p (char)
  (loop for ch across constituent-chars
        thereis (eql char ch)))

(defun print-symbol-name (name stream)
  (flet ((escape ()
           (write-char #\| stream)
           (loop for ch across name
                 do (when(member ch '(#\\ #\|))
                      (write-char #\\ stream))
                    (write-char ch stream))
           (write-char #\| stream))
         (print-case (upcase)
           (ecase *print-case*
             (:upcase (write-string (if upcase name (string-upcase name)) stream))
             (:downcase (write-string (if upcase (string-downcase name) name) stream))
             (:capitalize (write-string (string-capitalize name) stream)))))
    (if (and (loop for ch across name never (or (eql #\: ch) (not (constituent-char-p ch))))
             (not (potential-number-p name))
             (not (equal "" name)))
        (ecase (readtable-case *readtable*)
          (:upcase
           (if (loop for ch across name always (upper-case-p ch))
               (print-case t)
               (escape)))
          (:downcase
           (if (loop for ch across name always (lower-case-p ch))
               (print-case nil)
               (escape)))
          (:preserve
           (write-string name stream))
          (:invert
           (loop for ch across name
                 do (cond ((upper-case-p ch)
                           (write-char (char-downcase ch) stream))
                          ((lower-case-p ch)
                           (write-char (char-upcase ch) stream))
                          (t
                           (write-char ch stream))))))
        (escape))))

(defmethod print-object ((sym symbol) stream)
  (if (printer-escaping-p)
      (let ((package (symbol-package sym))
            (name (symbol-name sym)))
        (multiple-value-bind (this-sym this-found) (find-symbol name)
          (cond ((and this-found (eql sym this-sym))
                 (print-symbol-name name stream))
                ((eql *keyword-package* package)
                 (write-char #\: stream)
                 (print-symbol-name name stream))
                ((null package)
                 (when *print-gensym*
                   (write-string "#:" stream))
                 (print-symbol-name name stream))
                (t
                 (ecase (nth-value 1 (find-symbol name package))
                   (:external
                    (print-symbol-name (package-name package) stream)
                    (write-string ":" stream)
                    (print-symbol-name name stream))
                   (:internal
                    (print-symbol-name (package-name package) stream)
                    (write-string "::" stream)
                    (print-symbol-name name stream)))))))
      (ecase *print-case*
        (:upcase (write-string (string-upcase (symbol-name sym)) stream))
        (:downcase (write-string (string-downcase (symbol-name sym)) stream))
        (:capitalize (write-string (string-capitalize (symbol-name sym)) stream)))))

(defmethod print-object ((string string) stream)
  (if (printer-escaping-p)
      (progn
        (write-char #\" stream)
        (loop for ch across string
              do (when (or (eql #\\ ch)
                           (eql #\" ch))
                   (write-char #\\ stream))
                 (write-char ch stream))
        (write-char #\" stream))
      (write-string string stream)))

(defmethod print-object ((list cons) stream)
  (write-string "(" stream)
  (print-object (car list) stream)
  (loop while (consp (cdr list))
        do (setf list (cdr list))
           (write-string " " stream)
           (print-object (car list) stream))
  (unless (null (cdr list))
    (write-string " . " stream)
    (print-object (cdr list) stream))
  (write-string ")" stream))

(defmethod print-object ((bits bit-vector) stream)
  (if *print-array*
      (progn
        (write-string "#*" stream)
        (loop for bit across bits
              do (write-char (ecase bit (1 #\1) (0 #\0)) stream)))
      (default-print bits stream)))

(defmethod print-object ((vec vector) stream)
  (if *print-array*
      (progn
        (write-string "#(" stream)
        (loop for elem across vec
              with first = t
              do (if first
                     (setf first nil)
                     (write-char #\Space stream))
                 (print-object elem stream))
        (write-string ")" stream))
      (default-print vec stream)))

(defmethod print-object ((array array) stream)
  (if *print-array*
      (let* ((n (array-rank array))
             (dims (apply #'vector (array-dimensions array)))
             (indices (make-array (length dims) :initial-element 0))
             (i 0))
        (write-string "#" stream)
        (print-positive-integer n 10 stream)
        (write-string "A" stream)
        (loop for i below n do (write-string "(" stream))
        (block nil
          (labels ((increment (j)
                     (when (< j (- n 1))
                       (write-string ")" stream))
                     (when (< j 0)
                       (return))
                     (let ((new-index (1+ (aref indices j))))
                       (if (= new-index (aref dims j))
                           (progn
                             (setf (aref indices j) 0)
                             (increment (1- j)))
                           (progn
                             (setf (aref indices j) new-index)
                             (write-string " " stream))))
                     (when (< j (- n 1))
                       (write-string "(" stream))))
            (loop (print-object (row-major-aref array i) stream)
                  (incf i)
                  (increment (1- n))))))
      (default-print array stream)))

(defmethod print-object ((rand random-state) stream)
  (error "unimplemented"))

(defmethod print-object ((pathname pathname) stream)
  (write-string "#P" stream)
  (print-object (the string (namestring pathname)) stream))

(defmethod print-object ((struct structure-object) stream)
  (error "unimplemented"))


(defvar *format-directives* (make-hash-table))
(defvar *format-args*)
(defvar *format-args-idx*)

(defun pop-format-args ()
  (aref *format-args* (incf* *format-args-idx*)))

(defun peek-format-args ()
  (aref *format-args* *format-args-idx*))

(defun more-format-args-p ()
  (< *format-args-idx* (length *format-args*)))

(defmacro define-directive (name char args &body body)
  `(progn
     (setf (gethash ,char *format-directives*) ',name)
     (defun ,name ,args ,@body)))

(defmacro modifier-case (colon at &body (neither-case
                                         &optional
                                           (colon-case :invalid)
                                           (at-case :invalid)
                                           (both-case :invalid)))
  (flet ((transform (case)
           (case case
             (:invalid `(error "The format directive does not take the given modifiers"))
             (:unimplemented `(error "unimplemented"))
             (t case))))
    `(cond ((and (not ,colon) (not ,at))
            ,(transform neither-case))
           (,colon ,(transform colon-case))
           (,at ,(transform at-case))
           (t ,(transform both-case)))))

(define-directive tilde-character #\C (stream colon at)
  (let ((ch (pop-format-args)))
    (modifier-case colon at
      (write-char ch stream)
      :unimplemented
      (let ((*print-escape* t))
        (print-object ch stream))
      :unimplemented)))

(define-directive tilde-newline #\% (stream colon at &optional (n 1))
  (modifier-case colon at
    (loop for i below n
          do (terpri stream))))

(define-directive tilde-fresh-line #\& (stream colon at &optional (n 1))
  (modifier-case colon at
    (when (< 0 n)
      (fresh-line stream)
      (loop for i below (1- n)
            do (terpri stream)))))

(define-directive tilde-page #\| (stream colon at &optional (n 1))
  (modifier-case colon at
    (loop for i below n
          do (write-char #\Page stream))))

(define-directive tilde-tilde #\~ (stream colon at &optional (n 1))
  (modifier-case colon at
    (loop for i below n
          do (write-char #\~ stream))))

(defun pad (n char stream)
  (loop repeat n do (write-char char stream)))

(defun tilde-print-integer (print-comma-p print-sign-p stream base mincol padchar commachar comma-interval)
  (let ((n (pop-format-args))
        chars neg)
    (when (< n 0)
      (setf neg t
            n (- n)))
    (loop while (< 0 n) and i from 0
          do (when (and print-comma-p (= 0 (mod i comma-interval)))
               (push commachar chars))
             (multiple-value-bind (next-n rem) (truncate n base)
               (setf n next-n)
               (push (aref radix-chars rem) chars)))
    (when (and print-sign-p (not neg)) (push #\+ chars))
    (when neg (push #\- chars))
    (pad (- mincol (length chars)) padchar stream)
    (loop for ch in chars
          do (write-char ch stream))))

(defmacro defaults (&body pairs)
  `(setf ,@(loop for (name default) in pairs collect `(,name (or ,name ,default)))))

(define-constant small-number-names
    #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven"
      "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(define-constant small-ordinals
    #("zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth" "eleventh"
      "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(define-constant medium-number-names
    #("twenty" "thirty" "fourty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(define-constant medium-ordinals
    #("twentieth" "thirtieth" "fourtieth" "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(define-constant large-number-names
    #("thousand" "million" "billion" "trillion" "quadrillion" "quintillion" "sextillion"
      "septillion" "octillion" "nonillion" "decillion" "undecillion" "duodecillion"
      "tredecillion" "quattuordecillion" "quindecillion" "sexdecillion" "septendecillion"
      "octodecillion" "novemdecillion" "vigintillion"))

(defparameter *print-ordinal* nil)

(defun print-cardinal (n stream)
  (cond ((< n 20)
         (if *print-ordinal*
             (write-string (aref small-ordinals n) stream)
             (write-string (aref small-number-names n) stream)))
        ((< n 100)
         (multiple-value-bind (tens ones) (truncate n 10)
           (if (and *print-ordinal* (= 0 ones))
               (write-string (aref medium-ordinals (- tens 2)) stream)
               (write-string (aref medium-number-names (- tens 2)) stream))
           (when (< 0 ones)
             (write-char #\- stream)
             (print-cardinal ones stream))))
        ((< n 1000)
         (multiple-value-bind (hundreds tens) (truncate n 100)
           (print-cardinal hundreds stream)
           (write-string " hundred" stream)
           (if (< 0 tens)
               (progn
                 (write-char #\Space stream)
                 (print-cardinal tens stream))
               (when *print-ordinal*
                 (write-string "th" stream)))))
        ((< n (expt 10 66))
         (loop with first = t
               for i from (1- (length large-number-names)) downto 0
               do (multiple-value-bind (illions rem) (truncate n (expt 10 (+ (* 3 i) 3)))
                    (when (< 0 illions)
                      (if first
                          (setf first nil)
                          (write-char #\Space stream))
                      (print-cardinal illions stream)
                      (write-char #\Space stream)
                      (write-string (aref large-number-names i) stream))
                    (setf n rem))
               finally (if (< 0 n)
                           (progn
                             (if first
                                 (setf first nil)
                                 (write-char #\Space stream))
                             (print-cardinal n stream))
                           (when *print-ordinal*
                             (write-strig "th" stream)))))
        (t
         (error "Number is too large to print ~s." n))))

(defun print-ordinal (n stream)
  (let ((*print-ordinal* t))
    (print-cardinal n stream)))

(defvar *old-roman* nil)

(defun print-roman-component (n stream ones fives tens)
  (cond ((<= n (if *old-roman* 4 3))
         (loop repeat n do (write-char ones stream)))
        ((= n 4)
         (write-char ones stream)
         (write-char fives stream))
        ((<= n (if *old-roman* 9 8))
         (write-char fives stream)
         (loop repeat n do (write-char ones stream)))
        ((= n 9)
         (write-char ones stream)
         (write-char tens stream))
        (t (error "unreachable"))))

(defun print-roman (n stream)
  (when (<= n 0)
    (error "Roman numerals can not represent the number ~s." n))
  (multiple-value-bind (thousands n) (truncate n 1000)
    (if (<= thousands (if *old-roman* 4 3))
        (loop repeat n do (write-char #\M stream))
        (error "Number is too large to print ~s." n))
    (loop for (size ones fives tens) in '((100 #\C #\D #\M) (10 #\X #\L #\C) (1 #\I #\V #\X))
          do (multiple-value-bind (component rem) (truncate n 10)
               (print-roman-component component stream ones fives tens)
               (setf n rem)))))

(defun print-old-roman (n stream)
  (let ((*old-roman* t))
    (print-roman n stream)))

(define-directive tilde-radix #\R (stream colon at &optional radix mincol padchar commachar comma-interval)
  (if radix
      (let ((n (pop-format-args)))
        (defaults (radix 10) (mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
        (modifier-case colon at
          (print-cardinal n stream)
          (print-ordinal n stream)
          (print-roman n stream)
          (print-old-roman n stream)))
      (modifier-case colon at
        (tilde-print-integer nil nil radix mincol padchar commachar comma-interval)
        (tilde-print-integer t   nil radix mincol padchar commachar comma-interval)
        (tilde-print-integer nil t   radix mincol padchar commachar comma-interval)
        (tilde-print-integer t   t   radix mincol padchar commachar comma-interval))))

(defmacro ((define-tilde-radix (name char base)
             `(define-directive ,name ,char (stream colon at &optional mincol padchar commachar comma-interval)
                (if (integerp (peek-format-args))
                    (modifier-case colon at
                      (tilde-print-integer nil nil ,base mincol padchar commachar comma-interval)
                      (tilde-print-integer t   nil ,base mincol padchar commachar comma-interval)
                      (tilde-print-integer nil t   ,base mincol padchar commachar comma-interval)
                      (tilde-print-integer t   t   ,base mincol padchar commachar comma-interval))
                    (let ((*print-escape* nil)
                          (*print-readably* nil)
                          (*print-radix* nil)
                          (*print-base* ,base))
                      (print-object (pop-format-args) stream))))))
  (define-tilde-radix tilde-binary      #\B 2)
  (define-tilde-radix tilde-octal       #\O 8)
  (define-tilde-radix tilde-decimal     #\D 10)
  (define-tilde-radix tilde-hexadecimal #\X 16))

(define-directive tilde-float1 #\F (stream colon at &rest args)
  (error "unimplemented"))

(define-directive tilde-float2 #\E (stream colon at &rest args)
  (error "unimplemented"))

(define-directive tilde-float3 #\G (stream colon at &rest args)
  (error "unimplemented"))

(define-directive tilde-float4 #\$ (stream colon at &rest args)
  (error "unimplemented"))

(macrolet ((def (name char &body bindings)
             `(define-directive ,name ,char (stream colon at &optional mincol colinc minpad padchar)
                (defaults (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                (let (,@bindings
                      (arg (pop-format-args)))
                  (let* ((string (if (and colon (null arg))
                                     "()"
                                     (with-output-to-string (s) (print-object arg s))))
                         (distance (max 0 (- mincol (length string))))
                         (padding (max minpad (* (ceiling distance colinc) distance))))
                    (if at
                        (progn
                          (pad padding padchar stream)
                          (write-string string stream))
                        (progn
                          (write-string string stream)
                          (pad padding padchar stream))))))))
  (def tilde-aesthetic #\A (*print-escape* nil) (*print-readably* nil))
  (def tilde-standard  #\S (*print-escape* t)))

(define-directive tilde-write #\W (stream colon at)
  (let ((arg (pop-format-args)))
    (modifier-case colon at
      (write arg stream)
      (write arg stream :pretty t)
      (write arg stream :level nil :length nil)
      (write arg stream :pretty t :level nil :length nil))))

(define-directive tilde-conditional-newline #\_ (stream colon at)
  (error "unimplemented"))

(define-directive tilde-logical-block #\< (stream colon at)
  (error "unimplemented"))

(define-directive tilde-indent #\I (stream colon at)
  (error "unimplemented"))

(define-directive tilde-call-function #\/ (stream colon at name &rest args)
  (setf name (string-upcase name))
  (apply
   (aif (position #\: name)
        (let ((package-name (subseq name 0 (incf* it)))
              (internal (when (eql #\: (aref name it))
                          (incf it) t))
              (symbol-name (subseq name it)))
          (declare (ignore internal))
          (intern symbol-name package-name))
        (intern name "COMMON-LISP-USER"))
   stream (pop-format-args) colon at args))

(define-directive tilde-tabulate #\T (stream colon at &optional colnum colinc)
  (defaults (colnum 1) (colinc 1))
  (modifier-case colon at
    (let ((stream-column (stream-column stream)))
      (if (< stream-column colnum)
          (progn
            (pad (- colnum stream-column) #\Space stream)
            (setf stream-column 0))
          (decf stream-column colnum))
      (pad (* (ceiling stream-column colinc) colinc) #\Space stream))
    :unimplemented
    (progn
      (pad colnum #\Space stream)
      (pad (* (ceiling (stream-column stream) colinc) colinc) #\Space stream))
    :unimplemented))

(define-directive tilde-go-to #\* (stream colon at &optional n)
  (modifier-case colon at
    (incf *format-args-idx* (or n 1))
    (decf *format-args-idx* (or n 1))
    (setf *format-args-idx* (or n 0))
    :invalid))

(define-directive tilde-separator #\; (stream colon at)
  (error "Using a format separator outside of a valid context."))

(defun parse-conditional-expression-tokens (tokens)
  (loop with component
        with components
        with default
        for token in tokens
        when (and (consp token) (eql 'tilde-separator (car token)))
          do (modifier-case (cadr token) (caddr token)
               nil (if default
                       (error "Format separator ~~:; must be last in group.")
                       (setf default t))
               :invalid :invalid)
             (push (nreverse component) components)
             (setf component nil)
        else do (push token component)
        finally (return
                  (if default
                      (values (nreverse components) (nreverse component) t)
                      (values (nreverse (list* (nreverse component) components)) nil nil)))))

(define-directive tilde-conditional-expression #\[ (stream start-token tokens end-token)
  (destructuring-bind (colon at) (cdr end-token)
    (modifier-case colon at nil :invalid :invalid :invalid))
  (destructuring-bind (colon at &rest args) (cdr start-token)
    (modifier-case colon at
      (destructuring-bind (&optional n) args
        (unless n
          (setf n (pop-format-args)))
        (multiple-value-bind (strs default) (parse-conditional-expression-tokens tokens)
          (exec-format-tokens stream (if (<= 0 n (1- (length strs))) (nth n strs) default))))
      (destructuring-bind () args
        (multiple-value-bind (strs default default-p) (parse-conditional-expression-tokens tokens)
          (when default-p
            (error "Format conditional expression found an unexpected default case."))
          (unless (= 2 (length strs))
            (error "Format conditional expression requires both an alternative and a consequent."))
          (if (pop-format-args)
              (exec-format-tokens stream (cadr strs))
              (exec-format-tokens stream (car strs)))))
      (destructuring-bind () args
        (multiple-value-bind (strs default default-p) (parse-conditional-expression-tokens tokens)
          (when default-p
            (error "Format conditional expression found an unexpected default case."))
          (unless (= 1 (length strs))
            (error "Format conditional expression requires exactly one consequent."))
          (if (peek-format-args)
              (exec-format-tokens stream (car strs))
              (pop-format-args))))
      :invalid)))

(define-constant format-escape (make-symbol "ESCAPE"))
(define-constant format-escape-colon (make-symbol "ESCAPE-COLON"))
(defvar *format-colon-escape*)
(defvar *format-colon-end*)

(defmacro catch-format-escape (&body body)
  `(let ((*format-colon-escape* nil)
         (*format-colon-end* nil))
     (catch format-escape
       ,@body)))

(define-directive tilde-iteration #\{ (stream start-token tokens end-token &aux at-least-once-p)
  (destructuring-bind (colon at) (cdr end-token)
    (modifier-case colon at nil (setf at-least-once-p t) :invalid :invalid))
  (unless tokens
    (setf tokens (parse-format-string (pop-format-args))))
  (destructuring-bind (colon at &optional n) (cdr start-token)
    (let ((*format-args* (make-array :initial-contents (pop-format-args)))
          (*format-args-idx* 0))
      (catch-format-escape
        (loop for i from 0
              for first = t then nil
              while (and (or (and at-least-once-p first)
                             (more-format-args-p))
                         (or (not n) (< i n)))
              do (exec-format-tokens stream tokens))))
    (loop for i from 0
          for args-list = (or (pop-format-args)
                              (and at-least-once-p (list nil)))
            then (cdr args-list)
          while args-list
          for args = (car args-list)
          while (or (not n) (< i n))
          do (let ((*format-args* (make-array :initial-contents args))
                   (*format-args-idx* 0))
               (when (eql format-escape-colon
                          (catch-format-escape
                            (setf *format-colon-escape* t)
                            (setf *format-colon-end* (null (cdr args-list)))
                            (exec-format-tokens stream tokens)))
                 (return))))
    (catch-format-escape
      (loop for i from 0
            for first = t then nil
            while (and (or (and at-least-once-p first)
                           (more-format-args-p))
                       (or (not n) (< i n)))
            do (exec-format-tokens stream tokens)))
    (loop for i from 0
          while (and (or (and at-least-once-p first)
                         (more-format-args-p))
                     (or (not n) (< i n)))
          do (let ((colon-end (= (length *format-args*) (1+ *format-args-idx*)))
                   (*format-args* (make-array :initial-contents (pop-format-args)))
                   (*format-args-idx* 0))
               (when (eql format-escape-colon
                          (catch-format-escape
                            (setf *format-colon-escape* t)
                            (setf *format-colon-end* colon-end)
                            (exec-format-tokens stream tokens)))
                 (return))))))

(define-directive tilde-recursive-processing #\? (stream colon at)
  (modifier-case colon at
    (let ((tokens (parse-format-string (pop-format-args)))
          (*format-args* (make-array :initial-contents (pop-format-args)))
          (*format-args-idx* 0))
      (catch-format-escape
        (exec-format-tokens stream tokens)))
    :invalid
    (catch-format-escape
      (exec-format-tokens stream (parse-format-string (pop-format-args))))
    :invalid))

(define-directive tilde-case-conversion #\( (stream start-token tokens end-token)
  (destructuring-bind (colon at) (cdr end-token)
    (modifier-case colon at nil :invalid :invalid :invalid))
  (destructuring-bind (colon at &aux escaped) (cdr start-token)
    (let ((string (with-output-to-string (s)
                    (block nil
                      (catch-format-escape
                        (exec-format-tokens s tokens)
                        (return))
                      (setf escaped t)))))
      (modifier-case colon at
        (write-string (nstring-downcase string) stream)
        (write-string (nstring-capitalize string) stream)
        (progn
          (nstring-downcase string)
          (loop for ch across string and i from 0
                when (alpha-char-p ch)
                     (setf (aref string i) (char-upcase ch)))
          (write-string string stream))
        (write-string (nstring-upcase string) stream))
      (when escaped
        (throw format-escape nil)))))

(define-directive tilde-plural #\P (stream colon at)
  (let ((arg (if colon (aref *format-args* (1- *format-args-idx*)) (pop-format-args))))
    (write-string
     (if at
        (if (eql arg 1) "y" "ies")
        (if (eql arg 1) "" "s"))
     stream)))

(define-directive tilde-escape-upward #\^
    (stream colon at &optional (n nil one-arg-p) (m nil two-arg-p) (r nil three-arg-p))
  (let ((cond (cond (three-arg-p (<= n m r))
                    (two-arg-p   (= n m))
                    (one-arg-p   (= 0 n))
                    (t (if colon
                           *format-colon-end*
                           (= 0 (- (length *format-args*) *format-args-idx*)))))))
    (modifier-case colon at
      (when cond
        (throw format-escape nil))
      (if *format-colon-escape*
          (when cond
            (throw format-escape format-escape-colon))
          (error "You may not use a colon modifier for this escape."))
      :invalid
      :invalid)))

(define-directive tilde-ignored-newline #\Newline (stream colon at)
  (error "unreachable"))

(defun parse-format-string (string)
  (let ((delimiter-stack nil)
        (tokens nil)
        (start 0)
        (i 0))
    (labels ((next-char ()
               (aref string i))
             (read-char ()
               (aref string (incf* i)))
             (parse-sign ()
               (cond ((eql #\+ (next-char)) 1)
                     ((eql #\- (next-char)) -1)
                     (t 1)))
             (parse-digits ()
               (when (digit-char-p (next-char))
                 (loop with sum = 0
                       while (awhen (digit-char-p (next-char))
                               (setf sum (+ (* sum 10) it))
                               (incf i))
                       finally (return sum))))
             (parse-parameter ()
               (let ((sign (parse-sign)))
                 (aif (parse-digits)
                      (* sign it)
                      (when (eql #\' (next-char))
                        (incf i)
                        (read-char)))))
             (parse-directive ()
               (with-ext-lists ((params params-end))
                 (loop (let ((param (parse-parameter))
                             (comma (awhen (eql #\, (next-char))
                                      (incf i)
                                      t)))
                         (when (or param comma)
                           (push-tail param params-end))
                         (unless comm
                           (return))))
                 (let (colon at)
                   (loop while (or (and (eql #\: (next-char))
                                        (if colon
                                            (error "Too many colon modifiers.")
                                            (setf colon t)))
                                   (and (eql #\@ (next-char))
                                        (if at
                                            (error "Too many at modifiers.")
                                            (setf at t))))
                         do (incf i))
                   (let ((char (read-char)))
                     (multiple-value-bind (name found) (gethash (char-upcase char) *format-directives*)
                       (unless found
                         (error "The char ~s does not represent a valid format directive." char))
                       (if (eql #\/ char)
                           (list* name colon at
                                  (concatenate 'string (loop for ch = (read-char) until (eql #\/ ch) collect ch))
                                  params)
                           (list* name colon at params) char))))))
             (end-string ()
               (when (< start i)
                 (push (subseq string start i) tokens))))
      (loop do (if (eql (aref string i) #\~)
                   (progn
                     (end-string)
                     (multiple-value-bind (token char) (parse-directive)
                       (case char
                         ((#\Newline)
                          (destructuring-bind (colon at) (cdr token)
                            (modifier-case colon at
                              (loop for j from i while (member (next-char) whitespace-chars) do (incf i))
                              ()
                              (progn
                                (push (string #\Newline) tokens)
                                (loop for j from i while (member (next-char) whitespace-chars) do (incf i)))
                              :invalid)))
                         ((#\< #\[ #\{ #\()
                          (push token tokens)
                          (push tokens delimiter-stack)
                          (setf tokens nil))
                         ((#\> #\] #\} #\))
                          (let* ((prev-delimiters (pop delimiter-stack))
                                 (start-token (pop prev-delimiters)))
                            (unless (eql (car start-token) (car token))
                              (error "Unmatching delimiters."))
                            (push (list (car token) start-token (nreverse tokens) token) prev-delimiters)
                            (setf tokens prev-delimiters)))
                         (t (push token tokens))))
                     (setf start i))
                   (incf i))
            finally (end-string)
                    (return (nreverse tokens))))))

(defun exec-format-tokens (stream tokens)
  (loop for elem in tokens
        do (etypecase elem
             (string
              (write-string elem stream))
             (cons
              (apply (car elem) stream (cdr elem))))))

(defun format (destination constrol-string &rest args)
  (let ((stream ((case destination
                   ((t) *standard-output*)
                   ((nil) (make-string-output-stream))
                   (t destination)))))
    (let ((*format-args* (make-array :initial-contents args))
          (*format-args-idx* 0))
      (catch-format-escape
        (exec-format-tokens stream (parse-format-string control-string))))
    (when (null destination)
      (get-output-stream-string stream))))

(defun formatter (control-string)
  (let ((tokens (parse-format-string control-string)))
    (lambda (*standard-output* &rest args)
      (let ((*format-args* (make-array :initial-contents args))
            (*format-args-idx* 0))
        (catch-format-escape
          (exec-format-tokens *standard-output* tokens))
        (loop for i from *format-args-idx* below (length *format-args*)
              collect (aref *format-args* i))))))

(defun copy-pprint-dispatch (&rest args)
  (error "unimplemented"))

(defun pprint-dispatch (&rest args)
  (error "unimplemented"))

(defun pprint-exit-if-list-exhausted (&rest args)
  (error "unimplemented"))

(defun pprint-fill (&rest args)
  (error "unimplemented"))

(defun pprint-linear (&rest args)
  (error "unimplemented"))

(defun pprint-tabular (&rest args)
  (error "unimplemented"))

(defun pprint-indent (&rest args)
  (error "unimplemented"))

(defun pprint-logical-block (&rest args)
  (error "unimplemented"))

(defun pprint-newline (&rest args)
  (error "unimplemented"))

(defun pprint-pop (&rest args)
  (error "unimplemented"))

(defun pprint-tab (&rest args)
  (error "unimplemented"))

(defun set-pprint-dispatch (&rest args)
  (error "unimplemented"))

(defun pprint (&rest args)
  (error "unimplemented"))

(defun write (object &key (stream *standard-output*)
                       ((:array *print-array*) *print-array*)
                       ((:base *print-base*) *print-base*)
                       ((:case *print-case*) *print-case*)
                       ((:escape *print-escape*) *print-escape*)
                       ((:gensym *print-gensym*) *print-gensym*)
                       ((:length *print-length*) *print-length*)
                       ((:level *print-level*) *print-level*)
                       ((:lines *print-lines*) *print-lines*)
                       ((:miser-width *print-miser-width*) *print-miser-width*)
                       ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
                       ((:pretty *print-pretty*) *print-pretty*)
                       ((:radix *print-radix*) *print-radix*)
                       ((:readably *print-readably*) *print-readably*)
                       ((:right-margin *print-right-margin*) *print-right-margin*))
  (print-object object stream)
  object)

(defun prin1 (object &optional output-stream)
  (let ((*print-escape* t))
    (print-object object output-stream)
    object))

(defun print (object &optional output-stream)
  (let ((*print-escape* t))
    (write-char #\Newline output-stream)
    (print-object object output-stream)
    (write-char #\Space output-stream)
    object))

(defun princ (object &optional output-stream)
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (print-object object output-stream)
    object))

(defun write-to-string (object &rest args
                        &key array base case escape gensym length
                          level lines miser-width pprint-dispatch
                          pretty radix readably right-margin)
  (with-output-to-string (s)
    (apply #'write :stream s args)))

(defun prin1-to-string (object)
  (with-output-to-string (s)
    (let ((*print-escape* t))
      (print-object object s))))

(defun princ-to-string (object)
  (with-output-to-string (s)
    (let ((*print-escape* nil)
          (*print-readably* nil))
      (print-object object s))))

