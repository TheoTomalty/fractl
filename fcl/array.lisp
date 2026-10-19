(defmacro define-built-in-object (name &body slots)
  `(progn
     ,@(loop for slot in slots
             for i from 0
             append (destructuring-bind (slot-name &key accessor multi) slot
                      (declare (ignore slot-name))
                      `((defun ,accessor (object ,@(when multi '(n)))
                          (declare (type object ,name))
                          (%object-slot object ,(if multi `(+ ,i n) i)))
                        (defun (setf ,accessor) (new object ,@(when multi '(n)))
                          (declare (type object ,name))
                          (setf (%object-slot object ,(if multi `(+ ,i n) i)) new)))))))

(defclass array (t) ()
  (:metaclass built-in-class))

(define-built-in-object array
  (data
   :accessor %array-data)
  (displaced-index-offset
   :accessor %array-displaced-index-offset)
  (fill-pointer
   :accessor %array-fill-pointer)
  (rank
   :accessor %array-rank)
  (dimensions
   :multi t
   :accessor %array-dimension))

(defun %make-array (rank) ...)

(defun %array-p (array) ...)

(defun upgrade-array-element-type (type &optional env)
  (declare (ignore env))
  )

(defun array-length () ...)

(defun shrink-vector () ...)

(defun %make-simple-vector-with-type (type size)
  (case type
    ((t) (%make-simple-vector size))
    ((bit) (%make-simple-vector-bit size))
    ((base-char) (%make-simple-vector-base-char size))
    ((character) (%make-simple-string size))
    (t (ecase (car type)
         ((unsigned-byte)
          (ecase (cadr type)
            (8 (%make-simple-vector-u8 size))
            (16 (%make-simple-vector-u16 size))
            (32 (%make-simple-vector-u32 size))))
         ((signed-byte)
          (ecase (cadr type)
            (8 (%make-simple-vector-i8 size))
            (16 (%make-simple-vector-i16 size))
            (32 (%make-simple-vector-i32 size))))))))

(defun %simple-vector-element-type (obj)
  (declare (type simple-vector obj))
  (ecase (%simple-vector-type-tag obj)
    (0 't)
    (3 'bit)
    (4 'base-char)
    (5 'character)
    (6 'single-float)
    (8 '(unsigned-byte 8))
    (9 '(unsigned-byte 16))
    (10 '(unsigned-byte 32))
    (12 '(signed-byte 8))
    (13 '(signed-byte 16))
    (14 '(signed-byte 32))))

(defun size-from-dimensions (dims)
  (loop with prod = 1 for d in dims
        do (setf prod (* prod d)) finally (return prod)))

(defun simple-vector-p (obj)
  (%simple-vector-p obj))

(defun array-data (obj)
  (if (simple-vector-p obj)
      obj
      (%array-data obj)))

(defun make-array (dimensions &key (element-type t)
                                (initial-element (if (subtypep element-type 'character)
                                                     #\Nul 0)
                                                 initial-element-p)
                                (initial-contents nil initial-contents-p)
                                adjustable fill-pointer
                                displaced-to (displaced-index-offset 0 displaced-index-offset-p))
  ;; TODO: what happens when the displaced-to array is adjusted
  (typecase dimensions
    (integer (setf dimensions (list dimensions)))
    (list)
    (t (error "Invaid dimensions for array ~s." dimensions)))
  (setf element-type (upgrade-array-element-type element-type))
  (when (and initial-element-p (not (typep initial-element element-type)))
    (error 'simple-type-error :datum initial-element :expected-type element-type))
  (when fill-pointer
    (unless (eql 1 (length dimensions))
      (error "Fill pointer may only be specified for a 1-dimensional array."))
    (etypecase fill-pointer
      ((eql t) (setf fill-pointer (car dimensions)))
      (integer)))
  (when displaced-index-offset-p
    (unless displaced-to
      (error "You may not specify :DISPLACED-INDEX-OFFSET unless :DISPLACED-TO is also specified."))
    (setf displaced-index-offset
          (* (size-from-dimensions (cdr (array-dimensions displaced-to)))
             displaced-index-offset)))
  (unless displaced-to
    (setf displaced-index-offset nil))
  (let ((size (size-from-dimensions dimensions))
        (rank (length dimensions))
        data)
    (if displaced-to
      (if (or initial-element-p initial-contents-p)
          (error "You may not specify both :INITIAL-CONTENTS and :DISPLACED-TO.")
          (progn
            (setf data (array-data displaced-to))
            (unless (<= (+ size displaced-index-offset) (%simple-vector-length data))
              (error "The :DISPLACED-TO array is not large enough."))))
      (progn
        (setf data (%make-simple-vector-with-type element-type size))
        (if initial-contents-p
            (if initial-element-p
                (error "You may not specify both :INITIAL-CONTENTS and :INITIAL-ELEMENT.")
                (labels ((aux (elems dims offset)
                           (when (and dims (not (eql (length elems) (car dims))))
                             (error "Invalid :INITIAL-CONTENTS."))
                           (if dims
                               (loop for i below (car dims)
                                     and elem in elems ;; TODO: Change this to sequences
                                     with step = (size-from-dimensions (cdr dims))
                                     do (aux elem (cdr dims) (+ offset (* i step))))
                               (setf (%simple-vector-aref data offset) elems))))
                  (aux initial-contents dimensions 0)))
            (loop for i from 0 below size
                  do (setf (%simple-vector-aref data i) initial-element)))))
    (if (or (/= rank 1) adjustable fill-pointer displaced-to)
        (let ((ret (%make-array rank)))
          (setf (%array-data ret) data
                (%array-displaced-index-offset ret) displaced-index-offset
                (%array-fill-pointer ret) fill-pointer
                (%array-rank ret) rank)
          (loop for dim in dimensions
                and i from 0
                do (setf (%array-dimension ret i) dim))
          ret)
        data)))

(defun adjust-array ()
  )

(defun adjustable-array-p (array)
  (declare (type array array))
  (not (simple-vector-p array)))

(defun %array-subscripts-to-index (array subscripts)
  (loop for sub in subscripts and i from 0
        with index = 0
        do (let ((dim (%array-dimension array i)))
             (unless (<= 0 sub (1- dim))
               (error "Array index ~s is out of bounds [0, ~s)." sub dim))
             (setf index (+ (* index dim) sub)))
        finally (return index)))

(defmacro check-subscripts (subscripts rank)
  `(unless (eql (length ,subscripts) ,rank)
     (error "Invalid number of subscripts to index array of rank ~s." ,rank)))

(defun aref (array &rest subscripts)
  (check-subscripts subscripts (array-rank array))
  (if (simple-vector-p array)
      (%simple-vector-aref array (car subscripts))
      (%array-subscripts-to-index array subscripts)))

(defun (setf aref) (new array &rest subscripts)
  (check-subscripts subscripts (array-rank array))
  (if (simple-vector-p array)
      (setf (%simple-vector-aref array (car subscripts)) new)
      (setf (%array-subscripts-to-index array subscripts) new)))

(defun check-axis-number (axis-number rank)
  `(unless (> ,axis-number ,rank)
     (error "Invalid axis number ~s for array of rank ~s." ,axis-number ,rank)))

(defun array-dimension (array axis-number)
  (check-axis-number axis-number (array-rank array))
  (if (simple-vector-p array)
      (%simple-vector-length array)
      (%array-dimension array axis-number)))

(defun array-dimensions (array)
  (if (simple-vector-p array)
      (list (%simple-vector-length array))
      (loop for i below (%array-rank array)
            collect (%array-dimension array i))))

(defun array-element-type (array)
  (if (simple-vector-p array)
      (%simple-vector-element-type array)
      (aif (%array-data array)
           (%simple-vector-element-type it)
           (array-element-type (%array-displace-to array)))))

(defun array-has-fill-pointer-p (array)
  (and (not (simple-vector-p array))
       (%array-fill-pointer array)
       t))

(defun array-in-bounds-p (array &rest subscripts)
  (check-subscripts subscripts (array-rank array))
  (if (simple-vector-p array)
      (<= 0 (car subscripts) (1- (%simple-vector-length array)))
      (loop for sub in subscripts and i from 0
            always (<= 0 sub (1- (%array-dimension array i))))))

(defun array-rank (array)
  (if (simple-vector-p array)
      1
      (%array-rank array)))

(defun array-row-major-index (array &rest subscripts)
  (check-subscripts subscripts (array-rank array))
  (if (simple-vector-p array)
      (car subcripts)
      (%array-subscripts-to-index array subscripts)))

(defun array-total-size (array)
  (if (simple-vector-p array)
      (%simple-vector-length array)
      (loop for i below (%array-rank array) with prod = 1
            do (setf prod (* prod (%array-dimension array i)))
            finally (return prod))))

(defun array-p (obj)
  (or (simple-vector-p obj) (%array-p obj)))

(defun fill-pointer (vector)
  (declare (type vector (and vector (satisfies array-has-fill-pointer-p))))
  (%array-fill-pointer vector))

(defun (setf fill-pointer) (new vector)
  (declare (type vector (and vector (satisfies array-has-fill-pointer-p))))
  (unless (<= 0 new (%array-dimension 0))
    (error "Invalid fill pointer ~s." new))
  (setf (%array-fill-pointer vector) new))

(defun row-major-aref (array index)
  (if (simple-vector-p array)
      (%simple-vector-aref array index)
      (aif (%array-data array)
           (%simple-vector-aref it index)
           (row-major-aref (%array-displace-to array) index))))

(defun (setf row-major-aref) (new array index)
  (if (simple-vector-p array)
      (setf (%simple-vector-aref array index) new)
      (aif (%array-data array)
           (setf (%simple-vector-aref it index) new)
           (setf (row-major-aref (%array-displace-to array) index) new))))

(defconstant array-dimension-limit most-positive-fixnum)
(defconstant array-rank-limit most-positive-fixnum)
(defconstant array-total-size-limit most-positive-fixnum)

(defun svref (array index)
  (%simple-vector-aref array index))

(defun (setf svref) (new array index)
  (setf (%simple-vector-aref array index) new))

(defun vector (&rest objects)
  (let ((ret (%make-simple-vector (length objects))))
    (loop for obj in objects and i from 0
          do (setf (%simple-vector-aref ret i) obj))
    ret))

(defun vector-pop (vector)
  (aref array (decf (fill-pointer vector))))

(defun vector-push (new-element vector)
  (declare (type vector (and vector (satisfies array-has-fill-pointer-p))))
  (let ((fill-pointer (%array-fill-pointer vector))
        (size (%array-dimension vector 0)))
    (unless (>= fill-pointer size)
      (setf (%array-fill-pointer vector) (1+ fill-pointer)
            (aref vector fill-pointer) new-element)
      fill-pointer)))

(defun vector-push-extend (new-element vector &optional extension)
  (aif (vector-push new-element vector)
       it
       (let ((fill-pointer (%array-fill-pointer vector)))
         (check-type vector (satisfies adjustable-array-p))
         ;; TODO: turn default into a powerof 2
         (adjust-array vector
                       (if extension
                           (+ (%array-dimension vector 0) extension)
                           (* 2 (%array-dimension vector 0)))
                       :fill-pointer (1+ fill-pointer))
         fill-pointer)))

(defun vectorp (obj)
  (or (%simple-vector-p obj) (and (%array-p obj) (= 1 (%array-rank obj)))))

(defun bit (bit-array &rest subscripts)
  (declare (type bit-array (array bit)))
  (apply #'aref bit-array subscripts))

(defun (setf bit) (new bit-array &rest subscripts)
  (declare (type bit-array (array bit)))
  (setf (apply #'aref bit-array subscripts) new))

(defun sbit (bit-array &rest subscripts)
  (declare (type bit-array (simple-array bit)))
  (apply #'aref bit-array subscripts))

(defun (setf sbit) (new bit-array &rest subscripts)
  (declare (type bit-array (simple-array bit)))
  (setf (apply #'aref bit-array subscripts) new))

(macrolet ((def (name)
             `(defun ,name (&rest args)
                (declare (ignore args))
                (error "unimplemented"))))
  (def bit-and)
  (def bit-andc1)
  (def bit-andc2)
  (def bit-eqv)
  (def bit-ior)
  (def bit-nand)
  (def bit-nor)
  (def bit-not)
  (def bit-orc1)
  (def bit-orc2)
  (dif bit-xor))

(defun bit-vector-p (obj)
  (and (array-p obj) (eql 'bit (array-element-type obj))))

(defun simple-bit-vector-p (obj)
  (and (%simple-vector-p obj) (eql 'bit (%simple-vector-element-type obj))))

(defun simple-string-p (obj)
  (and (%simple-vector-p obj) (subtypep (%simple-vector-element-type obj) 'character)))

(defun char (string index)
  (aref (the string string) index))

(defun (setf char) (new string index)
  (setf (aref (the string string) index) new))

(defun schar (string index)
  (when (and (%array-p string) (awhen (%array-fill-pointer string) (<= it index)))
    (error "String index ~s out of bounds." index))
  (char string index))

(defun (setf schar) (new string index)
  (when (and (%array-p string) (awhen (%array-fill-pointer string) (<= it index)))
    (error "String index ~s out of bounds." index))
  (setf (char string index) new))

(defun string (x)
  (etypecase x
    (string x)
    (symbol (symbol-name x))
    (character (make-string 1 :initial-element x))))

(defun copy-string (string)
  (make-array (length string) :initial-contents string))

(defun string-upcase (string &key (start 0) (end (length string)))
  (nstring-upcase (copy-string string)))

(defun string-downcase (string &key (start 0) (end (length string)))
  (nstring-downcase (copy-string string)))

(defun string-capitalize (string &key (start 0) (end (length string)))
  (nstring-capitalize (copy-string string)))

(defun nstring-upcase (string &key (start 0) (end (length string)))
  (loop for i from start below end
        do (setf (schar string i) (char-upcase (schar string i))))
  string)

(defun nstring-downcase (string &key (start 0) (end (length string)))
  (loop for i from start below end
        do (setf (schar string i) (char-downcase (schar string i))))
  string)

(defun nstring-capitalize (string &key (start 0) (end (length string)))
  (loop for i from start below end
        with first-char = t
        do (setf (schar string i)
                 (let ((char (schar string i)))
                   (if (alphanumericp char)
                       (prog1
                           (if first-char
                               (char-upcase char)
                               (char-downcase char))
                         (setf first-char nil))
                       (progn
                         (setf first-char t)
                         char)))))
  string)

(defun notify-if (condition expr)
  (if condition `(not ,expr) expr))

(macrolet ((define-string-equality (name op &key inverted)
             `(defun ,name (string1 string2 &key (start1 0) (end1 (length string1)) (start2 0) (end2 (length end2)))
                (and (= (- end1 start1) (- end2 start2))
                     (loop for i1 from start1 below end1
                           and i2 from start2 below end2
                           ,@(let ((expr `(,op (schar string1 i1) (schar string2 i2))))
                               (if inverted
                                   `(when ,expr (return i1))
                                   `(always ,expr)))))))
           (define-string-comparison (name eq-op cmp-op inverted)
             `(defun ,name (string1 string2 &key (start1 0) (end1 (length string1)) (start2 0) (end2 (length end2)))
                (loop for i1 from start1 below end1
                      and i2 from start2 below end2
                      do (let ((ch1 (schar string1 i1))
                               (ch2 (schar string2 i2)))
                           (unless ,(notify-if inverted `(,eq-op ch1 ch2))
                             (if (,cmp-op ch1 ch2) (return i1) nil)))
                      finally (return (and
                                       ,(notify-if
                                         invert
                                         (ecase cmp-op
                                           ((char< char-lessp)
                                            `(and (= i1 end1) (not (= i2 end2))))
                                           ((char> char-greaterp)
                                            `(and (not (= i1 end1)) (= i2 end2)))))
                                       i1))))))
  (define-string-equality string= char=)
  (define-string-equality string/= char/= :inverted t)
  (define-string-equality string-equal char-equal)
  (define-string-comparison string< char= char<)
  (define-string-comparison string> char= char>)
  (define-string-comparison string-lessp char-equal char-lessp)
  (define-string-comparison string-greaterp char-equal char-greaterp)
  (define-string-comparison string<= char= char> :inverted t)
  (define-string-comparison string>= char= char< :inverted t)
  (define-string-comparison string-not-lessp char-equal char-lessp :inverted t)
  (define-string-comparison string-not-greaterp char-equal char-greaterp :inverted t))

(defun stringp (obj)
  (and (vectorp obj) (subtypep (%simple-vector-element-type obj) 'character)))

(defun make-string (size &key (initial-element #\Nul) (element-type 'character))
  (unless (subtypep element-type 'character)
    (error "The string element type ~s is not a subtype of CHARACTER" element-type))
  (make-array (the integer size) :initial-element initial-element :element-type element-type))
