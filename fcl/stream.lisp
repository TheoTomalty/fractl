(defstruct (stream (:conc-name %stream-)
                   (:predicate streamp))
  (read-char #'read-char-error)
  (read-byte #'read-byte-error)
  (write-char #'write-char-error)
  (write-byte #'write-byte-error)
  (next-char nil)
  (last-char nil)
  (column 0 :type (integer 0 *))
  (interactive-p nil :type boolean)
  (open-p t :type boolean))

(defun stream-column (stream)
  (%stream-column stream))

(deftype input-stream () `(satisfies input-stream-p))
(deftype output-stream () `(satisfies output-stream-p))

(defstruct (broadcast-stream
            (:constructor make-broadcast-stream (&rest streams))
            (:include stream
             (write-char #'broadcast-write-char)
             (write-byte #'broadcast-write-byte)))
  (streams nil :type list))

(defstruct (concatenated-stream
            (:constructor %make-concatenated-stream (streams))
            (:include stream
             (read-char #'concatenated-read-char)
             (read-byte #'concatenated-read-byte)))
  (streams nil :type list))

(defun make-concatenated-stream (&rest streams)
  (loop for stream in streams
        unless (input-stream-p stream)
          do (error "~s is not an input stream." stream))
  (%make-concatenated-stream streams))

(defstruct (echo-stream
            (:constructor make-echo-stream (input-stream output-stream))
            (:include stream
             (read-char #'echo-read-char)
             (read-byte #'echo-read-byte)
             (write-char #'echo-write-char)
             (write-byte #'echo-write-byte)))
  (input-stream nil :type input-stream)
  (output-stream nil :type output-stream))

(defstruct (fd-stream
            (:constructor %make-fd-stream)
            (:include stream))
  (fd nil :type fixnum))

(defun make-fd-stream (fd &key direction)
  (ecase direction
    (:output
     (%make-fd-stream :fd fd :write-char #'file-write-char))
    (:input
     (%make-fd-stream :fd fd :read-char #'file-read-char))
    (:io
     (%make-fd-stream :fd fd :write-char #'file-write-char :read-char #'file-read-char))
    (:probe
     (%make-fd-stream :fd fd))))

(defstruct (string-stream
            (:constructor %make-string-stream)
            (:include stream))
  (string nil :type string)
  (offset 0 :type fixnum))

(defstruct (synonym-stream
            (:constructor make-synonym-stream (symbol))
            (:include stream))
  (symbol nil :type proper-symbol))

(defstruct (two-way-stream
            (:constructor make-two-way-stream (input-stream output-stream))
            (:include stream))
  (input-stream nil :type input-stream)
  (output-stream nil :type output-stream))

(define-condition stream-error (error)
  ((stream
    :initarg :stream
    :reader stream-error-stream)))
(define-condition simple-stream-error (stream-error simple-condition) ())

(define-condition end-of-file (stream-error) ())

(define-constant eof (make-symbol "EOF"))

(defun read-char-error (stream)
  (error "Stream ~s is not a character input stream." stream))

(defun read-byte-error (stream)
  (error "Stream ~s is not a binary input stream." stream))

(defun write-char-error (stream char)
  (declare (ignore char))
  (error "Stream ~s is not a character output stream." stream))

(defun write-byte-error (stream byte)
  (declare (ignore byte))
  (error "Stream ~s is not a binary output stream." stream))

(defun input-stream-p (stream)
  (or (not (eql (%stream-read-char stream) #'read-error))
      (not (eql (%stream-read-byte stream) #'read-error))))

(defun output-stream-p (stream)
  (or (not (eql (%stream-write-char stream) #'write-error))
      (not (eql (%stream-write-byte stream) #'write-error))))

(defun interactive-stream-p (stream)
  (%stream-interactive-p stream))

(defun open-stream-p (stream)
  (%stream-open-p stream))

(defun stream-element-type (stream)
  (error "unimplemented"))

(defmacro do-broadcast ((stream broadcast-stream) &body body)
  (let ((thunk (make-symbol "THUNK"))
        (bs (make-symbol "BROADCAST-STREAM")))
    `(flet ((,thunk (,stream)
              ,@body))
       (let ((,bs ,broadcast-stream))
         (loop for ,stream in (butlast ,bs)
               do (,thunk ,stream))
         (awhen (last ,bs)
           (,thunk (car it)))))))

(defun broadcast-write-char (stream char)
  (do-broadcast (stream stream)
    (write-char char stream)))

(defun broadcast-write-byte (stream byte)
  (do-broadcast (stream stream)
    (write-byte byte stream)))

(macrolet ((def (name read)
             `(defun ,name (stream)
                (let ((streams (concatenated-stream-streams stream)))
                  (if streams
                      (let ((ret (,read (car streams) nil eof)))
                        (if (eql ret eof)
                            (progn
                              (setf (concatenated-stream-streams stream) (cdr streams))
                              (concatenated-read-char stream))
                            ret))
                      eof)))))
  (def concatenated-read-char read-char)
  (def concatenated-read-byte read-byte))

(macrolet ((def-read (name read write)
             `(defun ,name (stream)
                (let ((elem (,read (echo-stream-input-stream stream) nil eof)))
                  (unless (eql eof elem)
                    (,write elem (echo-stream-output-stream stream)))
                  elem)))
           (def-write (name write)
             `(defun ,name (stream elem)
                (,write elem (echo-stream-output-stream stream)))))
  (def-read echo-read-char read-char write-char)
  (def-read echo-read-byte read-byte write-byte)
  (def-write echo-write-char write-char)
  (def-write echo-write-byte write-byte))

(defun file-read-char (stream)
  (or (%read-char (file-stream-fd stream)) eof))

(defun file-write-char (stream ch)
  (%write-char (file-stream-fd stream) ch))

(defun string-read-char (stream)
  (let ((string (string-stream-string stream))
        (offset (string-stream-offset stream)))
    (if (< offset (length string))
        (prog1 (aref string offset)
          (setf (string-stream-offset stream) (1+ offset)))
        eof)))

(defun string-write-char (stream ch)
  (vector-push-extend ch (string-stream-string stream)))

(macrolet ((def-read (name read)
             `(defun ,name (stream)
                (,read (symbol-value (synonym-stream-symbol stream)) nil eof)))
           (def-write (name write)
             `(defun ,name (stream elem)
                (,write elem (symbol-value (synonym-stream-symbol stream))))))
  (def-read synonym-read-char read-char)
  (def-read synonym-read-byte read-byte)
  (def-write synonym-write-char write-char)
  (def-write synonym-write-byte write-byte))

(macrolet ((def-read (name read)
             `(defun ,name (stream)
                (,read (two-way-stream-input-stream stream) nil eof)))
           (def-write (name write)
             `(defun ,name (stream elem)
                (,write elem (two-way-stream-output-stream stream)))))
  (def-read two-way-read-char read-char)
  (def-read two-way-read-byte read-byte)
  (def-write two-way-write-char write-char)
  (def-write two-way-write-byte write-byte))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (or-eof (funcall (%stream-read-byte stream) stream)
          (if eof-error-p
              (error 'end-of-file :stream stream)
              eof-value)))

(defun write-byte (byte stream)
  (funcall (%stream-write-byte stream) stream byte)
  byte)

(defun peek-char (&optional peek-type (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (etypecase peek-type
    ((eql nil)
     (aif (%stream-next-char input-stream)
          it
          (setf (%stream-next-char input-stream) (read-char input-stream eof-error-p eof-value))))
    ((eql t)
     (loop for ch = (%stream-next-char input-stream)
             then (setf (%stream-next-char input-stream) (read-char input-stream eof-error-p eof-value))
           while (member ch whitespace-chars)
           finally (return ch)))
    (character
     (loop for ch = (%stream-next-char input-stream)
             then (setf (%stream-next-char input-stream) (read-char input-stream eof-error-p eof-value))
           until (char= ch peek-type)
           finally (return ch)))))

(defun read-char (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (aif (shiftf (%stream-next-char input-stream) nil)
       it
       (or-eof (setf (%stream-prev-char input-stream)
                     (funcall (%stream-read-byte stream) stream))
               (if eof-error-p
                   (error 'end-of-file :stream stream)
                   eof-value))))

(defun read-char-no-hang ()
  (error "unimplemented"))

(defun terpri (&optional (output-stream *standard-output*))
  (write-char #\Newline output-stream)
  nil)

(defun fresh-line (&optional (output-stream *standard-output*))
  (unless (eql #\Newline (%stream-prev-char output-stream))
    (write-char #\Newline output-stream)))

(defun unread-char (char &optional (input-stream *standard-input*))
  (check-type input-stream input-stream)
  (unless (eql (%stream-prev-char input-stream) char)
    (error "The character ~s was not the last character read from the stream." char))
  (setf (%stream-next-char input-stream) char))

(defun write-char (char &optional (output-stream *standard-output*))
  (funcall (%stream-write-char output-stream) stream char)
  (setf (%stream-prev-char output-stream) char)
  (setf (%stream-column output-stream) (if (eql #\Newline char)
                                           0
                                           (%stream-column output-stream)))
  char)

(defun read-line (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (loop for ch = (read input-stream nil eof)
        until (or (eql #\Newline ch) (eql eof ch))
        collect ch into chars
        finally (return
                  (if (eql eof ch)
                      (if eof-error-p
                          (error 'end-of-file :stream input-stream)
                          (values eof-value t))
                      (values (concatenate 'string chars) nil)))))

(defun write-string (string &optional (output-stream *standard-output*) &key (start 0) end)
  (setf end (or end (length string)))
  (loop for i from start below end
        do (write-char ch output-stream))
  string)

(defun write-line (string &optional (output-stream *standard-output*) &key (start 0) end)
  (write-string string output-stream :start start :end end)
  (terpri output-stream))

(defun read-char-or-byte (stream &optional (eof-error-p t) eof-value)
  (let ((element-type (stream-element-type stream)))
    (cond ((subtypep element-type 'character)
           (read-char stream eof-error-p eof-value))
          ((subtypep element-type 'integer)
           (read-byte stream eof-error-p eof-value))
          (t (error "Invalid element-type ~s for stream ~s." element-type stream)))))

(defun write-char-or-byte (elem stream)
  (etypecase elem
    (character
     (write-char elem stream))
    (integer
     (write-byte elem stream))))

(defun read-sequence (sequence stream &key (start 0) end)
  (setf end (or end (length sequence)))
  (let ((element-type (stream-element-type stream)))
    (seq-case sequence
      (loop for seq on sequence and i below end
            when (<= start i)
              do (aif (read-char-or-byte stream nil eof)
                      (setf (car seq) it)
                      (return i)))
      (loop for i from start below end
            do (aif (read-char-or-byte stream nil eof)
                    (setf (aref sequence i) it)
                    (return i))))))

(defun write-sequence (sequence stream &key (start 0) end)
  (setf end (or end (length sequence)))
  (seq-case sequence
    (loop for elem in sequence and i below end
            when (<= start i)
              do (write-char-or-byte elem stream))
    (loop for i from start below end
          do (write-char-or-byte (aref sequence i) stream)))
  sequence)

(defun file-length (stream)
  (error "unimplemented"))

(defun file-position (stream)
  (error "unimplemented"))

(defun file-string-length (stream)
  (error "unimplemented"))

(defun open ...)
(defun close ...)
(defun stream-external-format ...)

(defmacro with-open-file ...)
(defmacro with-open-stream ...)

(defun listen (&optional (input-stream *standard-input*))
  (and (%stream-next-char input-stream) t))
(defun clear-input (&optional (input-stream *standard-input*))
  (setf (%stream-next-char input-stream) nil))

(defun finish-output (&optional (output-stream *standard-output*))
  (declare (ignore output-stream))
  nil)
(defun force-output (&optional (output-stream *standard-output*))
  (declare (ignore output-stream))
  nil)
(defun clear-output (&optional (output-stream *standard-output*))
  (declare (ignore output-stream))
  nil)

(defun y-or-n-p (&optional (control "y or n? ") &rest args)
  (apply #'format *query-io* control args)
  (loop
    (let ((answer (read-line *query-io*)))
      (cond ((string-equal "y") (return-from y-or-n-p t))
            ((string-equal "n") (return-from y-or-n-p nil))
            (t (format *query-io* "Invalid answer ~s, please answer either \"y\" or \"n\": " answer))))))

(defun yes-or-no-p (&optional (control "yes or no? ") &rest args)
  (apply #'format *query-io* control args)
  (loop
    (let ((answer (read-line *query-io*)))
      (cond ((string-equal "yes") (return-from yes-or-no-p t))
            ((string-equal "no") (return-from yes-or-no-p nil))
            (t (format *query-io* "Invalid answer ~s, please answer either \"yes\" or \"no\": " answer))))))

(defun string-stream-base-string (element-type)
  (make-array 16 :element-type element-type :fill-pointer 0 :adjustable t))

(defun get-output-stream-string (stream)
  (check-type stream output-stream)
  (setf (string-stream-offset stream) 0)
  (let ((string (string-stream-string stream)))
    (setf (string-stream-string stream) (string-stream-base-string (array-element-type string)))))

(defun make-string-input-stream (string &optional (start 0) end)
  (setf end (or end (length string)))
  (%make-string-stream :string (subseq string start end) :read-char #'string-read-char))

(defun make-string-output-stream (&key (element-type 'character) (string (string-stream-base-string element-type)))
  (unless (subtype (array-element-type string) element-type)
    (error "Supplied string must have an element type of ~s." element-type))
  (unless (subtypep element-type 'character)
    (error "String stream element type must be a subtype of character but got ~s." element-type))
  (%make-string-stream :string string :write-char #'string-write-char))

(defmacro with-input-from-string ((var string &key (index nil index-p) (start 0 start-p) (end nil end-p)) &body body)
  `(let ((,var (make-string-input-stream ,string ,@(when (or start-p end-p) (list start)) ,@(when end-p (list end)))))
     (unwind-protect
          (prog1
              (progn
                ,@body)
            ,@(when index-p `((setf ,index (string-stream-offset ,var)))))
       (close ,var))))

(defmacro with-output-to-string ((var &optional (string-form nil) &key (element-type 'character element-type-p)) &body body)
  `(let ((,var (make-string-output-stream ,@(when string-form `(:string ,string-form))
                                          ,@(when element-type-p `(:element-type ,element-type)))))
     (unwind-protect
          (progn
            ,@body)
       (close ,var))))


(macrolet ((def (name val)
             (unless (boundp ',name)
               (setf ,name ,val))))
  
  (def *standard-input* (make-fd-stream 0 :direction :input))
  (def *standard-output* (make-fd-stream 1 :direction :output))
  (def *error-output* (make-fd-stream 2 :direction :output))

  (def *debug-io* (make-two-way-stream *standard-input* *standard-output*))
  (def *query-io* (make-two-way-stream *standard-input* *standard-output*))
  (def *trace-output* *standard-output*)
  (def *terminal-io* (make-two-way-stream *standard-input* *standard-output*)))
