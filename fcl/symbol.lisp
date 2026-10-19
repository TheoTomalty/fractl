(define-condition unbound-variable (cell-error) ())

(defun copy-symbol (symbol &optional copy-properties)
  (declare (type symbol symbol))
  (let ((ret (make-symbol (symbol-name symbol))))
    (when copy-properties
      (setf (symbol-value ret) (symbol-value symbol)
            (symbol-function ret) (symbol-function symbol)
            (symbol-plist ret) (copy-list (symbol-plist symbol))))
    ret))

(defvar *gensym-counter* 0)

(defun gensym (&optional (x nil x-p))
  (make-symbol
   (if x-p
       (etypecase x
         (string
          (format nil "~a~d" x (incf *gensym-counter*)))
         ((integer 0)
          (format nil "G~d" x)))
       (format nil "G~d" (incf *gensym-counter*)))))

(defun gentemp (&optional prefix package)
  (declare (ignore prefix package))
  (error "deprecated"))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (setf get) (new symbol indicator &optional default)
  (declare (ignore default))
  (setf (getf (symbol-plist symbol) indicator) new))

(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

(defun set (symbol value)
  (setf (symbol-value symbol) value))

(defun conc-sym (&rest symbols)
  (intern
   (apply #'concatenate 'string
          (mapcar (lambda (sym)
                    (if (stringp sym) sym (symbol-name sym)))
                  symbols))))
