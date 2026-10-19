(in-package "FED")


(defstruct (window-view (:constructor %make-window-view))
  (buffer    nil :type buffer)
  (cursor    nil)
  (view-mark nil :type mark)
  (view-vcol 0   :type (integer 0)))

(defclass window ()
  ((view
    :accessor window-view
    :type window-view)
   (views
    :initform nil
    :accessor window-views
    :type list)
   (dims
    :initarg :dims
    :accessor window-dims
    :type coord)
   (coord
    :initarg :coord
    :initform coord0
    :accessor window-coord
    :type coord)
   (frame
    :initarg :frame
    :reader window-frame
    :type frame)
   (jumps
    :initform (make-ring-buffer 20)
    :accessor window-jumps
    :type ring-buffer)
   (jump-ptr
    :initform 0
    :accessor window-jump-ptr
    :type non-negative-integer)))

(defun current-window ()
  (frame-focused-window (current-frame)))

(defun list-all-windows ()
  (loop for frame in (frames) nconc (copy-list (frame-windows frame))))

(defun make-window-view (window buffer)
  (let ((cursor (make-cursor buffer window)))
    (push cursor (buffer-cursors buffer))
    (%make-window-view :buffer buffer
                       :cursor cursor
                       :view-mark (make-mark-right-inserting
                                   (text-pos-point (buffer-text buffer) 0)))))

(defun window-view-window (view)
  (cursor-window (window-view-cursor view)))

(defun delete-window-view (view)
  (setf (buffer-cursors (window-view-buffer view))
        (delete (window-view-cursor view) (buffer-cursors (window-view-buffer view))))
  (delete-mark (window-view-view-mark view))
  (delete-cursor (window-view-cursor view)))

(defun create-window (frame dims &key (buffer (empty-buffer)) (coord coord0))
  (let ((ret (make-instance 'window :dims dims :frame frame :coord coord)))
    (setf (window-view ret) (make-window-view ret buffer))
    ret))

(defun push-buffer (buffer window)
  (cond ((eql buffer (window-buffer window))
         (values buffer (window-cursor window)))
        ((buffer-locked-p (window-buffer window))
         (editor-error "Can not switch locked buffer"))
        (t
         (let ((view (or (find buffer (window-views window) :key #'window-view-buffer)
                          (make-window-view window buffer))))
           (setf (window-views window) (delete view (window-views window)))
           (if (eql (empty-buffer) (window-buffer window))
               (delete-window-view (window-view window))
               (push (window-view window) (window-views window)))
           (setf (window-view window) view)
           (values buffer (window-view-cursor view))))))

(defun delete-mark-from-pair (pair)
  (delete-mark (cdr pair)))

(defun window-delete-buffer (window buffer)
  (if (eql buffer (window-buffer window))
      (pop-buffer window)
      (awhen (find buffer (window-views window) :key #'window-view-buffer)
        (delete-window-view it)
        (setf (window-views window) (delete buffer (window-views window) :key #'window-view-buffer))))
  (let* ((jumps (window-jumps window))
         (length (ring-buffer-length jumps))
         (ptr (window-jump-ptr window))
         (new (make-ring-buffer (ring-buffer-size jumps))))
    (loop for i below length
          do (let ((entry (ring-buffer-aref jumps i)))
               (if (eql buffer (car entry))
                   (progn
                     (when (<= (- length ptr) i)
                       (decf (window-jump-ptr window)))
                     (delete-mark-from-pair entry))
                   (ring-buffer-push entry new))))
    (setf (window-jumps window) new))
  buffer)

(defun pop-buffer (window)
  (let ((buffer (window-buffer window)))
    (delete-window-view (window-view window))
    (setf (window-view window)
          (or (pop (window-views window))
              (make-window-view window (empty-buffer))))
    buffer))

(defun find-file (filename)
  (let ((window (full-frame-selected-window (full-frame))))
    (multiple-value-prog1
        (aif (find (nth-value 1 (filename-buffer-name filename)) (buffers) :key #'buffer-file)
             (push-buffer it window)
             (create-file-buffer filename window))
      (switch-to-window window))))

(defun switch-to-window (window)
  (frame-focus-window (switch-to-frame (window-frame window)) window))

(defun window-set-jump (window)
  "Adds the current cursor position and buffer of the WINDOW to the jumps list,
unless the previous jump is on the same line.  Any jumps after the JUMP-PTR are
deleted."
  (let ((jumps (window-jumps window))
        (point (cursor-point (window-cursor window))))
    (setf (cursor-named-mark (window-cursor window) #\`) (make-mark-left-inserting point))
    (loop repeat (window-jump-ptr window) do (delete-mark-from-pair (ring-buffer-pop jumps)))
    (setf (window-jump-ptr window) 0)
    (unless (and (< 0 (ring-buffer-length jumps))
                 (= (point-line point) (point-line (cdr (ring-buffer-aref jumps (1- (ring-buffer-length jumps)))))))
      (ring-buffer-push (cons (window-buffer window) (make-mark-left-inserting point)) jumps :delete #'delete-mark-from-pair))))

(defun window-switch-to-jump (window &optional (ptr (window-jump-ptr window)))
  "Move the current cursor of WINDOW to the jump location indicated by PTR
(counting from end of the jump list)."
  (declare (type positive-integer ptr))
  (let ((jumps (window-jumps window)))
    (destructuring-bind (buffer . mark) (ring-buffer-aref jumps (- (ring-buffer-length jumps) ptr))
      (push-buffer buffer window)
      (setf (cursor-point (window-cursor window)) mark
            (cursor-snapcol (window-cursor window)) nil))))

(defun window-backward-jumps (window &optional (n 1))
  "Decrement the jump index indicated by the WINDOW jump-ptr by N.  If the index
can be decremented, move the current cursor to that location."
  (declare (type positive-integer n))
  (let ((jumps (window-jumps window)))
    (when (= 0 (window-jump-ptr window))
      (window-set-jump window)
      (setf (window-jump-ptr window) 1))
    (let ((prev-ptr (window-jump-ptr window)))
      (setf (window-jump-ptr window) (min (ring-buffer-length jumps) (+ prev-ptr n)))
      (when (and (= n 1) (= (ring-buffer-length jumps) prev-ptr))
        (editor-error "Beginning of jump list"))
      (when (< prev-ptr (window-jump-ptr window))
        (window-switch-to-jump window)))))

(defun window-forward-jumps (window &optional (n 1))
  "Increment the jump index indicated by the WINDOW jump-ptr by N.  Set the current
cursor to the largest jump index passed through during this process (not
including the current jump index)."
  (declare (type positive-integer n))
  (let ((prev-ptr (window-jump-ptr window)))
    (setf (window-jump-ptr window) (max 0 (- prev-ptr n)))
    (when (and (= n 1) (<= prev-ptr 1))
      (editor-error "End of jump list"))
    (when (and (< (window-jump-ptr window) prev-ptr)
               (< 1 prev-ptr))
      (window-switch-to-jump window (max (window-jump-ptr window) 1)))))

(defun window-buffer (window)
  (window-view-buffer (window-view window)))

(defun window-cursor (window)
  (window-view-cursor (window-view window)))

(defun window-view-point (window)
  (copy-point (window-view-view-mark (window-view window))))

(defun (setf window-view-point) (new window)
  (assert (eql (point-text new) (buffer-text (window-buffer window))))
  (delete-mark
   (shiftf (window-view-view-mark (window-view window))
           (make-mark-right-inserting new)))
  new)

(defun window-view-vcoord (window)
  (point-vcoord (window-view-point window)))

(defun (setf window-view-vcoord) (new window)
  (setf (window-view-point window) (text-vcoord-point (buffer-text (window-buffer window)) new))
  new)

(defun window-view-vcol (window)
  (let ((view (window-view window)))
    (if (buffer-variable (window-view-buffer view) 'wrap)
        0
        (window-view-view-vcol view))))

(defun (setf window-view-vcol) (new window)
  (setf (window-view-view-vcol (window-view window)) new))

(defun window-width (window)
  (coord-x (window-dims window)))

(defun window-height (window)
  (coord-y (window-dims window)))

(defun window-buffers (window)
  (list* (window-buffer window) (mapcar #'window-view-buffer (window-views window))))

(defun window-wrapcol (window)
  (when (buffer-variable (window-buffer window) 'wrap)
    (window-view-width window)))

(defun window-view-width (window)
  (max 0 (- (coord-x (window-dims window)) (window-margin-width window)
            (if (buffer-variable (window-buffer window) 'wrap) 1 0))))

(defun decimal-width (n)
  (declare (type (integer 0) n))
  (max 1 (loop until (= 0 n) count (setf n (truncate n 10)))))

(defun window-margin-width (window)
  (let ((buffer (window-buffer window)))
    (if (buffer-variable buffer 'number)
        (+ 2 (decimal-width (buffer-nlines buffer)))
        0)))

(defun %do-wlines-up (window point function)
  (assert (buffer-variable (window-buffer window) 'wrap))
  (let* ((buffer (window-buffer window))
         (width (window-view-width window))
         (vcoord (point-vcoord point))
         (v (coord (* (floor (coord-x vcoord) width) width) (coord-y vcoord)))
         (vcol-end nil))
    (loop do (funcall function v)
             (if (<= (coord-x v) 0)
                 (if (<= (coord-y v) 0)
                     (return)
                     (setf vcol-end (buffer-line-max-vcol buffer (- (coord-y v) 1))
                           v (coord (* (truncate vcol-end width) width) (- (coord-y v) 1))))
                 (setf v (coord- v (coord width 0)))))))

(defun %do-wlines-down (window point function)
  (assert (buffer-variable (window-buffer window) 'wrap))
  (let* ((buffer (window-buffer window))
         (width (window-view-width window))
         (max-lines (1- (buffer-nlines buffer)))
         (vcoord (point-vcoord point))
         (v (coord (* (floor (coord-x vcoord) width) width) (coord-y vcoord)))
         (vcol-end (buffer-line-max-vcol buffer (coord-y v))))
    (loop do (funcall function v)
             (if (< vcol-end (+ (coord-x v) width))
                 (if (<= max-lines (coord-y v))
                     (return)
                     (setf v (coord 0 (+ (coord-y v) 1))
                           vcol-end (buffer-line-max-vcol buffer (coord-y v))))
                 (setf v (coord+ v (coord width 0)))))))

(defmacro do-wlines-down ((var window point &optional result) &body body)
  `(block nil
     (%do-wlines-down ,window ,point (lambda (,var) ,@body))
     ,result))

(defmacro do-wlines-up ((var window point  &optional result) &body body)
  `(block nil
     (%do-wlines-up ,window ,point (lambda (,var) ,@body))
     ,result))

(defun window-render-margin (window render-buffer to line cursor-line &optional (display-num t))
  (let ((number-type (buffer-variable (window-buffer window) 'number)))
    (when number-type
      (let* ((rich-string
               (load-time-value (list (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
                                      :background :default :foreground :bright-black)))
             (strbuf (first rich-string))
             (width (window-margin-width window)))
        (setf (fill-pointer strbuf) 0)
        (if (eql line cursor-line)
            (setf (third rich-string) :bright-black
                  (fifth rich-string) :default)
            (setf (third rich-string) :default
                  (fifth rich-string) :bright-black))
        (if (and display-num (<= 0 line (1- (buffer-nlines (window-buffer window)))))
            (format strbuf " ~vd " (- width 2)
                    (ecase number-type
                      (:relative (if (= cursor-line line) (1+ line) (abs (- cursor-line line))))
                      (:absolute (1+ line))))
            (loop repeat width do (vector-push-extend #\Space strbuf)))
        (rich-string-render rich-string render-buffer to width)))))

(defun window-render (window render-buffer)
  (let ((buffer (window-buffer window))
        (height (window-height window))
        (to (window-coord window))
        (to+margin (coord+ (window-coord window)
                           (coord (window-margin-width window) 0)))
        (width (window-view-width window))
        (cursor-line (cursor-line (window-cursor window))))
    (if (buffer-variable buffer 'wrap)
        (let ((i 0))
          (do-wlines-down (v window (window-view-point window))
            (when (<= height i)
              (return))
            (window-render-margin window render-buffer
                                  (coord+ to (coord 0 i)) (coord-y v) cursor-line (= 0 (coord-x v)))
            (buffer-render-line buffer render-buffer
                                (coord+ to+margin (coord 0 i)) (coord-y v) (coord-x v) (+ (coord-x v) width))
            (incf i)))
        (let ((y-offset (point-line (window-view-point window)))
              (x-offset (window-view-vcol window))
              (nlines (buffer-nlines buffer)))
          (loop for i below height and line from y-offset below nlines
                do (window-render-margin window render-buffer
                                         (coord+ to (coord 0 i)) line cursor-line)
                   (buffer-render-line buffer render-buffer
                                       (coord+ to+margin (coord 0 i)) line x-offset (+ x-offset width)))))))

(defun window-point-view-coord (window point)
  (if (buffer-variable (window-buffer window) 'wrap)
      (let ((height (window-height window))
            (width (window-view-width window))
            (vcoord (point-vcoord point))
            (i 0))
        (do-wlines-down (v window (window-view-point window))
          (when (<= height i)
            (return))
          (when (and (= (coord-y v) (coord-y vcoord))
                     (<= (coord-x v) (coord-x vcoord) (1- (+ (coord-x v) width))))
            (return (coord (mod (coord-x vcoord) width) i)))
          (incf i)))
      (let ((vcoord (point-vcoord point))
            (x-min (window-view-vcol window))
            (y-min (point-line (window-view-point window))))
        (when (and (<= x-min (coord-x vcoord) (1- (+ x-min (window-view-width window))))
                   (<= y-min (coord-y vcoord) (1- (+ y-min (window-height window)))))
          (coord (- (coord-x vcoord) x-min)
                 (- (coord-y vcoord) y-min))))))

(defun window-point-gcoord (window point)
  (awhen (window-point-view-coord window point)
    (coord+ (window-coord window)
            (coord+ (coord (window-margin-width window) 0) it))))

(defun window-scroll-up (window n &key (cursor-lock 1) restrict-end)
  (window-scroll-down window (- n) :cursor-lock cursor-lock :restrict-end restrict-end))

(defun cursor-wline (cursor)
  (awhen (window-point-view-coord (cursor-window cursor) (cursor-point cursor))
    (coord-y it)))

(defun (setf cursor-wline) (new cursor)
  (let ((window (cursor-window cursor)))
    (if (buffer-variable (window-buffer window) 'wrap)
        (let ((i 0)
              (ret nil))
          (setf (window-view-vcoord window)
                (do-wlines-up (v window (cursor-point cursor) ret)
                  (when (<= new i)
                    (return v))
                  (setf ret v)
                  (incf i))))
        (setf (window-view-vcoord window) (coord 0 (max 0 (- (cursor-line cursor) new)))))))

(defun window-buffer-wnlines (window)
  (let ((buffer (window-buffer window)))
    (if (buffer-variable buffer 'wrap)
        (let ((i 0))
          (do-wlines-down (v window (text-pos-point (buffer-text buffer) 0))
            (declare (ignore v))
            (incf i))
          i)
        (buffer-nlines buffer))))

(defun window-scroll-down (window n &key (cursor-lock 1) restrict-end)
  (when (/= 0 n)
    (let* ((buffer (window-buffer window))
           (cursor (window-cursor window))
           (height (window-height window))
           (wline (cursor-wline cursor)))
      (assert wline)
      (labels ((top (bot) (1- (+ bot height)))
               (bot (top) (1+ (- top height)))
               (next-c_n (w) (+ (* (ceiling (- w wline) cursor-lock) cursor-lock) wline))
               (prev-c_n (w) (+ (* (floor (- w wline) cursor-lock) cursor-lock) wline))
               (within-cursor-range (s w-end)
                 (let ((c_n (next-c_n s)))
                   (and (<= c_n w-end)
                        (<= (bot c_n) s c_n))))
               (scroll-distance (n w-end)
                 (let* ((s-max (max 0 (min n (if restrict-end (bot w-end) w-end))))
                        (s-min (max 0 (min s-max (bot w-end)))))
                   (block nil
                     (when (/= s-max s-min)
                       (when (and (= n s-max) (within-cursor-range s-max w-end))
                         (return s-max))
                       (let ((new-s-max (min (prev-c_n w-end) s-max))
                             (s nil))
                         (when (and (<= s-min new-s-max) (<= s-min (setf s (* (truncate new-s-max cursor-lock) cursor-lock))))
                           (return s))))
                     (when (within-cursor-range s-min w-end)
                       (return s-min))
                     (max 0 (prev-c_n (1- s-min))))))
               (scroll-params (n w-end)
                 (let* ((s (ecase (sign n)
                             (1 (scroll-distance n w-end))
                             (-1 (- (scroll-distance (- n) (- (1- height) w-end))))))
                        (w-c (ecase (sign n)
                               (1 (max (next-c_n s) wline))
                               (-1 (min (prev-c_n (top s)) wline)))))
                   (values s w-c))))
        (if (buffer-variable buffer 'wrap)
            (let ((width (window-view-width window)))
              (let ((w wline)
                    (v-buffer (make-array height :initial-element nil))
                    (i 0))
                (ecase (sign n)
                  (1 ;; Down
                   (do-wlines-down (v window (cursor-point cursor))
                     (unless (<= w (top n))
                       (return))
                     (when (= 0 (mod (- w wline) cursor-lock))
                       (setf (svref v-buffer (mod i height)) v)
                       (incf i))
                     (incf w))
                   (decf w) (decf i))
                  (-1 ;; Up
                   (do-wlines-up (v window (cursor-point cursor))
                     (unless (<= n w)
                       (return))
                     (when (= 0 (mod (- w wline) cursor-lock))
                       (setf (svref v-buffer (mod i height)) v)
                       (decf i))
                     (decf w))
                   (incf w) (incf i)))
                (multiple-value-bind (s w-c) (scroll-params n w)
                  (let ((n-c (floor (- w-c wline) cursor-lock)))
                    (assert (< (abs (- n-c i)) height))
                    (psetf (cursor-vcoord cursor) (coord+ (coord (mod (cursor-snapcol cursor) width) 0)
                                                          (aref v-buffer (mod n-c height)))
                           (cursor-snapcol cursor) (cursor-snapcol cursor))
                    (setf (cursor-wline cursor) (- w-c s))))))
            (let* ((line (cursor-line cursor))
                   (w-start (- (point-line (window-view-point window))))
                   (w-end (+ w-start (1- (buffer-nlines buffer))))
                   (wline (+ w-start line)))
              (multiple-value-bind (s w-c) (scroll-params n (ecase (sign n) (1 w-end) (-1 w-start)))
                (psetf (cursor-vcoord cursor) (coord (cursor-snapcol cursor) (+ (- w-c wline) line))
                       (cursor-snapcol cursor) (cursor-snapcol cursor))
                (setf (cursor-wline cursor) (- w-c s)))))))))

(defun window-scroll-left (window n)
  (unless (buffer-variable (window-buffer window) 'wrap)
    (let* ((cursor (window-cursor window))
           (line (cursor-line cursor))
           (width (window-view-width window))
           (vcol (coord-x (cursor-vcoord cursor)))
           (vcol-start (max (- (window-view-vcol window) n) 0))
           (vcol-end (+ vcol-start (1- width))))
      (when (< vcol-end vcol)
        (setf (cursor-vcoord cursor) (coord vcol-end line)
              (cursor-snapcol cursor) nil)
        (setf vcol (coord-x (cursor-vcoord cursor)))
        (when (< vcol vcol-start)
          (setf vcol-start vcol vcol-end (+ vcol-start (1- width)))))
      (setf (window-view-vcol window) vcol-start))))

(defun window-scroll-right (window n)
  (unless (buffer-variable (window-buffer window) 'wrap)
    (let* ((cursor (window-cursor window))
           (line (cursor-line cursor))
           (width (window-view-width window))
           (max-vcol (let ((point (text-vcoord-point (buffer-text (cursor-buffer cursor)) (coord -1 line))))
                       (if (point-bol-p point) 0 (coord-x (point-vcoord (point-skip-prev point))))))
           (vcol (coord-x (cursor-vcoord cursor)))
           (vcol-start (min (+ (window-view-vcol window) n) max-vcol))
           (vcol-end (+ vcol-start (1- width))))
      (when (< vcol vcol-start)
        (setf (cursor-vcoord-forward cursor) (coord vcol-start line)
              (cursor-snapcol cursor) nil)
        (setf vcol (coord-x (cursor-vcoord cursor)))
        (when (< vcol-end vcol)
          (setf vcol-end vcol vcol-start (- vcol-end (1- width)))))
      (setf (window-view-vcol window) vcol-start))))

(defun window-scroll-halign (window &optional (direction 0))
  (unless (buffer-variable (window-buffer window) 'wrap)
    (setf (window-view-vcol window)
          (let ((width (window-view-width window))
                (vcol (coord-x (cursor-vcoord (window-cursor window)))))
            (ecase direction
              (-1 (max (- (1+ vcol) width) 0))
              (0 (max (- vcol (floor width 2)) 0))
              (1 vcol))))))

(defun window-recentre (window)
  (unless (or (buffer-variable (window-buffer window) 'wrap)
              (<= (window-view-vcol window)
                  (coord-x (cursor-vcoord (window-cursor window)))
                  (1- (+ (window-view-vcol window) (window-view-width window)))))
    (window-scroll-halign window))
  (unless (window-point-view-coord window (cursor-point (window-cursor window)))
    (setf (cursor-wline (window-cursor window)) (truncate (window-height window) 2))))

(define-editor-command scroll-down-line
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (window-scroll-down (cursor-window cursor) n))

(define-editor-command scroll-up-line
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (window-scroll-up (cursor-window cursor) n :restrict-end t))

(define-editor-command scroll-down-half
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  ;; COMPAT: the cursor is sticky so that scrolling down and then back up the same number of times does not change position
  ;; COMPAT: the numeric argument mulitplies the scroll by n half-pages, not n lines.
  (let ((base (1+ (truncate (window-height (cursor-window cursor)) 2))))
    (window-scroll-down (cursor-window cursor) (* n base) :cursor-lock base)))

(define-editor-command scroll-up-half
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (let ((base (1+ (truncate (window-height (cursor-window cursor)) 2))))
    (window-scroll-up (cursor-window cursor) (* n base) :cursor-lock base :restrict-end t)))

(define-editor-command scroll-down-full
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (let ((base (coord-y (window-dims (cursor-window cursor)))))
    (window-scroll-down (cursor-window cursor) (* n base) :cursor-lock base)))

(define-editor-command scroll-up-full
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (let ((base (coord-y (window-dims (cursor-window cursor)))))
    (window-scroll-up (cursor-window cursor) (* n base) :cursor-lock base :restrict-end t)))

(define-editor-command scroll-left
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (window-scroll-left (cursor-window cursor) n))

(define-editor-command scroll-right
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (window-scroll-right (cursor-window cursor) n))

(define-editor-command scroll-left-half
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (let ((window (cursor-window cursor)))
    (window-scroll-left window (1+ (* n (truncate (window-view-width window) 2))))))

(define-editor-command scroll-right-half
    (:cursor cursor :modes (:insert :normal :visual) :n n :record (:insert :record :visual :record))
  (let ((window (cursor-window cursor)))
    (window-scroll-right window (1+ (* n (truncate (window-view-width window) 2))))))

(define-editor-command scroll-hstart
    (:cursor cursor :modes (:insert :normal :visual) :record (:insert :record :visual :record))
  (window-scroll-halign (cursor-window cursor) 1))

(define-editor-command scroll-hend
    (:cursor cursor :modes (:insert :normal :visual) :record (:insert :record :visual :record))
  (window-scroll-halign (cursor-window cursor) -1))

(define-editor-command scroll-cursor-top
    (:cursor cursor :modes (:insert :normal :visual) :record (:insert :record :visual :record))
  (window-scroll-down (cursor-window cursor) (coord-y (window-point-view-coord (cursor-window cursor) (cursor-point cursor)))))

(define-editor-command scroll-cursor-middle
    (:cursor cursor :modes (:insert :normal :visual) :record (:insert :record :visual :record))
  (window-scroll-down (cursor-window cursor) (- (coord-y (window-point-view-coord (cursor-window cursor) (cursor-point cursor)))
                                                (truncate (1- (window-height (cursor-window cursor))) 2))))

(define-editor-command scroll-cursor-bottom
    (:cursor cursor :modes (:insert :normal :visual) :record (:insert :record :visual :record))
  (window-scroll-down (cursor-window cursor) (- (coord-y (window-point-view-coord (cursor-window cursor) (cursor-point cursor)))
                                                (1- (window-height (cursor-window cursor))))))

(defun window-line-vcoord (window wline)
  (declare (type (integer 0) wline))
  (let ((i 0)
        (ret nil))
    (do-wlines-down (v window (window-view-point window))
      (when (<= wline i)
        (return-from window-line-vcoord v))
      (setf ret v)
      (incf i))
    ret))

(macrolet ((def (name &body position)
             `(define-editor-command ,name
                  (:cursor cursor :modes (:normal :visual) :mode mode :operator operator :record (:visual :record))
                (let* ((window (cursor-window cursor))
                       (position (progn ,@position)))
                  (if operator
                      (order-points :point (cursor-point cursor) (text-vcoord-point (cursor-text cursor) position))
                      (ecase mode
                        ((:normal :visual)
                         (window-set-jump window)
                         (psetf (cursor-vcoord cursor) position
                                (cursor-snapcol cursor) (cursor-snapcol cursor)))))))))
  ;; COMPAT: these commands act point-wise instead of line-wise
  (def top-of-window
    (if (buffer-variable (cursor-buffer cursor) 'wrap)
        (coord+ (window-line-vcoord window 0)
                (coord (mod (cursor-snapcol cursor) (window-view-width window)) 0))
        (coord (cursor-snapcol cursor) (point-line (window-view-point window)))))
  (def middle-of-window
    (if (buffer-variable (cursor-buffer cursor) 'wrap)
        (coord+ (window-line-vcoord window (truncate (window-height window) 2))
                (coord (mod (cursor-snapcol cursor) (window-view-width window)) 0))
        (coord (cursor-snapcol cursor) (min (1- (buffer-nlines (cursor-buffer cursor)))
                                            (+ (point-line (window-view-point window))
                                               (truncate (window-height window) 2))))))
  (def bottom-of-window
    (if (buffer-variable (cursor-buffer cursor) 'wrap)
        (coord+ (window-line-vcoord window (1- (window-height window)))
                (coord (mod (cursor-snapcol cursor) (window-view-width window)) 0))
        (coord (cursor-snapcol cursor) (min (1- (buffer-nlines (window-buffer window)))
                                            (+ (point-line (window-view-point window))
                                               (1- (window-height window))))))))

(macrolet ((def (name do-lines sign error)
             `(defun ,name (window point &optional (n 1))
                (let ((vcoord nil))
                  (if (buffer-variable (window-buffer window) 'wrap)
                      (let ((i -1))
                        (,do-lines (v window point) (incf i) (setf vcoord v) (when (<= n i) (return)))
                        (when (and (= n 1) (< i n))
                          ,error))
                      (progn
                        (setf vcoord (coord (window-view-vcol window) (min (1- (buffer-nlines (window-buffer window))) (max 0 (,sign (point-line point) n)))))
                        (when (and (= n 1) (= (coord-y vcoord) (point-line point)))
                          ,error)))
                  (values vcoord (coord+ vcoord (coord (window-view-width window) 0)))))))
  (def previous-visual-line-vcoord do-wlines-up - (bob-error))
  (def next-visual-line-vcoord do-wlines-down + (eob-error)))

(macrolet ((def (name vcoord &key (cursor-point 'cursor-point) (transform-point 'point) (snapcol '(cursor-snapcol cursor)))
             `(define-editor-command ,name
                  (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
                (let ((point (text-vcoord-point (buffer-text (cursor-buffer cursor))
                                                ,vcoord)))
                  (setf point ,transform-point)
                  (if operator
                      (order-points :point (cursor-point cursor) point)
                      (psetf (,cursor-point cursor) point
                             (cursor-snapcol cursor) ,snapcol))))))
  (def previous-visual-line
       (multiple-value-bind (v1 v2) (previous-visual-line-vcoord (cursor-window cursor) (cursor-point cursor) n)
         (coord+ v1 (coord (mod (- (cursor-snapcol cursor) (window-view-vcol (cursor-window cursor)))
                                (- (coord-x v2) (coord-x v1))) 0))))
  (def next-visual-line
       (multiple-value-bind (v1 v2) (next-visual-line-vcoord (cursor-window cursor) (cursor-point cursor) n)
         (coord+ v1 (coord (mod (- (cursor-snapcol cursor) (window-view-vcol (cursor-window cursor)))
                                (- (coord-x v2) (coord-x v1))) 0))))
  (def beginning-of-visual-line
       (previous-visual-line-vcoord (cursor-window cursor) (cursor-point cursor) (1- n))
       :snapcol nil)
  (def end-of-visual-line
       (nth-value 1 (next-visual-line-vcoord (cursor-window cursor) (cursor-point cursor) (1- n)))
       :cursor-point cursor-point-after
       :snapcol nil)
  (def back-to-visible-indentation
       (previous-visual-line-vcoord (cursor-window cursor) (cursor-point cursor) (1- n))
       :transform-point (loop until (not (member (point-peek point) '(#\Space #\Tab))) do (point-skip point) finally (return point))
       :snapcol nil)
  (def middle-of-visual-line
       (multiple-value-bind (v1 v2) (next-visual-line-vcoord (cursor-window cursor) (cursor-point cursor) 0)
         (coord+ v1 (coord (truncate (- (coord-x v2) (coord-x v1)) 2) 0)))
       :snapcol nil))

(define-editor-command jump-forward
    (:cursor cursor :modes (:normal) :n n)
  (window-forward-jumps (cursor-window cursor) n))

(define-editor-command jump-backward
    (:cursor cursor :modes (:normal) :n n)
  (window-backward-jumps (cursor-window cursor) n))
