(in-package "FED")


(defun frames ()
  (list (full-frame) (repl-frame)))

(defclass frame ()
  ((editor
    :initarg :editor
    :reader frame-editor
    :type editor)
   (dims
    :initarg :dims
    :accessor frame-dims
    :type coord)
   (global-render-state
    :initform (make-global-render-state)
    :accessor frame-global-render-state
    :type global-render-state)
   (render-state
    :initarg :render-state
    :accessor frame-render-state
    :type render-state)
   (prev-render-state
    :initarg :render-state-alt
    :accessor frame-prev-render-state
    :type render-state)))

(defun frame-p (frame) (typep frame 'frame))
(defun frame-stream (frame) (editor-render-stream (frame-editor frame)))
(defun frame-width (frame) (coord-x (frame-dims frame)))
(defun frame-height (frame) (coord-y (frame-dims frame)))

(defgeneric frame-windows (frame))
(defgeneric frame-focused-window (frame))
(defgeneric frame-selected-window (frame))

;; Full frame

(defclass full-frame (frame)
  ((main-window
    :initarg :main-window
    :reader full-frame-main-window
    :type window)
   (focused-window
    :initarg :focused-window
    :accessor full-frame-focused-window
    :reader frame-focused-window
    :type window)
   (selected-window
    :initarg :focused-window
    :accessor full-frame-selected-window
    :reader frame-selected-window
    :type window)
   (minibuffer
    :initarg :minibuffer
    :reader full-frame-minibuffer
    :type buffer)
   (minibuffer-window
    :initarg :minibuffer-window
    :reader full-frame-minibuffer-window
    :type window)))

(defun full-frame-p (frame)
  (typep frame 'full-frame))

(defun make-full-frame (termsize)
  (let* ((ret (make-instance 'full-frame
                             :editor *editor*
                             :dims termsize
                             :render-state (make-render-state termsize)
                             :render-state-alt (make-render-state termsize)))
         (minibuffer-window
           ;; TODO: error-proof dims and offset
           (create-window ret (coord (coord-x termsize) 1)
                          :coord (coord 0 (1- (coord-y termsize)))))
         (main-window
           ;; TODO: error proof dims and offset
           (create-window ret (coord (coord-x termsize) (- (coord-y termsize) 2))))
         (minibuffer (create-minibuffer minibuffer-window)))
    (setf (slot-value ret 'main-window) main-window
          (full-frame-focused-window ret) main-window
          (full-frame-selected-window ret) main-window
          (slot-value ret 'minibuffer) minibuffer
          (slot-value ret 'minibuffer-window) minibuffer-window)
    ret))

(defmethod frame-windows ((frame full-frame))
  (list (full-frame-main-window frame) (full-frame-minibuffer-window frame)))

;; Repl Frame

(defclass repl-frame (frame)
  ((window
    :initarg :window
    :reader repl-frame-window)
   (max-height
    :initarg :max-height
    :accessor repl-frame-max-height
    :type (integer 1))))

(defun repl-frame-p (frame)
  (typep frame 'repl-frame))

(defmethod frame-windows ((frame repl-frame))
  (list (repl-frame-window frame)))

(defmethod frame-focused-window ((frame repl-frame))
  (repl-frame-window frame))

(defmethod frame-selected-window ((frame repl-frame))
  (repl-frame-window frame))

(defun make-repl-frame (termsize)
  (let* ((dims (coord (1- (coord-x termsize)) 1))
         (ret (make-instance 'repl-frame
                             :editor *editor*
                             :dims dims
                             :render-state (make-repl-render-state dims nil)
                             :render-state-alt (make-repl-render-state dims t)
                             :max-height (coord-y termsize)))
         (window (create-window ret dims)))
    (setf (slot-value ret 'window) window)
    ret))

;; Frames

(defun current-frame ()
  (editor-current-frame *editor*))

(defun (setf current-frame) (new)
  (etypecase new
    (full-frame (assert (eql new (full-frame))))
    (repl-frame (assert (eql new (repl-frame))))
    (null))
  (setf (editor-current-frame *editor*) new))

(defun frame-display-output-hook (frame)
  (let ((string (editor-stream-get-capture-string (editor-stream (frame-editor frame)) (typep frame 'full-frame))))
    (etypecase frame
      (repl-frame
       (setf (render-state-message (frame-render-state frame)) string))
      (full-frame
       (add-minibuffer-string string)))))

(defun frame-expand-repl-hook (frame)
  (etypecase frame
    (repl-frame
     (let* ((window (repl-frame-window frame))
            (height (min (window-buffer-wnlines window)
                         (repl-frame-max-height frame)))
            (dims (coord (frame-width frame) height)))
       (unless (= height (frame-height frame))
         (setf (frame-dims frame) dims
               (window-dims window) dims)
         (adjust-render-state (frame-render-state frame) dims))
       (setf (window-view-point window) (text-pos-point (buffer-text (window-buffer window)) 0))))
    (full-frame
     (let* ((window (full-frame-minibuffer-window frame))
            (buffer (window-buffer window))
            (height (max 0 (min (window-buffer-wnlines window)
                                (truncate (* (1- (frame-height frame)) (/ 40 100)))))))
       (setf (window-view-point window) (text-pos-point (buffer-text buffer) 0)
             (window-dims window) (coord (window-width window) height)
             (window-coord window) (coord 0 (- (frame-height frame) height)))))))

(defun frame-recentre-focused-window-hook (frame)
  (window-recentre (frame-focused-window frame)))

(add-default-editor-hook 'pre-render 'frame-recentre-focused-window-hook)
(add-default-editor-hook 'pre-render 'frame-expand-repl-hook)
(add-default-editor-hook 'pre-render 'frame-display-output-hook)

(defmethod frame-render (frame)
  (let* ((global-state (frame-global-render-state frame))
         (state (frame-render-state frame))
         (window (frame-focused-window frame))
         (cursor (window-cursor window)))
    (setf (global-render-state-cursor-type global-state)
          (ecase (cursor-mode cursor)
            ((:normal :visual) :block)
            ((:insert) :bar)
            ((:operator) :underline)))
    (aif (window-point-gcoord window (cursor-point cursor))
         (setf (render-state-cursor-pos state) it
               (global-render-state-cursor-visibility global-state) :visible)
         (setf (render-state-cursor-pos state) coord0
               (global-render-state-cursor-visibility global-state) :invisible))))

(defmethod frame-render ((frame full-frame))
  (call-next-method)
  (setf (global-render-state-active-buffer (frame-global-render-state frame)) :alt)
  (let* ((render-state (frame-render-state frame))
         (render-buffer (render-state-buffer render-state))
         (minibuffer-window (full-frame-minibuffer-window frame)))
    (window-render (full-frame-main-window frame) render-buffer)
    (rich-string-render (frame-modeline frame) render-buffer
                        (coord 0 (- (frame-height frame) (window-height minibuffer-window) 1))
                        (frame-width frame))
    (if (and (eql (window-buffer minibuffer-window) (full-frame-minibuffer frame))
             (= 0 (text-length (buffer-text (window-buffer minibuffer-window)))))
        (rich-string-render (frame-default-minibuffer frame) render-buffer
                            (coord 0 (- (frame-height frame) 1))
                            (frame-width frame))
        (window-render minibuffer-window render-buffer))))

(defmethod frame-render ((frame repl-frame))
  (call-next-method)
  (setf (global-render-state-active-buffer (frame-global-render-state frame)) :main)
  (window-render (repl-frame-window frame) (render-state-buffer (frame-render-state frame))))

(defun frame-draw (frame)
  (render-diff-draw (frame-global-render-state frame) (frame-render-state frame)
                    (editor-render-state (frame-editor frame)) (frame-prev-render-state frame)
                    (frame-stream frame))
  (rotatef (editor-render-state (frame-editor frame)) (frame-global-render-state frame))
  (rotatef (frame-render-state frame) (frame-prev-render-state frame))
  (clear-global-render-state (frame-global-render-state frame))
  (clear-render-state (frame-render-state frame)))

(defmethod frame-handle-winch ((frame full-frame) termsize)
  (setf (frame-dims frame) termsize)
  (adjust-render-state (frame-render-state frame) termsize)
  (let ((minibuffer-window (full-frame-minibuffer-window frame))
        (dims (frame-dims frame)))
    ;; TODO: error-proof dims and offset
    (setf (window-coord minibuffer-window) (coord 0 (1- (coord-y dims)))
          (window-dims minibuffer-window) (coord (coord-x dims) 1)
          (window-dims (full-frame-main-window frame)) (coord (coord-x dims)
                                                              (max 0 (- (coord-y dims) 2))))))

(defmethod frame-handle-winch ((frame repl-frame) termsize)
  ;; TODO: fix prompt redraw for multi-line prompts by calculating line reflow
  (let ((dims (coord (1- (coord-x termsize)) 1)))
    (setf (frame-dims frame) dims)
    (adjust-render-state (frame-render-state frame) dims)
    (setf (window-dims (repl-frame-window frame)) dims)))

(defvar *memory-usage* nil)

(defun gc-and-record-memory-usage-timer ()
  (sb-ext:gc :full t)
  (setf *memory-usage* (sb-kernel:dynamic-usage))
  (start-gc-and-record-memory-usage-timer))

(defun start-gc-and-record-memory-usage-timer ()
  (add-editor-timer (list 10 :second) 'gc-and-record-memory-usage-timer))

(defun reset-gc-and-record-memory-usage-timer ()
  (remove-editor-timer 'gc-and-record-memory-usage-timer)
  (start-gc-and-record-memory-usage-timer))

(add-default-editor-hook 'editor-init 'gc-and-record-memory-usage-timer)
(add-default-editor-hook 'post-command 'reset-gc-and-record-memory-usage-timer)

(defun format-bytes (stream bytes)
  (loop with n = bytes
        for suffix in '("B" "KB" "MB" "GB")
        do (if (< n (expt 2 10))
               (return (format stream "~d~a" n suffix))
               (setf n (truncate n (expt 2 10))))
        finally (return (format stream "~dTB" n))))

(defun frame-modeline (frame)
  (let* ((window (frame-selected-window frame))
         (buffer (window-buffer window))
         (cursor (window-cursor window))
         (vcoord (cursor-vcoord cursor))
         (left-string (format nil "~{~a~^  ~}"
                              (list (buffer-status-indicator buffer)
                                    (format nil "~a~15,0T" (buffer-name buffer))
                                    (format nil "~d:~d~9,0T" (1+ (coord-y vcoord)) (coord-x vcoord)))))
         (right-string (format nil "~{~a~^  ~}  "
                               (when *memory-usage*
                                 (list (format-bytes nil *memory-usage*)))))
         (width (frame-width frame))
         (full-string (concatenate 'string
                                   left-string
                                   (make-string (max 0 (- width (+ (length left-string) (length right-string))))
                                                :initial-element #\Space)
                                   right-string)))
    (subseq full-string 0 width)))

(defun frame-default-minibuffer (frame)
  (let ((cursor (window-cursor (frame-focused-window frame))))
    (concatenate 'string
                 (case (cursor-mode cursor)
                   (:insert "-- INSERT --")
                   (:visual (ecase (cursor-vtype cursor)
                              (:point "-- VISUAL --")
                              (:line "-- VISUAL LINE --"))))
                 (awhen (recording-register)
                   (format nil "recording @~a" it)))))

(defun exit-frames (&optional after-p)
  (exit-render (global-render-state) (frame-prev-render-state (repl-frame)) (render-stream) after-p))

(defun clear-frame (frame)
  (render-state-clear-display (frame-prev-render-state frame) (render-stream)))

(defun switch-to-frame (frame)
  (assert (or (eql frame (repl-frame)) (eql frame (full-frame))))
  (setf (editor-current-frame *editor*) frame))

(defmethod frame-focus-window ((frame full-frame) window)
  (assert (or (eql (full-frame-minibuffer-window frame) window)
              (eql (full-frame-main-window frame) window)))
  (unless (eql (full-frame-focused-window frame) window)
    (if (buffer-locked-p (window-buffer (full-frame-focused-window frame)))
        (editor-error "Can not switch away from locked window")
        (setf (full-frame-focused-window frame) window)))
  window)

(defmethod frame-focus-window ((frame repl-frame) window)
  (assert (eql (repl-frame-window frame) window)))

(define-editor-command switch-frame ()
  (if (full-frame-p (current-frame))
      (switch-to-frame (repl-frame))
      (switch-to-frame (full-frame))))

(define-editor-command refresh-screen ()
  (clear-frame (current-frame)))

(define-editor-command handle-sigwinch ()
  (let ((termsize (termsize)))
    (setf (editor-stream-line-length (output-stream)) (coord-x termsize))
    (loop for frame in (frames)
          do (frame-handle-winch frame termsize))))

