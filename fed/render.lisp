(in-package "FED")


(defstruct text-face
  (attributes nil :type list))

(defclass global-render-state ()
  ((cursor-type
    :initform nil
    :accessor global-render-state-cursor-type
    :type (or null cursor-type))
   (cursor-visibility
    :initform nil
    :accessor global-render-state-cursor-visibility
    :type (member nil :invisible :visible))
   (active-buffer
    :initform nil
    :accessor global-render-state-active-buffer
    :type (member nil :main :alt))))

(defun make-global-render-state ()
  (make-instance 'global-render-state))

(defun clear-global-render-state (state)
  (setf (global-render-state-cursor-type state) nil
        (global-render-state-cursor-visibility state) nil
        (global-render-state-active-buffer state) nil))

(defclass render-state ()
  ((cursor-pos
    :initform coord0
    :accessor render-state-cursor-pos
    :type coord)
   (buffer
    :initarg :buffer
    :accessor render-state-buffer
    :type (array * (* *)))))

(defclass repl-render-state (render-state)
  ((cleared-p
    :initarg :cleared-p
    :accessor render-state-cleared-p
    :type boolean)
   (message
    :initform ""
    :accessor render-state-message
    :type string)))

(defun make-render-state (dims)
  (make-instance 'render-state :buffer (make-render-buffer dims)))

(defun make-repl-render-state (dims cleared-p)
  (make-instance 'repl-render-state :buffer (make-render-buffer dims) :cleared-p cleared-p))

(defun adjust-render-state (state dims)
  (let ((buffer (adjust-array (render-state-buffer state) (list (coord-y dims) (coord-x dims)))))
    (loop for i below (array-dimension buffer 0)
          do (loop for j below (array-dimension buffer 1)
                   do (when (eql 0 (aref buffer i j))
                        (setf (aref buffer i j) (make-render-char)))))
    (setf (render-state-buffer state) buffer)))

(defun clear-render-state (state)
  (setf (render-state-cursor-pos state) coord0)
  (clear-render-buffer (render-state-buffer state))
  (when (typep state 'repl-render-state)
    (setf (render-state-cleared-p state) nil
          (render-state-message state) "")))

(defstruct render-char
  (char #\Space)
  (foreground :default)
  foreground-z
  (background :default)
  background-z
  bold
  italic
  underline)

(defun clear-render-char (rc)
  (setf (render-char-char rc) #\Space
        (render-char-foreground rc) :default
        (render-char-foreground-z rc) nil
        (render-char-background rc) :default
        (render-char-background-z rc) nil
        (render-char-bold rc) nil
        (render-char-italic rc) nil
        (render-char-underline rc) nil))

(defun render-char-visually-identical-p (rc1 rc2)
  (and (eql (render-char-char rc1) (render-char-char rc2))
       (eql (render-char-foreground rc1) (render-char-foreground rc2))
       (eql (render-char-background rc1) (render-char-background rc2))
       (eql (render-char-bold rc1) (render-char-bold rc2))
       (eql (render-char-italic rc1) (render-char-italic rc2))
       (eql (render-char-underline rc1) (render-char-underline rc2))))

(defun make-render-buffer (dims)
  (let ((ret (make-array (list (coord-y dims) (coord-x dims)) :initial-element nil)))
    (loop for i below (coord-y dims)
          do (loop for j below (coord-x dims)
                   do (setf (aref ret i j) (make-render-char))))
    ret))

(defun adjust-render-buffer-height (render-buffer height)
  (let* ((prev-height (array-dimension render-buffer 0))
         (width (array-dimension render-buffer 1))
         (ret (adjust-array render-buffer (list height width))))
    (loop for i from prev-height below height
          do (loop for j below width
                   do (setf (aref ret i j) (make-render-char))))
    ret))

(defun clear-render-buffer (render-buffer)
  (loop for i below (array-dimension render-buffer 0)
        do (loop for j below (array-dimension render-buffer 1)
                 do (clear-render-char (aref render-buffer i j)))))

(defun rich-string-mapc-style (function s)
  (declare (type rich-string s))
  (let ((fg (if (boundp 'fg) (symbol-value 'fg) :default))
        (bg (if (boundp 'bg) (symbol-value 'bg) :default))
        (bold0 (if (boundp 'bold0) (symbol-value 'bold0) nil))
        (italic0 (if (boundp 'italic0) (symbol-value 'italic0) nil))
        (underline0 (if (boundp 'underline0) (symbol-value 'underline0) nil)))
    (declare (special fg bg))
    (etypecase s
      (string (loop for ch across s do (funcall function ch fg bg bold0 italic0 underline0)))
      (vector (loop for s across s do (rich-string-mapc-style function s)))
      (cons (destructuring-bind (&key foreground background bold italic underline) (cdr s)
              (when foreground (setf fg foreground))
              (when background (setf bg background))
              (setf bold0 (or bold0 bold) italic0 (or italic0 italic) underline0 (or underline0 underline))
              (rich-string-mapc-style function (car s))))))
  s)

(defun rich-string-length (s &aux (sum 0))
  (rich-string-mapc (lambda (ch) (declare (ignore ch)) (incf sum)) s)
  sum)

(defun rich-string-width (s tabsize &aux (sum 0))
  (rich-string-mapc (lambda (ch) (incf sum (true-char-width ch sum tabsize))) s)
  sum)

(defun rich-string-render (s render-buffer to width)
  (declare (type (array render-char (* *)) render-buffer))
  ;; NOTE: does not handle multi-width chars or tabs/newlines
  (let ((end (+ (coord-x to) width))
        (x (coord-x to))
        (y (coord-y to)))
    (block nil
      (rich-string-mapc-style
       (lambda (ch fg bg bold italic underline)
         (unless (< x end)
           (return))
         (assert (= 1 (true-char-width ch 0 4)))
         (let ((rc (aref render-buffer y x)))
           (setf (render-char-char rc) ch
                 (render-char-foreground rc) fg
                 (render-char-background rc) bg
                 (render-char-bold rc) bold
                 (render-char-italic rc) italic
                 (render-char-underline rc) underline))
         (incf x))
       s))))

(defun render-buffer-draw (render-buffer old-render-buffer cursor stream)
  (let ((foreground :default) (background :default) bold italic underline
        (d0 (array-dimension render-buffer 0))
        (d1 (array-dimension render-buffer 1))
        (first-draw t))
    (assert (= d0 (array-dimension old-render-buffer 0)))
    (assert (= d1 (array-dimension old-render-buffer 1)))
    (labels ((ensure-properties (rc)
               (unless (eql foreground (render-char-foreground rc))
                 (csi-text-foreground (setf foreground (render-char-foreground rc)) stream))
               (unless (eql background (render-char-background rc))
                 (csi-text-background (setf background (render-char-background rc)) stream))
               (cond ((and (render-char-bold rc) (not bold)) (setf bold t) (csi-text-bold stream))
                     ((and (not (render-char-bold rc)) bold) (setf bold nil) (csi-text-unbold stream)))
               (cond ((and (render-char-italic rc) (not italic)) (setf italic t) (csi-text-italic stream))
                     ((and (not (render-char-italic rc)) italic) (setf italic nil) (csi-text-unitalic stream)))
               (cond ((and (render-char-underline rc) (not underline)) (setf underline t) (csi-text-underline stream))
                     ((and (not (render-char-underline rc)) underline) (setf underline nil) (csi-text-ununderline stream))))
             (draw-section (line start end clear-idx)
               (when first-draw
                 (csi-text-reset stream)
                 (setf first-draw nil))
               (setf cursor (move-cursor cursor (coord start line) stream))
               (loop for j from start below end
                     do (let ((rc (aref render-buffer line j)))
                          (awhen (render-char-char rc)
                            (ensure-properties rc)
                            (if (or (not clear-idx) (< j clear-idx))
                                (write-char it stream)
                                (progn
                                  (csi-erase-line-end stream)
                                  (setf cursor (coord j line))
                                  (return)))))
                     finally (setf cursor (coord end line)))))
      (loop for i below d0
            do (let ((end (let ((rc (aref render-buffer i (1- d1))))
                            (when (eql #\Space (render-char-char rc))
                              (- (1- d1)
                                 (loop for j from (- d1 2) downto 0
                                       while (render-char-visually-identical-p (aref render-buffer i j) rc)
                                       count t))))))
                 (loop with j = 0 and section-start = nil and section-end = nil and n-same = nil
                       while (< j d1)
                       do (let* ((rc (aref render-buffer i j))
                                 (width (char-width (render-char-char rc))))
                            (when (or (not (render-char-char rc))
                                      (< d1 (+ j width)))
                              (clear-render-char rc)
                              (setf (render-char-char rc) #\Space
                                    (render-char-background rc) :blue
                                    width 1))
                            (if (render-char-visually-identical-p rc (aref old-render-buffer i j))
                                (when section-start
                                  (if section-end
                                      (when (= 0 (decf n-same))
                                        (draw-section i section-start section-end end)
                                        (when (and end (< end section-end))
                                          (return))
                                        (setf section-start nil
                                              section-end nil
                                              n-same nil))
                                      (setf section-end j
                                            ;; 3 subsequent same characters are connected to next difference section
                                            n-same (1- 3))))
                                (if section-start
                                    (when section-end
                                      (setf section-end nil
                                            n-same nil))
                                    (setf section-start j)))
                            (incf j width))
                       finally (when section-start
                                 (draw-section i section-start (or section-end d1) end)))
                 (unless (< (coord-x cursor) d1)
                   (assert (= (coord-x cursor) d1))
                   ;; Reset pending-wrap state
                   (csi-cursor-backward 1 stream)
                   (setf cursor (coord- cursor (coord 1 0))))))
      (unless first-draw
        (csi-text-reset stream))
      cursor)))

(defun render-state-move-cursor (rs to stream)
  (declare (type render-state rs)
           (type coord to)
           (type stream stream))
  (setf (render-state-cursor-pos rs) (move-cursor (render-state-cursor-pos rs) to stream)))

(defun render-state-clear-display (state stream)
  (render-state-move-cursor state coord0 stream)
  (csi-erase-display-end stream)
  (clear-render-buffer (render-state-buffer state)))

(defun exit-alt-buffer (stream)
  (csi-disable-alt-screen stream)
  (csi-disable-mouse stream))

(defun enter-alt-buffer (stream)
  (csi-enable-alt-screen stream)
  (csi-enable-mouse stream))

(defun exit-render (gs repl-state stream &optional (after-p nil))
  (declare (type global-render-state gs)
           (type repl-render-state repl-state)
           (type stream stream))
  (unless (null (global-render-state-active-buffer gs))
    (let ((render-buffer (render-state-buffer repl-state)))
      (when (eql :alt (global-render-state-active-buffer gs))
        (exit-alt-buffer stream))
      (unless (render-state-cleared-p repl-state)
        (setf (render-state-cleared-p repl-state) t)
        (if after-p
            (progn
              (render-state-move-cursor repl-state (coord 0 (1- (array-dimension render-buffer 0))) stream)
              (write-char #\Newline stream)
              (setf (render-state-cursor-pos repl-state) coord0)
              (clear-render-buffer render-buffer))
            (render-state-clear-display repl-state stream)))
      (unless (eql :block (global-render-state-cursor-type gs))
        (csi-set-cursor-type :block stream))
      (unless (eql :visible (global-render-state-cursor-visibility gs))
        (csi-show-cursor stream))
      (clear-global-render-state gs)
      (csi-disable-paste stream)
      (finish-output stream)
      (when (input-tty-p)
        (activate-default-termios))
      (setf (editor-stream-capturing-p (output-stream)) nil)
      t)))

(defun render-diff-draw (gs s gsp sp stream)
  (declare (type global-render-state gs gsp)
           (type render-state s sp)
           (type stream stream))
  ;; Print the message, if one is pending
  (when (and (typep s 'repl-render-state)
             (not (string= "" (render-state-message s))))
    (exit-render gsp sp stream nil)
    (write-string (render-state-message s))
    (finish-output))
  ;; Prepare the terminal for rendering (activate the correct buffer, and ensure state matches sp)
  (let ((ab (global-render-state-active-buffer gs))
        (abp (global-render-state-active-buffer gsp)))
    (assert ab)
    (when (and (eql abp :alt) (not (eql ab :alt)))
      (exit-alt-buffer stream))
    (when (null abp)
      (csi-enable-paste stream)
      (finish-output stream)
      (when (input-tty-p)
        (activate-editor-termios))
      (setf (editor-stream-capturing-p (output-stream)) t))
    (when (and (eql ab :alt) (not (eql abp :alt)))
      (enter-alt-buffer stream)
      (clear-render-state sp))
    (when (and (eql ab :main) (null abp))
      (assert (render-state-cleared-p sp))
      (when (and (termio-initialised-p)
                 (not (= 0 (coord-x (csi-report-cursor stream)))))
        (csi-text-invert stream)
        (write-char #\% stream)
        (csi-cursor-backward 1 stream)
        (csi-text-reset stream)
        (write-char #\Newline stream)
        (csi-cursor-horizontal-absolute 1 stream))))
  (let ((*move-absolute* (eql :alt (global-render-state-active-buffer gs)))
        (prev-height (array-dimension (render-state-buffer sp) 0))
        (height (array-dimension (render-state-buffer s) 0)))
    ;; Ensure that both buffers have matching dimensions
    (unless (equal (array-dimensions (render-state-buffer s)) (array-dimensions (render-state-buffer sp)))
      (adjust-render-state sp (coord (array-dimension (render-state-buffer s) 1) (array-dimension (render-state-buffer s) 0)))
      (csi-cursor-horizontal-absolute 1 stream)
      (setf (render-state-cursor-pos sp) (coord 0 (coord-y (render-state-cursor-pos sp))))
      (render-state-clear-display sp stream)
      (let ((diff (- height prev-height)))
        (when (and (eql :main (global-render-state-active-buffer gs))
                   (< 0 diff))
          (render-state-move-cursor sp (coord 0 (1- prev-height)) stream)
          (loop repeat diff
                do (write-char #\Newline stream)
                   (csi-erase-line stream))
          (csi-cursor-up diff stream))))
    ;; Render the buffer, and set the cursor position
    (move-cursor (render-buffer-draw (render-state-buffer s) (render-state-buffer sp) (render-state-cursor-pos sp) stream)
                 (render-state-cursor-pos s)
                 stream))
  ;; Set cursor-type
  (let ((cursor-type (global-render-state-cursor-type gs)))
    (assert cursor-type)
    (unless (eql cursor-type (global-render-state-cursor-type gsp))
      (csi-set-cursor-type cursor-type stream)))
  ;; Set cursor-visibility
  (let ((visibility (global-render-state-cursor-visibility gs)))
    (assert visibility)
    (unless (eql visibility (global-render-state-cursor-visibility gsp))
      (ecase visibility (:visible (csi-show-cursor stream)) (:invisible (csi-hide-cursor stream)))))
  ;; Commit
  (finish-output stream))

(defun render-buffer-strings (render-buffer)
  (loop for i below (array-dimension render-buffer 0)
        collect (concatenate 'string (loop for j below (array-dimension render-buffer 1)
                                           collect (render-char-char (aref render-buffer i j))))))

(defun buffer-render-line (buffer render-buffer to line start-vcol end-vcol)
  (unless (< start-vcol end-vcol)
    (return-from buffer-render-line))
  (let* ((text (buffer-text buffer))
         (tabsize (text-tabsize text))
         (width (- end-vcol start-vcol))
         (valid-p (nth-value 1 (buffer-ast buffer))))
    (buffer-ensure-search-results buffer)
    (loop for j from (coord-x to) repeat width
          do (clear-render-char (aref render-buffer (coord-y to) j)))
    (unless (<= 0 line (1- (buffer-nlines buffer)))
      (return-from buffer-render-line))
    (let* ((point-start (text-vcoord-point text (coord start-vcol line)))
           (point (copy-point point-start))
           (vcol 0))
      (labels ((vcol-rc (i)
                 (aref render-buffer (coord-y to) (+ (coord-x to) (- i start-vcol))))
               (set-char (i ch)
                 (setf (render-char-char (vcol-rc i)) ch))
               (put-char-width (ch width)
                 (cond ((< vcol start-vcol))
                       ((< end-vcol (+ vcol width))
                        (loop for i from vcol below end-vcol
                              do (set-char i #\Space)))
                       (t
                        (set-char vcol ch)
                        (loop for i from 1 below width
                              do (set-char i nil))))
                 (incf vcol width))
               (restrict-range (i)
                 (min (max i start-vcol) end-vcol))
               (put-char (ch &aux (vcol1 vcol))
                 (if (char= ch #\Newline)
                     (progn
                       (setf vcol (max vcol start-vcol))
                       (loop repeat (- end-vcol vcol) do (put-char-width #\Space 1)))
                     (multiple-value-bind (width first second) (true-char-width ch vcol tabsize)
                       (if (char= ch #\Tab)
                           (loop repeat width do (put-char-width #\Space 1))
                           (case width
                             (0 nil)
                             (1 (put-char-width first 1))
                             (t
                              (put-char-width first (- width (if second 1 0)))
                              (when second (put-char-width second 1)))))))
                 (values (restrict-range vcol1) (restrict-range vcol)))
               (restrict-vcoord-x (vcoord)
                 (cond ((lcoord< vcoord (coord start-vcol line))
                        start-vcol)
                       ((lcoord< (coord end-vcol line) vcoord)
                        end-vcol)
                       (t
                        (coord-x vcoord))))
               (paint (attrs region)
                 (when attrs
                   (destructuring-bind (&key foreground background bold italic underline (z 0)) attrs
                     (loop for i from (restrict-vcoord-x (point-vcoord (region-left region)))
                           below (restrict-vcoord-x (point-vcoord (region-right region)))
                           for rc = (vcol-rc i)
                           do (when (and foreground (or (null (render-char-foreground-z rc)) (<= (render-char-foreground-z rc) z)))
                                (setf (render-char-foreground rc) foreground
                                      (render-char-foreground-z rc) z))
                              (when (and background (or (null (render-char-background-z rc)) (<= (render-char-background-z rc) z)))
                                (setf (render-char-background rc) background
                                      (render-char-background-z rc) z))
                              (when bold (setf (render-char-bold rc) t))
                              (when italic (setf (render-char-italic rc) t))
                              (when underline (setf (render-char-underline rc) t)))))))
        (when (< start-vcol (coord-x (point-vcoord point)))
          (rich-string-mapc-style
           (lambda (ch fg bg bold italic underline)
             (multiple-value-bind (start end) (put-char ch)
               (loop for i from start below end
                     do (let ((rc (vcol-rc i)))
                          (setf (render-char-foreground rc) fg
                                (render-char-background rc) bg
                                (render-char-bold rc) bold
                                (render-char-italic rc) italic
                                (render-char-underline rc) underline)))))
           (buffer-line-prefix buffer line)))
        (setf vcol (coord-x (point-vcoord point)))
        (loop do (let ((ch (point-next point)))
                   (if (or (null ch) (char= ch #\Newline))
                       (progn (put-char #\Newline) (return))
                       (put-char ch)))
              until (<= end-vcol vcol))
        (if valid-p
            (do-regions-between (region point-start point)
              (cond ((text-face-p (region-data region))
                     (paint (text-face-attributes (region-data region)) region))
                    ((ast-p (region-data region))
                     (paint (gethash (region-data region) (buffer-node-attrs buffer)) region))
                    ((and (consp (region-data region)) (ast-p (car (region-data region))))
                     (loop for node in (region-data region)
                           do (paint (gethash node (buffer-node-attrs buffer)) region)))))
            (loop for i from start-vcol below end-vcol
                  for rc = (vcol-rc i)
                  do (setf (render-char-foreground rc) :red)))))))
