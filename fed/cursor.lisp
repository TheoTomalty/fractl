(in-package "FED")


(defstruct record
  (commands nil :type list))

(defstruct local-marks
  ;; Buffer is implicit
  (named (make-array 26 :initial-element nil) :type simple-vector)
  (insert nil :type (or null mark))
  (visual-start nil :type (or null mark))
  (visual-end nil :type (or null mark)))

(defstruct global-marks
  (named (make-array 26 :initial-element nil) :type simple-vector)
  (jump nil :type (or null (cons buffer mark))))

(defclass cursor ()
  ((buffer
    :initarg :buffer
    :reader cursor-buffer
    :type buffer)
   (window
    :initarg :window
    :reader cursor-window
    :type window)
   (mark
    :initarg :mark
    :accessor cursor-mark
    :type mark)
   (mode
    :initform :normal
    :accessor cursor-mode
    :type mode)
   (vmark
    :initform nil
    :accessor cursor-vmark
    :type (or null mark))
   (vtype
    :initform :point
    :accessor cursor-vtype
    :type (member :point :line))
   (vregion
    :initform nil
    :accessor cursor-vregion
    :type (or null region))
   (venterpos
    :initform nil
    :accessor cursor-venterpos
    :type (or null (integer 0)))
   (record
    :initform nil
    :accessor cursor-record
    :type (or null record))
   (record-tmp
    :initform nil
    :accessor cursor-record-tmp
    :type (or null record))
   (snapcol
    :initform nil
    :reader cursor-%snapcol
    :writer (setf cursor-snapcol)
    :type (or (member nil :eol) fixnum))
   (prev-find/till
    :initform nil
    :accessor cursor-prev-find/till)
   (prev-search
    :initform nil
    :accessor cursor-prev-search)
   (prev-visual
    :initform nil
    :accessor cursor-prev-visual)
   (marks
    :initform (make-local-marks)
    :accessor cursor-marks
    :type local-marks)))

(defun current-cursor ()
  (window-cursor (current-window)))

(defun make-cursor (buffer window)
  (make-instance 'cursor
                 :mark (make-mark-left-inserting (text-pos-point (buffer-text buffer) 0))
                 :buffer buffer
                 :window window))

(defun delete-cursor (cursor)
  (delete-mark (cursor-mark cursor))
  (awhen (cursor-vmark cursor) (delete-mark it))
  (awhen (cursor-vregion cursor) (delete-region it))
  (delete-local-marks (cursor-marks cursor)))

(defun delete-local-marks (marks)
  (loop for mark across (local-marks-named marks) do (when mark (delete-mark mark)))
  (awhen (local-marks-insert marks) (delete-mark it))
  (awhen (local-marks-visual-start marks) (delete-mark it))
  (awhen (local-marks-visual-end marks) (delete-mark it)))

(defun cursor-frame (cursor)
  (window-frame (cursor-window cursor)))

(defun cursor-line (cursor)
  (coord-y (point-coord (cursor-mark cursor))))

(defun cursor-text (cursor)
  (buffer-text (cursor-buffer cursor)))

(defun cursor-wrapcol (cursor)
  (window-wrapcol (cursor-window cursor)))

(defmethod (setf cursor-mark) :before (mark (cursor cursor))
  (assert (eql (mark-text mark) (buffer-text (cursor-buffer cursor)))
          nil "Mark not in buffer"))
(defmethod (setf cursor-vmark) :before (mark (cursor cursor))
  (when mark
    (assert (eql (mark-text mark) (buffer-text (cursor-buffer cursor)))
            nil "Mark not in buffer")))

;; TODO: get rid of cursor-update-visual-range by making it implicit in mark and vmark
(macrolet ((def (name)
             `(defmethod (setf ,name) :after (new (cursor cursor))
                (cursor-update-visual-range cursor))))
  (def cursor-mark)
  (def cursor-vmark)
  (def cursor-vtype))

(defmethod (setf cursor-mode) :after (new (cursor cursor))
  (setf (cursor-point cursor) (cursor-point cursor))
  (cursor-update-visual-range cursor))

(defmethod cursor-snapcol ((cursor cursor))
  (case (cursor-%snapcol cursor)
    ((nil) (coord-x (cursor-vcoord cursor)))
    ((:eol) -1)
    (t (cursor-%snapcol cursor))))

(defun cursor-point (cursor)
  (copy-point (cursor-mark cursor)))

(defun (setf cursor-point) (point cursor)
  (let* ((point (point-adjust (copy-point point) (cursor-mode cursor) (cursor-wrapcol cursor))))
    (setf (cursor-mark cursor)
          (prog1 (ecase (mark-type (cursor-mark cursor))
                   (:left-inserting (make-mark-left-inserting point))
                   (:right-inserting (make-mark-right-inserting point)))
            (delete-mark (cursor-mark cursor)))))
  point)

(defun cursor-vpoint (cursor)
  (copy-point (cursor-vmark cursor)))

(defun (setf cursor-vpoint) (point cursor)
  (awhen (cursor-vmark cursor)
    (delete-mark it))
  (setf (cursor-vmark cursor) (make-mark-left-inserting point))
  point)

(defun cursor-point-after (cursor)
  (point-after (cursor-point cursor) (cursor-mode cursor)))

(defun (setf cursor-point-after) (point cursor)
  (setf (cursor-point cursor) (point-before (copy-point point) (cursor-mode cursor))))

(defun (setf cursor-point-after1) (point cursor)
  (setf (cursor-point cursor) (point-before1 (copy-point point) (cursor-mode cursor) (cursor-wrapcol cursor))))

(defun cursor-vpoint-after (cursor)
  (point-after (cursor-vpoint cursor) (cursor-mode cursor)))

(defun (setf cursor-vpoint-after) (point cursor)
  (setf (cursor-vpoint cursor) (point-before (copy-point point) (cursor-mode cursor))))

(defun (setf cursor-vpoint-after1) (point cursor)
  (setf (cursor-vpoint cursor) (point-before1 (copy-point point) (cursor-mode cursor) (cursor-wrapcol cursor))))

(defun cursor-pos (cursor)
  (point-pos (cursor-mark cursor)))

(defun (setf cursor-pos) (new cursor)
  (setf (cursor-point cursor) (text-pos-point (buffer-text (cursor-buffer cursor)) new)))

(defun cursor-vcoord (cursor)
  (point-vcoord (cursor-mark cursor)))

(defun (setf cursor-vcoord) (new cursor)
  (setf (cursor-point cursor) (text-vcoord-point (buffer-text (cursor-buffer cursor)) new)))

(defun (setf cursor-vcoord-forward) (new cursor)
  (let ((point (text-vcoord-point (buffer-text (cursor-buffer cursor)) new)))
    (setf (cursor-point cursor)
          (if (and (<= 0 (coord-x new))
                   (< (coord-x (point-vcoord point)) (coord-x new))
                   (not (point-eol-p point)))
              (point-skip point)
              point))))

(defun point-bovl-p (point wrapcol)
  "Test if POINT is at the beginning of a visual line.  This could be
because it is physically at the beggining of the line, or because it
is at the WRAPCOL virtual column, and so is rendered on a new line."
  (or (point-bol-p point)
      (when wrapcol (= 0 (mod (coord-x (point-vcoord point)) wrapcol)))))

(defun point-adjust (point mode &optional wrapcol)
  "Adjusts the POINT so that it is in a legal position according to MODE."
  (when (and (ecase mode ((:normal :operator) (point-eol-p point)) (:visual (point-eob-p point)) (:insert nil))
             (not (point-bovl-p point wrapcol)))
    (point-skip-prev point))
  point)

(defun point-after (point mode)
  "Transforms POINT from the left-hand-side of a cursor (according to
MODE) to its corresponding right-hand-side.  If the POINT corresponds
to an illegal position, the cursor is considered to have zero width."
  (when (ecase mode ((:normal :operator) (not (point-eol-p point))) (:visual t) (:insert nil))
    (point-skip point))
  point)

(defun point-before (point mode)
  "Transforms POINT from the right-hand-side of a cursor (according to
MODE) to its corresponding left-hand-side.  This is the inverse of
POINT-AFTER acting on POINT, where it exists.  For points with more
than one inverse (at the end of a line), the left-most point is
chosen.  For points with no inverse (at the beginning of a line), the
point one character to the left is chosen."
  (when (ecase mode ((:normal :operator) (not (point-bol-p point))) (:visual t) (:insert nil))
    (point-skip-prev point))
  point)

(defun point-before1 (point mode &optional wrapcol)
  "Transforms POINT from the right-hand-side of a cursor (according to
MODE) to its corresponding left-hand-side.  Same as POINT-BEFORE,
except that the POINT does not pass visual line boundaries."
  ;; NOTE: there are two ``point-before'' functions because there is an ambiguity since ``point-after'' is not injective or surjective
  (when (ecase mode ((:normal :operator) (not (point-bovl-p point wrapcol))) (:visual t) (:insert nil))
    (point-skip-prev point))
  point)

(defun cursor-visual-bounds (cursor)
  (let* ((mark  (cursor-mark  cursor))
         (vmark (cursor-vmark cursor))
         (switched-p (mark< mark vmark))
         (left  (copy-point (if switched-p mark vmark)))
         (right (copy-point (if switched-p vmark mark))))
    (ecase (cursor-vtype cursor)
      (:point (point-next right))
      (:line
       (point-beginning-of-line left 1)
       (point-end-of-line right 1)))
    (values left right)))

(defun cursor-update-visual-range (cursor)
  (let ((region (cursor-vregion cursor)))
    (if (and (eql :visual (cursor-mode cursor))
             (cursor-vmark cursor))
        (multiple-value-bind (left right) (cursor-visual-bounds cursor)
          (when (eql :line (cursor-vtype cursor))
            (point-skip right))
          (unless (and region (point= left (region-left region)) (point= right (region-right region)))
            (when region
              (buffer-unpaint (cursor-buffer cursor) region))
            (setf (cursor-vregion cursor)
                  (unless (point= left right)
                    (buffer-paint (cursor-buffer cursor)
                                  :left-inserting  left
                                  :right-inserting right
                                  (list :background :bright-black :foreground :default :z z-visual-range))))))
        (when region
          (buffer-unpaint (cursor-buffer cursor) (cursor-vregion cursor))
          (setf (cursor-vregion cursor) nil)))))

(defun cursor-expand-region (cursor type left right)
  (setf (cursor-vtype cursor) type)
  (let ((mark (cursor-mark cursor))
        (vmark (cursor-vmark cursor)))
    (ecase type
      (:point
       (if (mark<= vmark mark)
           (progn
             (when (point< left vmark) (setf (cursor-vpoint cursor) left))
             (when (point< mark right) (setf (cursor-point-after cursor) right)))
           (progn
             (when (point< left mark) (setf (cursor-point cursor) left))
             (when (point< vmark right) (setf (cursor-vpoint-after cursor) right)))))
      (:line
       (setf (cursor-vpoint cursor)
             (point-beginning-of-line
              (if (mark<= vmark mark)
                  (if (< (point-line left) (point-line vmark)) left (copy-point vmark))
                  (if (< (point-line vmark) (point-line right)) right (copy-point vmark)))))
       (setf (cursor-point cursor)
             (point-beginning-of-line
              (if (mark<= vmark mark)
                  (if (< (point-line mark) (point-line right)) right (copy-point mark))
                  (if (< (point-line left) (point-line mark)) left (copy-point mark)))))))
    (setf (cursor-snapcol cursor) nil)))

(defmacro cursor-cmp-mark-vmark (cursor &body (case1 case2 case3))
  `(cond ((mark< (cursor-mark ,cursor) (cursor-vmark ,cursor))
          ,case1)
         ((mark= (cursor-vmark ,cursor) (cursor-mark ,cursor))
          ,case2)
         ((mark< (cursor-vmark ,cursor) (cursor-mark ,cursor))
          ,case3)
         (t (error "unreachable"))))

(defun char-eol-p (char)
  (or (null char) (char= char #\Newline)))

(defun char-space-p (char)
  (or (eql char #\Space) (eql char #\Tab)))

(defun point-eol-p (point)
  (char-eol-p (point-peek point)))

(defun point-bol-p (point)
  (char-eol-p (point-peek-prev point)))

(defun point-eob-p (point)
  (null (point-peek point)))

(defun point-bob-p (point)
  (null (point-peek-prev point)))

(defun cursor-clear-record (cursor)
  (setf (cursor-record-tmp cursor) nil
        (cursor-record cursor) nil))

(defun cursor-start-recording (cursor)
  (unless *no-record*
    (setf (cursor-record-tmp cursor) (make-record))))

(defun cursor-commit-record (cursor)
  (unless *no-record*
    (setf (cursor-record cursor) (cursor-record-tmp cursor)
          (cursor-record-tmp cursor) nil)))

(defun cursor-record-function (cursor function &rest args)
  (unless *no-record*
    (let ((record (cursor-record-tmp cursor)))
      (when record
        (push (list* function args) (record-commands record))))))

(defun cursor-record-simple (cursor function &rest args)
  (unless *no-record*
    (setf (cursor-record cursor) (make-record :commands (list (list* function args)))
          (cursor-record-tmp cursor) nil)))

(defun exit-visual-mode (cursor)
  (awhen (shiftf (cursor-prev-visual cursor) (list (make-mark-right-inserting (cursor-point cursor))
                                                   (make-mark-left-inserting (cursor-vpoint cursor))
                                                   (cursor-vtype cursor)))
    (delete-mark (first it))
    (delete-mark (second it)))
  (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
    (setf (cursor-named-mark cursor #\<) (values (make-mark-right-inserting p1) (cursor-buffer cursor))
          (cursor-named-mark cursor #\>) (values (make-mark-left-inserting p2) (cursor-buffer cursor))))
  (delete-mark (cursor-vmark cursor))
  (setf (cursor-vmark cursor) nil
        (cursor-venterpos cursor) nil))

(defun enter-insert-mode (cursor &optional change-p venterpos)
  (let ((buffer (cursor-buffer cursor))
        (venterpos (or venterpos (cursor-venterpos cursor))))
    (when (eql :visual (cursor-mode cursor))
      (exit-visual-mode cursor))
    (when (or change-p (not (buffer-variable buffer 'default-insert)))
      (open-undo-tree-group cursor :pos venterpos))
    (setf (cursor-mode cursor) :insert
          (cursor-venterpos cursor) nil)))

(defun exit-insert-mode (cursor point)
  (let ((buffer (cursor-buffer cursor)))
    (setf (cursor-named-mark cursor #\^) (values (make-mark-left-inserting point) buffer))
    (close-undo-tree-group cursor)))

(define-editor-command insert-mode
    (:cursor cursor :modes (:normal) :record (:normal :start-recording))
  (enter-insert-mode cursor)
  (setf (cursor-snapcol cursor) nil))

(define-editor-command previous-insert-mode
    (:cursor cursor :modes (:normal) :record (:normal :start-recording))
  (enter-insert-mode cursor)
  (aif (cursor-named-mark cursor #\^)
       (setf (cursor-point cursor) it
             (cursor-snapcol cursor) nil)
       (editor-error "No previous insert")))

(define-editor-command insert-mode-after
    (:cursor cursor :modes (:normal) :record (:normal :start-recording))
  (let ((point (cursor-point-after cursor)))
    (enter-insert-mode cursor)
    (setf (cursor-point cursor) point
          (cursor-snapcol cursor) nil)))

(macrolet ((def (name op)
             `(define-editor-command ,name
                  (:cursor cursor :modes (:normal) :record (:normal :start-recording))
                (enter-insert-mode cursor)
                (,op :cursor cursor))))
  (def insert-mode-beginning-of-line beginning-of-line)
  (def insert-mode-back-to-indentation back-to-indentation)
  (def insert-mode-end-of-line end-of-line))

(define-editor-command open-line-next
    (:cursor cursor :modes (:normal) :record (:normal :start-recording))
  (enter-insert-mode cursor)
  (with-marks ((end (point-end-of-line (cursor-point cursor)) :left-inserting))
    (with-undo-group (cursor :pos (point-pos end))
      (buffer-insert (cursor-buffer cursor) (copy-point end) (string #\Newline))
      (buffer-indent-at (cursor-buffer cursor) end)
      (setf (cursor-point cursor) (copy-point end))
      (setf (cursor-snapcol cursor) nil))))

(define-editor-command open-line-previous
    (:cursor cursor :modes (:normal) :record (:normal :start-recording))
  (enter-insert-mode cursor)
  (with-marks ((start (point-beginning-of-line (cursor-point cursor)) :right-inserting))
    (with-undo-group (cursor :pos (point-pos start))
      (buffer-insert (cursor-buffer cursor) (copy-point start) (string #\Newline))
      (buffer-indent-at (cursor-buffer cursor) start)
      (setf (cursor-point cursor) (point-end-of-line (copy-point start)))
      (setf (cursor-snapcol cursor) nil))))

(define-editor-command toggle-visual-mode
    (:cursor cursor :modes (:normal :visual) :mode mode :record (:normal :start-recording))
  (ecase mode
    (:normal
     (assert (null (cursor-vmark cursor)))
     (setf (cursor-mode cursor) :visual
           (cursor-vmark cursor) (copy-mark (cursor-mark cursor))
           (cursor-vtype cursor) :point
           (cursor-venterpos cursor) (cursor-pos cursor)))
    (:visual
     (ecase (cursor-vtype cursor)
       (:point (normal-mode :cursor cursor))
       (:line
        (setf (cursor-vtype cursor) :point)))))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command toggle-visual-line-mode
    (:cursor cursor :modes (:normal :visual) :mode mode :record (:normal :start-recording))
  (ecase mode
    (:normal
     (assert (null (cursor-vmark cursor)))
     (setf (cursor-mode cursor) :visual
           (cursor-vmark cursor) (copy-mark (cursor-mark cursor))
           (cursor-vtype cursor) :line
           (cursor-venterpos cursor) (cursor-pos cursor)))
    (:visual
     (ecase (cursor-vtype cursor)
       (:point
        (setf (cursor-vtype cursor) :line))
       (:line (normal-mode :cursor cursor)))))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command previous-visual-mode
    (:cursor cursor :modes (:normal) :record (:normal :start-recording))
  (assert (null (cursor-vmark cursor)))
  (aif (cursor-prev-visual cursor)
       (destructuring-bind (mark vmark vtype) it
         (setf (cursor-venterpos cursor) (cursor-pos cursor)
               (cursor-mode cursor) :visual
               (cursor-point cursor) mark
               (cursor-vmark cursor) (copy-mark vmark)
               (cursor-vtype cursor) vtype
               (cursor-snapcol cursor) nil))
       (editor-error "No previous selection")))

(define-editor-command normal-mode
    (:cursor cursor :modes (:visual :insert :operator :normal) :mode mode :record (:insert :commit-record))
  (ecase mode
    (:visual
     (exit-visual-mode cursor)
     (setf (cursor-mode cursor) :normal))
    (:insert
     (let ((point (cursor-point cursor)))
       (setf (cursor-mode cursor) :normal
             (cursor-point-after1 cursor) point)
       (exit-insert-mode cursor point)))
    (:operator
     (setf (cursor-mode cursor) :normal
           (cursor-venterpos cursor) nil))
    (:normal))
  (setf (cursor-snapcol cursor) nil))

(defun delete-buffer-from-global-marks (buffer marks)
  (declare (type global-marks marks))
  (loop for pair across (global-marks-named marks) and i from 0
        do (when (eql buffer (car pair))
             (delete-mark (cdr pair))
             (setf (aref (global-marks-named marks) i) nil)))
  (when (awhen (global-marks-jump marks) (eql buffer (car it)))
    (delete-mark (cdr (global-marks-jump marks)))
    (setf (global-marks-jump marks) nil)))

(defun named-mark-cursor-setter (name)
  (declare (type character name))
  (case name
    (#\> #'(setf cursor-point-after))
    (#\^ #'(setf cursor-point-after1))
    (t #'(setf cursor-point))))

;; TODO: implement #\[, #\], and #\. marks

(defun set-cursor-named-mark (cursor name mark &optional buffer)
  (declare (type cursor cursor) (type character name) (type mark mark) (type (or null buffer) buffer))
  (setf buffer (or buffer (cursor-buffer cursor)))
  (assert (eql (mark-text mark) (buffer-text buffer))
          nil "Mark not in buffer")
  (macrolet ((local-mark (accessor)
               `(progn
                  (assert (eql buffer (cursor-buffer cursor)))
                  (awhen ,accessor
                    (delete-mark it))
                  (setf ,accessor mark)))
             (global-mark (accessor)
               `(progn
                  (awhen ,accessor
                    (delete-mark (cdr it)))
                  (setf ,accessor (cons buffer mark)))))
    (case name
      (#\< (local-mark (local-marks-visual-start (cursor-marks cursor))))
      (#\> (local-mark (local-marks-visual-end (cursor-marks cursor))))
      (#\^ (local-mark (local-marks-insert (cursor-marks cursor))))
      ((#\` #\')
       (global-mark (global-marks-jump (marks))))
      (t
       (cond ((char<= #\a name #\z)
              (local-mark (aref (local-marks-named (cursor-marks cursor))
                                (- (char-code name) (char-code #\a)))))
             ((char<= #\A name #\Z)
              (global-mark (aref (global-marks-named (marks))
                                 (- (char-code name) (char-code #\A)))))
             (t (editor-error "Invalid mark: ~s" name)))))
    (values mark buffer)))

(defun cursor-named-mark (cursor name)
  (declare (type cursor cursor) (type character name))
  (macrolet ((local-mark (accessor)
               `(aif (,accessor (cursor-marks cursor))
                     (values it (cursor-buffer cursor))
                     (values nil nil)))
             (global-mark (accessor)
               `(aif (,accessor (marks))
                     (values (cdr it) (car it))
                     (values nil nil))))
    (case name
      (#\< (local-mark local-marks-visual-start))
      (#\> (local-mark local-marks-visual-end))
      (#\^ (local-mark local-marks-insert))
      ((#\` #\')
       (global-mark global-marks-jump))
      (t
       (cond ((char<= #\a name #\z)
              (local-mark (lambda (marks)
                            (aref (local-marks-named marks)
                                  (- (char-code name) (char-code #\a))))))
             ((char<= #\A name #\Z)
              (global-mark (lambda (marks)
                             (aref (global-marks-named marks)
                                   (- (char-code name) (char-code #\A))))))
             (t (editor-error "Invalid mark: ~s" name)))))))

(defun goto-mark-aux (cursor operator name mark buffer line-p alt-p)
  (unless mark
    (editor-error "Mark ~s is not set" name))
  (if operator
      (progn
        (unless (eql (cursor-buffer cursor) buffer)
          (editor-error "Mark not in current buffer"))
        (if line-p
            (multiple-value-bind (type p1 p2)
                (order-points :line (cursor-point cursor) (copy-point mark))
              (values type (point-beginning-of-line p1) (point-end-of-line p2)))
            (order-points :point (cursor-point cursor) (copy-point mark))))
      (progn
        (unless alt-p
          (window-set-jump (cursor-window cursor)))
        (switch-to-buffer buffer)
        (let ((cursor (current-cursor)))
          (if line-p
              (setf (cursor-point cursor) (point-back-to-indentation (copy-point mark)))
              (funcall (named-mark-cursor-setter name) (copy-point mark) cursor))
          (setf (cursor-snapcol cursor) nil)))))

(defun search-mark-aux (cursor n operator name direction)
  (let* ((line-p (ecase name (#\' t) (#\` nil)))
         (cmp (ecase direction (:forward #'point<) (:backward #'point>)))
         (point (cursor-point cursor))
         (marks (sort (loop for mark across (local-marks-named (cursor-marks cursor))
                            when (and mark (funcall cmp point mark))
                            collect mark)
                      cmp)))
    (unless (<= 1 n (length marks))
      (editor-error "No ~:r letter mark ~a in buffer" n (string-downcase (symbol-name direction))))
    (goto-mark-aux cursor operator name (nth (1- n) marks) (cursor-buffer cursor) line-p nil)))

(define-editor-command goto-mark-point
    (:cursor cursor :modes (:normal :visual) :operator operator
     :keys ((name (read-input))) :record (:visual :record))
  (multiple-value-bind (mark buffer) (cursor-named-mark cursor name)
    (goto-mark-aux cursor operator name mark buffer nil nil)))

(define-editor-command goto-mark-line
    (:cursor cursor :modes (:normal :visual) :operator operator
     :keys ((name (read-input))) :record (:visual :record))
  (multiple-value-bind (mark buffer) (cursor-named-mark cursor name)
    (goto-mark-aux cursor operator name mark buffer t nil)))

(define-editor-command goto-mark-point-aux
    (:cursor cursor :modes (:normal :visual) :operator operator
     :keys ((name (read-input))) :record (:visual :record))
  (multiple-value-bind (mark buffer) (cursor-named-mark cursor name)
    (goto-mark-aux cursor operator name mark buffer nil t)))

(define-editor-command goto-mark-line-aux
    (:cursor cursor :modes (:normal :visual) :operator operator
     :keys ((name (read-input))) :record (:visual :record))
  (multiple-value-bind (mark buffer) (cursor-named-mark cursor name)
    (goto-mark-aux cursor operator name mark buffer t t)))

(define-editor-command set-mark
    (:cursor cursor :modes (:normal :visual)
     :keys ((name (read-input))) :record (:visual :record))
  (setf (cursor-named-mark cursor name) (values (make-mark-left-inserting (cursor-point cursor)) (cursor-buffer cursor))))

(define-editor-command visual-swap-mark
    (:cursor cursor :modes (:visual) :record (:visual :record))
  (psetf (cursor-point cursor) (cursor-vpoint cursor)
         (cursor-vpoint cursor) (cursor-point cursor))
  (setf (cursor-snapcol cursor) nil))
