(in-package "FED")


(define-editor-command delete-forward-char
    (:cursor cursor :modes (:insert :normal) :mode mode :n n :register r :record (:insert :record :normal :record-simple)
     :keys ((merge (and (toplevel-p) (eql 'delete-forward-char *previous-binding*)))))
  (ecase mode
    (:insert
     (with-undo-group (cursor :merge merge)
       (let ((left (cursor-point cursor))
             (right (point-forward-char (cursor-point cursor) n)))
         (unless (point-peek left) (eob-error))
         (buffer-delete-between (cursor-buffer cursor) left right))))
    (:normal
     (with-undo-group cursor
       (let ((left (cursor-point cursor))
             (right (cursor-point cursor)))
         (loop repeat n until (point-eol-p right) do (point-skip right))
         (unless (point= left right)
           (buffer-delete-between-copy-register (cursor-buffer cursor) left right (or r #\-) :point))))))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command delete-backward-char
    (:cursor cursor :modes (:insert :normal) :mode mode :n n :register r :record (:insert :record :normal :record-simple)
     :keys ((merge (and (toplevel-p) (eql 'delete-backward-char *previous-binding*)))))
  (ecase mode
    (:insert
     (with-undo-group (cursor :merge merge)
       (let ((left (cursor-point cursor))
             (right (cursor-point cursor)))
         (loop repeat n while (point-peek-prev left)
               do (if (point< (point-back-to-indentation (copy-point left)) left)
                      (point-prev left)
                      (let* ((buffer (cursor-buffer cursor))
                             (text (buffer-text buffer))
                             (tabsize (buffer-tabsize buffer))
                             (x (coord-x (point-vcoord left))))
                        (if (= 0 x)
                            (point-prev left)
                            (setf left (text-vcoord-point text (coord (* (floor (1- x) tabsize) tabsize)
                                                                      (point-line left))))))))
         (unless (point-peek-prev right) (bob-error))
         (buffer-delete-between (cursor-buffer cursor) left right))))
    (:normal
     (with-undo-group cursor
       (let ((left (cursor-point cursor))
             (right (cursor-point cursor)))
         (loop repeat n until (point-bol-p left) do (point-skip-prev left))
         (unless (point= left right)
           (buffer-delete-between-copy-register (cursor-buffer cursor) left right (or r #\-) :point))))))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command transpose-char
    (:cursor cursor :modes (:insert) :n n :record (:insert :record))
  (let ((char (point-peek-prev (cursor-point cursor))))
    (unless char
      (bob-error))
    (unless (point-peek (cursor-point cursor))
      (eob-error))
    (with-undo-group cursor
      (delete-backward-char)
      (let ((point (point-forward-char (cursor-point cursor) n)))
        (setf (cursor-point cursor) point
              (cursor-snapcol cursor) nil)
        (buffer-insert (cursor-buffer cursor) point (string char))))))

(define-editor-command delete-forward-word
    (:cursor cursor :modes (:insert) :n n :register r :record (:insert :record)
     :keys ((repeat (and (toplevel-p) (eql *previous-binding* 'delete-forward-word)))
            (true-r (or r (when repeat (editor-variable 'previous-edit-register)) #\"))))
  (when (toplevel-p)
    (setf (editor-variable 'previous-edit-register) true-r))
  (with-undo-group cursor
    (let ((point (cursor-point cursor)))
      (loop repeat n while (point-peek point) do (point-forward-word-end point))
      (when (point= point (cursor-point cursor))
        (eob-error))
      (buffer-delete-between-copy-register (cursor-buffer cursor) (cursor-point cursor) point true-r :point (when repeat :after))
      (setf (cursor-snapcol cursor) nil))))

(define-editor-command delete-backward-word
    (:cursor cursor :modes (:insert) :n n :register r :record (:insert :record)
     :keys ((repeat (and (toplevel-p) (eql *previous-binding* 'delete-backward-word)))
            (true-r (or r (when repeat (editor-variable 'previous-edit-register)) #\"))))
  (when (toplevel-p)
    (setf (editor-variable 'previous-edit-register) true-r))
  (with-undo-group cursor
    (let ((point (cursor-point cursor)))
      (loop repeat n while (point-peek-prev point) do (point-backward-word point))
      (when (point= point (cursor-point cursor))
        (bob-error))
      (buffer-delete-between-copy-register (cursor-buffer cursor) point (cursor-point cursor) true-r :point (when repeat :before))
      (setf (cursor-snapcol cursor) nil))))

(defun buffer-delete-extra-newline (buffer point)
  (when (or (eql #\Newline (point-peek point)) (eql #\Newline (point-prev point)))
    (buffer-delete buffer point 1)))

(define-operator (delete-operator apply-delete-operator) (p1 p2 type)
    (:cursor cursor :register r :record t :venterpos venterpos)
  (with-undo-group (cursor :pos venterpos)
    (with-marks ((m1 p1))
      (buffer-delete-between-copy-register (cursor-buffer cursor) p1 p2 (or r #\") type)
      (when (eql :line type)
        (buffer-delete-extra-newline (cursor-buffer cursor) (copy-point m1))
        (move-mark m1 (point-back-to-indentation (copy-point m1))))
      (setf (cursor-point cursor) (copy-point m1)
            (cursor-snapcol cursor) nil))))

(define-editor-command delete-end-of-line
    (:cursor cursor :modes (:insert :normal) :mode mode :n n :register r :record (:insert :record :normal :record-simple)
     :keys ((repeat (and (toplevel-p) (eql *previous-binding* 'delete-end-of-line)))
            (true-r (or r (when repeat (editor-variable 'previous-edit-register)) #\"))))
  (ecase mode
    (:insert
     (when (toplevel-p)
       (setf (editor-variable 'previous-edit-register) true-r))
     (with-undo-group cursor
       (let ((left (cursor-point cursor))
             (right (point-end-of-line (cursor-point cursor) n)))
         (when (loop with p = (copy-point left)
                     until (point= p right)
                     always (char-space-p (point-next p)))
           (point-skip right))
         (buffer-delete-between-copy-register (cursor-buffer cursor) left right true-r :point (when repeat :after)))))
    (:normal
     (with-undo-group cursor
       (buffer-delete-between-copy-register (cursor-buffer cursor) (cursor-point cursor) (point-end-of-line (cursor-point cursor) n) (or r #\") :point))))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command delete-line
    (:cursor cursor :modes (:visual) :n n :register r :record (:visual :commit-record))
  (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
    (with-undo-group cursor
      (normal-mode :cursor cursor)
      (point-beginning-of-line p1)
      (point-end-of-line p2)
      (with-marks ((m1 p1))
        (buffer-delete-between-copy-register (cursor-buffer cursor) p1 p2 (or r #\") :line)
        (buffer-delete-extra-newline (cursor-buffer cursor) (copy-point m1))
        (move-mark m1 (point-back-to-indentation (copy-point m1)))
        (setf (cursor-point cursor) (copy-point m1)
              (cursor-snapcol cursor) nil)))))

(define-operator (change-operator apply-change-operator) (p1 p2 type)
    (:cursor cursor :register r :record :insert :venterpos venterpos)
  (enter-insert-mode cursor t venterpos)
  (setf (cursor-point cursor) p1)
  (with-undo-group cursor
    (buffer-delete-between-copy-register (cursor-buffer cursor) p1 p2 (or r #\") type)
    (when (eql :line type)
      (indent-line :cursor cursor)))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command change-end-of-line
    (:cursor cursor :modes (:normal) :n n :register r :record (:normal :start-recording))
  (enter-insert-mode cursor t)
  (with-undo-group cursor
    (delete-end-of-line :cursor cursor :n n :register r)))

(define-editor-command change-line
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :register r
     :record (:normal :start-recording :visual :record))
  (multiple-value-bind (p1 p2) (ecase mode
                                 (:normal (values (cursor-point cursor) (point-end-of-line (cursor-point cursor) n)))
                                 (:visual (cursor-visual-bounds cursor)))
    (enter-insert-mode cursor t)
    (with-undo-group cursor
      (buffer-delete-between-copy-register (cursor-buffer cursor)
                                           (point-beginning-of-line p1)
                                           (point-end-of-line p2)
                                           (or r #\") :line)
      (indent-line :cursor cursor)))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command change-char
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :register r
     :record (:normal :start-recording :visual :record))
  (ecase mode
    (:normal
     (enter-insert-mode cursor t)
     (with-undo-group cursor
       (let ((point (cursor-point cursor)))
         (loop repeat n until (point-eol-p point) do (point-skip point))
         (buffer-delete-between-copy-register (cursor-buffer cursor) (cursor-point cursor) point (or r #\-) :point))))
    (:visual
     (change-operator :cursor cursor)))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command repeat-edit
    (:cursor cursor :modes (:normal) :n n)
  ;; COMPAT: repeat always loops prefix-count times, rather than replacing the prefix-count on the corresponding text object
  (let ((record (cursor-record cursor))
        (*no-record* t))
    (unless record
      (editor-error "No command to repeat"))
    (unwind-protect
        (loop repeat n
              do (loop for (function . args) in (reverse (record-commands record))
                       do (apply function :cursor cursor args)))
      (unless (eql :normal (cursor-mode cursor))
        (normal-mode :cursor cursor)))))

(define-editor-command delete-indentation
    (:cursor cursor :modes (:insert) :n n :record (:insert :record)
     :keys ((merge (and (toplevel-p) (eql 'delete-indentation *previous-binding*)))))
  (with-undo-group (cursor :merge merge)
    (when (= 0 (cursor-line cursor))
      (bob-error))
    (loop repeat n
          do (beginning-of-line :cursor cursor)
          while (point-peek-prev (cursor-point cursor))
          do (delete-backward-char :cursor cursor)
             (clear-whitespace cursor)
             (unless (or (eql #\Newline (point-peek (cursor-point cursor)))
                         (eql #\Newline (point-peek-prev (cursor-point cursor)))
                         (position (point-peek (cursor-point cursor)) *close-delimiters*)
                         (position (point-peek-prev (cursor-point cursor)) *open-delimiters*))
               (self-insert :cursor cursor :key #\Space)
               (backward-char :cursor cursor)))))

(defun join-lines-aux (cursor mode n add-space-p)
  ;; COMPAT: joins n lines down, rather than n lines total
  ;; COMPAT: whitespace at end of line gets deleted the same way that indentation does
  (with-undo-group cursor
    (when (eql :visual mode)
      (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
        (when (eql #\Newline (point-peek-prev p2))
          (point-skip-prev p2))
        (setf n (max (- (point-line p2) (point-line p1)) 1))
        (setf (cursor-point cursor) p1)
        (normal-mode :cursor cursor)))
    (with-cursor-properties (cursor :mark-type :right-inserting)
      (let ((point (cursor-point cursor))
            (first-p t))
        (loop repeat n
              while (point-peek (point-end-of-line point))
              do (with-marks ((mark point :right-inserting))
                   (let ((left (point-backward-until-maybe (copy-point point) #'point-not-at-space-prev-p))
                         (right (progn (point-next point)
                                       (point-forward-until-maybe point #'point-not-at-space-p))))
                     (buffer-delete-between (cursor-buffer cursor) left right)
                     (when (and add-space-p
                                (not (position (point-peek mark) *close-delimiters*))
                                (not (position (point-peek-prev mark) *open-delimiters*)))
                       (buffer-insert (cursor-buffer cursor) mark " "))
                     (setf point (copy-point mark))))
                 (setf first-p nil))
        (when first-p
          (eob-error))
        (setf (cursor-point cursor) point
              (cursor-snapcol cursor) nil)))))

(define-editor-command join-lines-next
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :record (:visual :commit-record :normal :record-simple))
  (join-lines-aux cursor mode n t))

(define-editor-command join-lines-next-alt
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :record (:visual :commit-record :normal :record-simple))
  (join-lines-aux cursor mode n nil))

(define-editor-command replace-char
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :record (:normal :record-simple :visual :commit-record)
     :keys ((char (read-input))))
  (flet ((string-replace (s)
           (map 'string (lambda (ch) (if (eql #\Newline ch) #\Newline char)) s)))
    (ecase mode
      (:normal
       (with-undo-group cursor
         (let ((left (cursor-point cursor))
               (right (cursor-point cursor)))
           (loop repeat n do (when (point-eol-p right) (eol-error)) (point-skip right))
           (with-marks ((end-mark right :left-inserting))
             (buffer-replace (cursor-buffer cursor) left right #'string-replace)
             (setf (cursor-point-after cursor) end-mark)))))
      (:visual
       (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
         (with-undo-group cursor
           (normal-mode :cursor cursor)
           (with-marks ((start-mark p1 :right-inserting))
             (buffer-replace (cursor-buffer cursor) p1 p2  #'string-replace)
             (setf (cursor-point cursor) start-mark))))))
    (setf (cursor-snapcol cursor) nil)))

(define-editor-command swap-case
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :record (:normal :record-simple :visual :commit-record))
  (ecase mode
    (:normal
     (with-undo-group cursor
       (let ((left (cursor-point cursor))
             (right (cursor-point cursor)))
         (loop repeat n until (point-eol-p right) do (point-skip right))
         (with-marks ((end-mark right :left-inserting))
           (buffer-replace (cursor-buffer cursor) left right #'string-invcase)
           (setf (cursor-point cursor) end-mark)))))
    (:visual
     (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
       (with-undo-group cursor
         (normal-mode :cursor cursor)
         (with-marks ((start-mark p1 :right-inserting))
           (buffer-replace (cursor-buffer cursor) p1 p2 #'string-invcase)
           (setf (cursor-point cursor) start-mark))))))
  (setf (cursor-snapcol cursor) nil))

(macrolet ((def (name apply-name function)
             `(define-operator (,name ,apply-name) (p1 p2 type)
                  (:cursor cursor :record t :venterpos venterpos)
                (with-marks ((m1 p1))
                  (with-undo-group (cursor :pos venterpos)
                    (buffer-replace (cursor-buffer cursor) p1 p2 #',function)
                    (setf (cursor-point cursor) (ecase type (:line (point-back-to-indentation (copy-point m1))) (:point m1))
                          (cursor-snapcol cursor) nil))))))
  (def invcase-operator  apply-invcase-operator  string-invcase)
  (def upcase-operator   apply-upcase-operator   string-upcase)
  (def downcase-operator apply-downcase-operator string-downcase))

(defun point-forward-until-alphanumeric-maybe (point)
  (point-forward-until-maybe
   point
   (lambda (point) (awhen (point-peek point) (alphanumericp it)))))

(defun point-forward-until-not-alphanumeric-maybe (point)
  (point-forward-until-maybe
   point
   (lambda (point) (aif (point-peek point) (not (alphanumericp it)) t))))

(macrolet ((def (name function)
             `(define-editor-command ,name
                  (:cursor cursor :modes (:insert) :n n :record (:insert :record))
                (with-undo-group cursor
                  (loop repeat n
                        with first = t
                        for point = (point-forward-until-alphanumeric-maybe (cursor-point cursor))
                        for next-point = (point-forward-until-not-alphanumeric-maybe (copy-point point))
                        until (point= point next-point)
                        do (setf first nil)
                           (setf (cursor-point cursor) next-point
                                 (cursor-snapcol cursor) nil)
                           (buffer-replace (cursor-buffer cursor) point next-point #',function)
                        finally (when first
                                  (eob-error)))))))
  (def capitalise-word string-capitalize)
  (def upcase-word string-upcase)
  (def downcase-word string-downcase))

(defun transpose-aux (cursor n points-fn)
  (with-undo-group cursor
    (loop with w1 and w2 and t1 and t2
          with first = t
          repeat n
          do (setf (values w1 w2 t1 t2) (funcall points-fn (cursor-point cursor)))
          until (point= t1 t2)
          do (setf first nil)
             (with-marks ((mw1 w1) (mw2 w2) (mt1 t1) (mt2 t2))
               (let* ((buffer (cursor-buffer cursor))
                      (s1 (text-substring (buffer-text buffer) (point-pos mw1) (point-pos mw2)))
                      (s2 (text-substring (buffer-text buffer) (point-pos mt1) (point-pos mt2))))
                 (buffer-delete-between buffer mw1 mw2)
                 (buffer-delete-between buffer mt1 mt2)
                 (buffer-insert buffer mw1 s2)
                 (buffer-insert buffer mt1 s1))
               (setf (cursor-point cursor) mt2
                     (cursor-snapcol cursor) nil))
          finally (when first
                    (editor-error "Can not transpose further")))))

(define-editor-command transpose-words
    (:cursor cursor :modes (:insert) :n n :record (:insert :record))
  (transpose-aux
   cursor
   n
   (lambda (point)
     (let* ((w1 (point-backward-until point
                                      (lambda (point)
                                        (when (and (point-peek point) (point-peek-prev point))
                                          (and (alphanumericp (point-peek point))
                                               (not (alphanumericp (point-peek-prev point))))))))
            (w2 (point-forward-until-not-alphanumeric-maybe (copy-point w1)))
            (t1 (point-forward-until-alphanumeric-maybe (copy-point w2)))
            (t2 (point-forward-until-not-alphanumeric-maybe (copy-point t1))))
       (when (point= w1 w2)
         (bob-error))
       (values w1 w2 t1 t2)))))

(defun clear-whitespace (cursor &optional (point (cursor-point cursor)))
  (let ((left (point-backward-until-maybe (copy-point point) #'point-not-at-space-prev-p))
        (right (point-forward-until-maybe (copy-point point) #'point-not-at-space-p)))
    (buffer-delete-between (cursor-buffer cursor) left right)))

(define-editor-command delete-horizontal-space
    (:cursor cursor :modes (:insert) :record (:insert :record))
  (with-undo-group cursor
    (clear-whitespace cursor))
  (setf (cursor-snapcol cursor) nil))
