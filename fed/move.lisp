(in-package "FED")


(defmacro with-move-or-error ((cursor error n) &body body)
  (let ((cursor-sym (gensym "CURSOR"))
        (n-sym (gensym "N"))
        (point-sym (gensym "POINT")))
    `(let ((,cursor-sym ,cursor)
           (,n-sym ,n)
           (,point-sym (cursor-point ,cursor)))
       (multiple-value-prog1 (progn ,@body)
         (when (and (= 1 ,n-sym) (point= ,point-sym (cursor-point ,cursor-sym)))
           ,error)))))

(define-editor-command forward-char
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  (if operator
      (let ((point (cursor-point cursor)))
        (loop repeat n until (point-eol-p point) do (point-skip point))
        (values :point (cursor-point cursor) point))
      (ecase mode
        (:insert
         (with-move-or-error (cursor (eob-error) n)
           (let ((point (cursor-point cursor)))
             (loop repeat n while (point-next point))
             (setf (cursor-point cursor) point
                   (cursor-snapcol cursor) nil))))
        ((:normal :visual)
         (with-move-or-error (cursor (eol-error) n)
           (let ((point (cursor-point cursor)))
             (loop repeat n until (point-eol-p point) do (point-skip point))
             (setf (cursor-point cursor) point
                   (cursor-snapcol cursor) nil)))))))

(define-editor-command backward-char
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  (if operator
      (let ((point (cursor-point cursor)))
        (loop repeat n until (point-bol-p point) do (point-skip-prev point))
        (values :point point (cursor-point cursor)))
      (ecase mode
        (:insert
         (with-move-or-error (cursor (bob-error) n)
           (let ((point (cursor-point cursor)))
             (loop repeat n while (point-prev point))
             (setf (cursor-point cursor) point
                   (cursor-snapcol cursor) nil))))
        ((:normal :visual)
         (with-move-or-error (cursor (bol-error) n)
           (let ((point (cursor-point cursor)))
             (loop repeat n until (point-bol-p point) do (point-skip-prev point))
             (setf (cursor-point cursor) point
                   (cursor-snapcol cursor) nil)))))))

(define-editor-command beginning-of-line
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  (if operator
      (values :point (point-beginning-of-line (cursor-point cursor) n) (cursor-point cursor))
      (ecase mode
        ((:insert :normal :visual)
         (setf (cursor-point cursor) (point-beginning-of-line (cursor-point cursor) n)
               (cursor-snapcol cursor) nil)))))

(define-editor-command back-to-indentation
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  ;; COMPAT: takes a count prefix
  (if operator
      (order-points :point (point-back-to-indentation (cursor-point cursor) n) (cursor-point cursor))
      (ecase mode
        ((:insert :normal :visual)
         (setf (cursor-point cursor) (point-back-to-indentation (cursor-point cursor) n)
               (cursor-snapcol cursor) nil)))))

(define-editor-command end-of-line
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  (if operator
      (values :point (cursor-point cursor) (point-end-of-line (cursor-point cursor) n))
      (ecase mode
        ((:insert :normal :visual)
         (setf (cursor-point cursor) (point-end-of-line (cursor-point cursor) n)
               (cursor-snapcol cursor) :eol)))))

(macrolet ((def (name point-move-name forward-p after-p boundary &key exception1 exception2)
             `(progn
                (defun ,point-move-name (point &key (n 1))
                  (loop repeat n
                        do (multiple-value-bind (p eob)
                               (,(if forward-p 'point-forward-until 'point-backward-until) point #',boundary)
                             (declare (ignore p))
                             (when eob (return (values point t))))
                        finally (return (values point nil))))
                (define-editor-command ,name
                    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator
                     :record (:insert :record :visual :record))
                  (if operator
                      (let ((point (cursor-point cursor)))
                        ,(let ((standard-form (or exception1 `(,point-move-name point :n n))))
                           (if exception2
                               `(if (eql 'apply-change-operator (first operator))
                                    (point-forward-word-end point :n n)
                                    ,standard-form)
                               standard-form))
                        (when (point= point (cursor-point cursor))
                          ,(if forward-p '(eob-error) '(bob-error)))
                        (order-points :point (cursor-point cursor) point))
                      (ecase mode
                        ((:insert :normal :visual)
                         (let ((point (,(if after-p 'cursor-point-after 'cursor-point) cursor)))
                           ,(if forward-p
                                `(unless (point-peek (cursor-point-after cursor))
                                   (eob-error))
                                `(unless (point-peek-prev (cursor-point cursor))
                                   (bob-error)))
                           ,(if (and forward-p after-p)
                                `(if (nth-value 1 (,point-move-name point :n n))
                                     (setf (cursor-point cursor) point)
                                     (setf (cursor-point-after cursor) point))
                                `(setf (,(if after-p 'cursor-point-after 'cursor-point) cursor) (,point-move-name point :n n)
                                       (cursor-snapcol cursor) nil))))))))))
  ;; COMPAT: empty lines are not treated as word-boundaries
  ;; COMPAT: `ge' and `gE' text objects do not include boundaries, for better consistency with rest of family
  (def forward-word           point-forward-word           t   nil point-at-begin-word-p
    :exception1 (loop repeat n
                      do (point-forward-until point (lambda (point)
                                                      (or (point-at-begin-word-p point)
                                                          (eql #\Newline (point-peek point))))))
    :exception2 t)
  (def backward-word          point-backward-word          nil nil point-at-begin-word-p)
  (def forward-word-end       point-forward-word-end       t   t   point-at-end-word-p)
  (def backward-word-end      point-backward-word-end      nil t   point-at-end-word-p)
  (def forward-word-full      point-forward-word-full      t   nil point-at-begin-word-full-p)
  (def backward-word-full     point-backward-word-full     nil nil point-at-begin-word-full-p)
  (def forward-word-end-full  point-forward-word-end-full  t   t   point-at-end-word-full-p)
  (def backward-word-end-full point-backward-word-end-full nil t   point-at-end-word-full-p))

(define-editor-command previous-line
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  (if operator
      (let ((buffer (cursor-buffer cursor))
            (line (cursor-line cursor)))
        (when (= 0 line)
          (bob-error))
        (select-lines buffer (max (- line n) 0) line))
      (with-move-or-error (cursor (bob-error) n)
        (let ((line (point-line (cursor-point cursor)))
              (snapcol (cursor-snapcol cursor)))
          (psetf (cursor-vcoord cursor) (coord snapcol (max (- line n) 0))
                 (cursor-snapcol cursor) (cursor-snapcol cursor))))))

(define-editor-command next-line
    (:cursor cursor :modes (:insert :normal :visual) :mode mode :n n :operator operator :record (:insert :record :visual :record))
  (if operator
      (let* ((buffer (cursor-buffer cursor))
             (line (cursor-line cursor)))
        (when (= (1- (buffer-nlines buffer)) line)
          (eob-error))
        (select-lines buffer line (+ line n)))
      (with-move-or-error (cursor (eob-error) n)
        (let ((line (point-line (cursor-point cursor)))
              (snapcol (cursor-snapcol cursor))
              (last-line (1- (buffer-nlines (cursor-buffer cursor)))))
          (psetf (cursor-vcoord cursor) (coord snapcol (min (+ line n) last-line))
                 (cursor-snapcol cursor) (cursor-snapcol cursor))))))

(define-editor-command goto-column
    (:cursor cursor :modes (:normal :visual) :n n :operator operator :record (:visual :record))
  (let ((vcoord (coord (1- n) (coord-y (cursor-vcoord cursor)))))
    (if operator
        (order-points :point
                      (cursor-point cursor)
                      (text-vcoord-point (buffer-text (cursor-buffer cursor)) vcoord))
        (setf (cursor-vcoord cursor) vcoord
              (cursor-snapcol cursor) nil))))

(define-editor-command goto-line-middle
    (:cursor cursor :modes (:normal :visual) :n n :n-nullable-p t :operator operator :record (:visual :record))
  (setf n (or n 50))
  (unless (<= 1 n 100)
    (editor-error "Invalid percentage %~d" n))
  (let ((vcoord (coord (truncate (* n (coord-x (point-vcoord (point-end-of-line (cursor-point cursor))))) 100)
                       (coord-y (cursor-vcoord cursor)))))
    (if operator
        (order-points :point
                      (cursor-point cursor)
                      (text-vcoord-point (buffer-text (cursor-buffer cursor)) vcoord))
        (setf (cursor-vcoord cursor) vcoord
              (cursor-snapcol cursor) nil))))

(define-editor-command goto-offset
    (:cursor cursor :modes (:normal :visual) :n n :operator operator :record (:visual :record))
  (if operator
      (order-points :point
                    (cursor-point cursor)
                    (text-pos-point (buffer-text (cursor-buffer cursor)) (1- n)))
      (setf (cursor-pos cursor) (1- n)
            (cursor-snapcol cursor) nil)))

(macrolet ((def-around (name point-begin-p point-end-p)
             `(define-text-object ,name
                  (:cursor cursor :mode mode :n n :operator operator :record (:visual :record))
                (flet ((right-text-object (left indent-special-case-p)
                         (let ((right (copy-point left)))
                           (if (eql :whitespace (char-syntax-type (point-peek right)))
                               (progn
                                 (point-backward-until-maybe left #'point-not-at-space-prev-p)
                                 (point-forward-until right #',point-begin-p)
                                 (unless (point-peek right) (eob-error))
                                 (loop repeat n do (point-forward-until right #',point-end-p))
                                 (values :point left right))
                               (progn
                                 (unless (point-peek right) (eob-error))
                                 (point-backward-until-maybe left #',point-begin-p)
                                 (loop repeat n do (point-forward-until right #',point-end-p))
                                 (if (char-space-p (point-peek right))
                                     (point-forward-until right #'point-not-at-space-p)
                                     (let ((point (copy-point left)))
                                       (point-backward-until-maybe point #'point-not-at-space-prev-p)
                                       (unless (and indent-special-case-p (point-bol-p point))
                                         (setf left point))))
                                 (values :point left right)))))
                       (left-text-object (right)
                         (let ((left  (copy-point right)))
                           (if (eql :whitespace (char-syntax-type (point-peek-prev left)))
                               (progn
                                 (point-forward-until-maybe right #'point-not-at-space-p)
                                 (point-backward-until left #',point-end-p)
                                 (unless (point-peek-prev left) (bob-error))
                                 (loop repeat n do (point-backward-until left #',point-begin-p))
                                 (values :point left right))
                               (progn
                                 (unless (point-peek-prev left) (bob-error))
                                 (point-forward-until-maybe right #',point-end-p)
                                 (loop repeat n do (point-backward-until left #',point-begin-p))
                                 (if (char-space-p (point-peek-prev left))
                                     (point-backward-until left #'point-not-at-space-prev-p)
                                     (point-forward-until-maybe right #'point-not-at-space-p))
                                 (values :point left right))))))
                  (if operator
                      (right-text-object (cursor-point cursor) t)
                      (ecase mode
                        (:visual
                         (cursor-cmp-mark-vmark cursor
                           (left-text-object (cursor-point cursor))
                           (right-text-object (cursor-point cursor) t)
                           (right-text-object (cursor-point-after cursor) nil))))))))
           (def-inside (name point-at-begin-p point-at-end-p)
             `(define-text-object ,name
                  (:cursor cursor :mode mode :n n :operator operator :record (:visual :record))
                (flet ((right-text-object (left)
                         (let* ((right (copy-point left))
                                (char (point-peek left)))
                           (cond ((char-eol-p char)
                                  (loop while (eql #\Newline (point-peek right)) do (point-skip right))
                                  (unless (point-peek right)
                                    (eob-error)))
                                 ((char-space-p char)
                                  (point-backward-until-maybe left #'point-not-at-space-prev-p))
                                 (t
                                  (point-backward-until-maybe left #',point-at-begin-p)))
                           (loop repeat n
                                 do (point-forward-until right (lambda (point)
                                                                 (or (,point-at-begin-p point)
                                                                     (,point-at-end-p point)
                                                                     (and (char-space-p (point-peek-prev point))
                                                                          (point-eol-p point))))))
                           (values :point left right)))
                       (left-text-object (right)
                         (let* ((left (copy-point right))
                                (char (point-peek-prev right)))
                           (cond ((char-eol-p char)
                                  (loop while (eql #\Newline (point-peek-prev left)) do (point-skip-prev left))
                                  (unless (point-peek-prev left)
                                    (bob-error)))
                                 ((char-space-p char)
                                  (point-forward-until-maybe right #'point-not-at-space-p))
                                 (t
                                  (point-forward-until-maybe right #',point-at-end-p)))
                           (loop repeat n
                                 do (point-backward-until left (lambda (point)
                                                                 (or (,point-at-begin-p point)
                                                                     (,point-at-end-p point)
                                                                     (and (char-space-p (point-peek point))
                                                                          (point-bol-p point))))))
                           (values :point left right))))
                  (if operator
                      (right-text-object (cursor-point cursor))
                      (ecase mode
                        (:visual
                         (cursor-cmp-mark-vmark cursor
                           (left-text-object (cursor-point cursor))
                           (right-text-object (cursor-point cursor))
                           (right-text-object (cursor-point-after cursor))))))))))
  ;; COMPAT: empty lines are not treated as word-boundaries
  ;; COMPAT: if a partial word is selected (more than one char) the whole word is selected, rather than selecting just one side
  ;; COMPAT: `inside-word' on empty lines will select the next non-empty valid word region, in visual mode, `inside-word' skips newlines if starting on one
  (def-around around-word      point-at-begin-word-p      point-at-end-word-p)
  (def-around around-word-full point-at-begin-word-full-p point-at-end-word-full-p)
  (def-inside inside-word      point-at-begin-word-p      point-at-end-word-p)
  (def-inside inside-word-full point-at-begin-word-full-p point-at-end-word-full-p))

(macrolet ((def (name type direction)
             (declare (type (member :find :till) type)
                      (type (member :forward :backward) direction))
             (let ((cursor-point (if (or (and (eql :find type) (eql :forward direction))
                                         (and (eql :till type) (eql :backward direction)))
                                     'cursor-point
                                     'cursor-point-after)))
               `(define-editor-command ,name
                    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator
                     :keys ((char (read-input)) (set-direction-p t)) :record (:visual :record))
                  (let ((point (,cursor-point cursor)))
                    (when set-direction-p
                      (setf (cursor-prev-find/till cursor) (list ',type ',direction char)))
                    (loop repeat n
                          do (,(ecase direction (:forward 'point-forward-until) (:backward 'point-backward-until))
                              point
                              (lambda (point)
                                (let ((ch (,(ecase direction (:forward 'point-peek) (:backward 'point-peek-prev)) point)))
                                  (when (char-eol-p ch) (editor-error "No ~:r character ~c found in line" n char))
                                  (eql char ch)))))
                    (if operator
                        (progn
                          ,@(when (eql :find type)
                              (ecase direction
                                (:forward `((point-skip point)))
                                (:backward `((point-skip-prev point)))))
                          ,(ecase direction
                             (:forward `(values :point (cursor-point cursor) point))
                             (:backward `(values :point point (cursor-point cursor)))))
                        (ecase mode
                          ((:normal :visual)
                           (setf (,cursor-point cursor) point
                                 (cursor-snapcol cursor) nil)))))))))
  ;; COMPAT: the `*-till-*' commands do not stay in place when already at a valid location (same when used as text-object)
  (def forward-find-char :find :forward)
  (def forward-till-char :till :forward)
  (def backward-find-char :find :backward)
  (def backward-till-char :till :backward))

(define-editor-command repeat-find/till
    (:cursor cursor :modes (:normal :visual) :n n :operator operator :record (:visual :record))
  (aif (cursor-prev-find/till cursor)
       (destructuring-bind (type direction char) it
         (ecase type
           (:find
            (ecase direction
              (:forward (forward-find-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))
              (:backward (backward-find-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))))
           (:till
            (ecase direction
              (:forward (forward-till-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))
              (:backward (backward-till-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))))))
       (editor-error "No previous find")))

(define-editor-command repeat-find/till-reverse
    (:cursor cursor :modes (:normal :visual) :n n :operator operator :record (:visual :record))
  (aif (cursor-prev-find/till cursor)
       (destructuring-bind (type direction char) it
         (ecase type
           (:find
            (ecase direction
              (:forward (backward-find-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))
              (:backward (forward-find-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))))
           (:till
            (ecase direction
              (:forward (backward-till-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))
              (:backward (forward-till-char :cursor cursor :n n :operator operator :char char :set-direction-p nil))))))
       (editor-error "No previous find")))

(define-editor-command beginning-of-buffer
    (:cursor cursor :modes (:normal :visual) :operator operator :n n :record (:visual :record))
  (if operator
      (select-lines (cursor-buffer cursor) (cursor-line cursor) (1- n))
      (progn
        (window-set-jump (cursor-window cursor))
        (setf (cursor-vcoord cursor) (coord 0 (1- n)))
        (back-to-indentation :cursor cursor))))

(define-editor-command end-of-buffer
    (:cursor cursor :modes (:normal :visual) :operator operator :record (:visual :record))
  (let ((line (1- (buffer-nlines (cursor-buffer cursor)))))
    (if operator
        (select-lines (cursor-buffer cursor) (cursor-line cursor) line)
        (progn
          (window-set-jump (cursor-window cursor))
          (setf (cursor-pos cursor) -1)
          (back-to-indentation :cursor cursor)))))

(define-editor-command forward-paragraph
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
  (if operator
      (let ((end-point (point-forward-paragraph (cursor-point cursor) :n n)))
        (when (and (eql #\Newline (point-peek-prev end-point)) (point< (cursor-point cursor) end-point))
          (point-skip-prev end-point))
        (if (loop with point = (cursor-point cursor) until (point-bol-p point) always (char-space-p (point-prev point)))
            (values :line (point-beginning-of-line (cursor-point cursor)) end-point)
            (values :point (cursor-point cursor) end-point)))
      (setf (cursor-point cursor) (prog1 (point-forward-paragraph (cursor-point cursor) :n n)
                                    (window-set-jump (cursor-window cursor)))
            (cursor-snapcol cursor) nil)))

(define-editor-command backward-paragraph
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
  (if operator
      (if (point-bol-p (cursor-point cursor))
          (values :line (point-backward-paragraph (cursor-point cursor) :n n) (point-skip-prev (cursor-point cursor)))
          (values :point (point-backward-paragraph (cursor-point cursor) :n n) (cursor-point cursor)))
      (setf (cursor-point cursor) (prog1 (point-backward-paragraph (cursor-point cursor) :n n)
                                    (window-set-jump (cursor-window cursor)))
            (cursor-snapcol cursor) nil)))

(define-text-object in-paragraph
    (:cursor cursor :mode mode :n n :operator operator :record (:visual :record))
  (if operator
      (point-select-paragraphs-down (cursor-point cursor) :n n)
      (ecase mode
        (:visual
         (cursor-cmp-mark-vmark cursor
           (let ((point (cursor-point cursor)))
             (point-beginning-of-line point)
             (unless (point-peek-prev point)
               (bob-error))
             (point-beginning-of-line point 2)
             (point-select-paragraphs-up point :n n))
           (point-select-paragraphs-down (cursor-point cursor) :n n)
           (let ((point (cursor-point cursor)))
             (point-end-of-line point)
             (unless (point-peek point)
               (eob-error))
             (point-end-of-line point 2)
             (point-select-paragraphs-down point :n n)))))))

(define-text-object around-paragraph
    (:cursor cursor :mode mode :n n :operator operator :record (:visual :record))
  (if operator
      (point-select-around-paragraphs-down (cursor-point cursor) :n n)
      (ecase mode
        (:visual
         (cursor-cmp-mark-vmark cursor
           (let ((point (cursor-point cursor)))
             (point-beginning-of-line point)
             (unless (point-peek-prev point)
               (bob-error))
             (point-beginning-of-line point 2)
             (point-select-around-paragraphs-up point :n n))
           (point-select-around-paragraphs-down (cursor-point cursor) :n n)
           (let ((point (cursor-point cursor)))
             (point-end-of-line point)
             (unless (point-peek point)
               (eob-error))
             (point-end-of-line point 2)
             (point-select-around-paragraphs-down point :n n)))))))
