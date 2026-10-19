(in-package "FED")


(define-condition no-more-search-results (editor-error) ()
  (:report "No more search results"))

(defun point-search-forward (point string)
  (setf point (copy-point point))
  (loop (let ((tmp (copy-point point)))
          (when (loop for ch across string
                      always (eql (point-next tmp) ch))
            (return (values point tmp))))
        (unless (point-next point)
          (return (values nil nil)))))

(defun point-search-backward (point string)
  (setf point (copy-point point))
  (loop (let ((tmp (copy-point point)))
          (when (loop for ch across string
                      always (eql (point-next tmp) ch))
            (return (values point tmp))))
        (unless (point-prev point)
          (return (values nil nil)))))

(defun buffer-invalidate-search-results (buffer)
  (awhen (buffer-search-results buffer)
    (awhen (cdr it)
      (loop for entry across it
            do (when (third entry)
                 (buffer-unpaint buffer (third entry)))))
    (setf (cdr it) nil)))

(defun buffer-clear-search (buffer)
  (buffer-invalidate-search-results buffer)
  (setf (buffer-search-results buffer) nil))

(defun buffer-search-highlight (buffer string)
  (assert (not (string= "" string)))
  (buffer-clear-search buffer)
  (let ((point (text-pos-point (buffer-text buffer) 0))
        (results nil))
    (loop (multiple-value-bind (p1 p2) (point-search-forward point string)
            (unless p1 (return))
            (push (list (point-pos p1) (point-pos p2)
                        (buffer-paint buffer :left-inserting p1 :right-inserting p2
                                      (list :background :yellow :foreground :default :z z-search-highlight)))
                  results)
            (setf point p2)))
    (setf (buffer-search-results buffer) (cons string (apply #'vector (nreverse results))))))

(defun buffer-ensure-search-results (buffer)
  (awhen (buffer-search-results buffer)
    (unless (cdr it)
      (buffer-search-highlight buffer (car it)))
    it))

(defun result-idx-before (results pos)
  (labels ((aux (start end)
             (case (- end start)
               (0 nil)
               (1 (when (< (first (aref results start)) pos)
                    start))
               (t (let* ((mid (+ (truncate (- end start) 2) start))
                         (mid-pos (first (aref results mid))))
                    (if (< mid-pos pos)
                        (aux mid end)
                        (aux start mid)))))))
    (aux 0 (length results))))

(defun result-idx-after (results pos)
  (labels ((aux (start end)
             (case (- end start)
               (0 nil)
               (1 (when (< pos (first (aref results start)))
                    start))
               (t (let* ((mid (+ (truncate (1- (- end start)) 2) start))
                         (mid-pos (first (aref results mid))))
                    (if (<= mid-pos pos)
                        (aux (1+ mid) end)
                        (aux start (1+ mid))))))))
    (aux 0 (length results))))

(defun next-search-point (cursor n &key invert-p (forward-p nil directed-p) (from (cursor-point cursor)))
  (unless (cursor-prev-search cursor)
    (editor-error "No previous search"))
  (let ((string (first (cursor-prev-search cursor)))
        (results (buffer-ensure-search-results (cursor-buffer cursor))))
    (unless directed-p (setf forward-p (ecase (second (cursor-prev-search cursor)) (:after t) (:before nil))))
    (when invert-p (setf forward-p (not forward-p)))
    (if (and results (string= (car results) string))
        (let ((result-idx (funcall (if forward-p #'result-idx-after #'result-idx-before)
                                   (setf results (cdr results))
                                   (point-pos from))))
          (if result-idx
              (let ((text (buffer-text (cursor-buffer cursor)))
                    (entry (aref results (if forward-p
                                             (min (+ result-idx (1- n)) (1- (length results)))
                                             (max (- result-idx (1- n)) 0)))))
                (values (text-pos-point text (first entry)) (text-pos-point text (second entry))))
              (error 'no-more-search-results)))
        (let (ret1 ret2)
          (assert (< 0 n))
          (loop with point = (copy-point from)
                for i below n
                do (if (if forward-p (point-next point) (point-prev point))
                       (multiple-value-bind (p1 p2)
                           (funcall (if forward-p #'point-search-forward #'point-search-backward)
                                    point string)
                         (unless p1 (return))
                         (setf ret1 p1 ret2 p2)
                         (setf point (if forward-p p2 p1)))
                       (return)))
          (if (and ret1 ret2)
              (values ret1 ret2)
              (error 'no-more-search-results))))))

(defun cursor-next-search (cursor mode operator n &key invert-p (from (cursor-point cursor)))
  (let ((point (next-search-point cursor n :invert-p invert-p :from from)))
    (if operator
        (order-points :point from point)
        (ecase mode
          ((:normal :visual)
           (window-set-jump (cursor-window cursor))
           (setf (cursor-point cursor) point
                 (cursor-snapcol cursor) nil))))))

(defun cursor-around-search (cursor mode operator n &key invert-p)
  (unless (cursor-prev-search cursor)
    (editor-error "No previous search"))
  (assert (< 0 n))
  (let ((forward-p (ecase (second (cursor-prev-search cursor)) (:after t) (:before nil)))
        p1 p2 swapped-p)
    (when invert-p (setf forward-p (not forward-p)))
    (when (and (eql mode :visual)
               (or (and forward-p (point< (cursor-point cursor) (cursor-vpoint cursor)))
                   (and (not forward-p) (point< (cursor-vpoint cursor) (cursor-point cursor)))))
      (setf swapped-p t)
      (visual-swap-mark :cursor cursor))
    (handler-case
        (progn
          (setf (values p1 p2) (next-search-point cursor 1 :forward-p nil :from (cursor-point-after cursor)))
          (when (and (point<= p1 (cursor-point cursor)) (point< (cursor-point cursor) p2)
                     (not (and (eql :visual mode)
                               (multiple-value-bind (v1 v2) (cursor-visual-bounds cursor)
                                 (and (point<= v1 p1) (point<= p2 v2))))))
            (decf n 1)))
      (no-more-search-results ()))
    (unless (= n 0)
      (setf (values p1 p2) (unwind-protect
                               (multiple-value-prog1
                                   (next-search-point cursor n :invert-p invert-p)
                                 (setf swapped-p nil))
                             (when swapped-p
                               (visual-swap-mark :cursor cursor)))))
    (if operator
        (values :point p1 p2)
        (ecase mode
          (:normal
           (assert (null (cursor-vmark cursor)))
           (setf (cursor-venterpos cursor) (cursor-pos cursor)
                 (cursor-mode cursor) :visual
                 (cursor-vtype cursor) :point)
           (if forward-p
               (setf (cursor-point-after cursor) p2
                     (cursor-vpoint cursor) p1)
               (setf (cursor-point cursor) p1
                     (cursor-vpoint-after cursor) p2))
           (setf (cursor-snapcol cursor) nil))
          (:visual
           (cursor-expand-region cursor (cursor-vtype cursor) p1 p2))))))

(macrolet ((def (name type prompt)
             `(define-editor-command ,name
                  (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator
                   :minibuffer-keys ((,prompt string reason)) :record (:visual :record))
                (when (and (eql :accept reason) (string= "" string))
                  (if (cursor-prev-search cursor)
                      (setf string (first (cursor-prev-search cursor)))
                      (setf reason :close)))
                (ecase reason
                  (:accept
                   (setf (cursor-prev-search cursor) (list string ,type))
                   (unless operator
                     (buffer-search-highlight (cursor-buffer cursor) string))
                   (cursor-next-search cursor mode operator n))
                  (:close)
                  (:cancel
                   (editor-error "Search cancelled"))))))
  (def search-forward :after "/")
  (def search-backward :before "?"))

(define-editor-command next-search
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
  (cursor-next-search cursor mode operator n))

(define-editor-command previous-search
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
  (cursor-next-search cursor mode operator n :invert-p t))

(define-editor-command around-next-search
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
  (cursor-around-search cursor mode operator n))

(define-editor-command around-previous-search
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
  (cursor-around-search cursor mode operator n :invert-p t))

(define-editor-command clear-search
    (:cursor cursor :modes (:normal :visual) :record (:visual :record))
  (buffer-clear-search (cursor-buffer cursor)))

(macrolet ((def (name type)
             `(define-editor-command ,name
                  (:cursor cursor :modes (:normal :visual) :mode mode :n n :operator operator :record (:visual :record))
                (let ((p2 (point-forward-until (cursor-point cursor) #'point-at-end-identifier-p)))
                  (unless (point-at-end-identifier-p p2)
                    (editor-error "No next identifier"))
                  (let* ((p1 (point-backward-until (copy-point p2) #'point-at-begin-identifier-p))
                         (string (text-substring (buffer-text (cursor-buffer cursor)) (point-pos p1) (point-pos p2))))
                    (assert (not (string= string "")))
                    (setf (cursor-prev-search cursor) (list string ,type))
                    (cursor-next-search cursor mode operator n :from p1))))))
  ;; COMPAT: always takes the next identifier after the cursor, possibly skipping newlines
  ;; TODO: when regex is implemented, add word anchors \< and \> around identifier
  (def search-forward-next-identifier :after)
  (def search-backward-next-identifier :before))
