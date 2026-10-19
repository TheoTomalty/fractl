(in-package "FED")


(defvar *digraphs* (make-array 128 :initial-element nil))

(defun valid-digraph-p (digraph)
  (and (stringp digraph)
       (= 2 (length digraph))
       (< (char-code (aref digraph 0)) 128)
       (< (char-code (aref digraph 1)) 128)))

(defun digraph (digraph)
  (declare (type string digraph))
  (assert (valid-digraph-p digraph) nil "Invalid digraph ~s" digraph)
  (awhen (aref *digraphs* (char-code (aref digraph 0)))
    (aref it (char-code (aref digraph 1)))))

(defun (setf digraph) (new digraph)
  (declare (type string digraph) (type character new))
  (assert (valid-digraph-p digraph) nil "Invalid digraph ~s" digraph)
  (let ((c1 (char-code (aref digraph 0)))
        (c2 (char-code (aref digraph 1))))
    (unless (aref *digraphs* c1)
      (setf (aref *digraphs* c1) (make-array 128 :initial-element nil)))
    (setf (aref (aref *digraphs* c1) c2) new)))

(defun save-digraphs-file (filename)
  (with-open-file (s filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (let ((pairs (loop for tbl across *digraphs* and c1 from 0
                       when tbl
                       nconc (loop for ch across tbl and c2 from 0
                                   when ch
                                   collect (list ch c1 c2)))))
      (setf pairs (sort pairs #'char< :key #'first))
      (loop for (ch c1 c2) in pairs
            do (format s "~c	~c~c~%" ch (code-char c1) (code-char c2))))))

(defun load-digraphs-file (filename)
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil)
          while line
          do (assert (char= #\Tab (aref line 1)))
             (setf (digraph (subseq line 2)) (aref line 0)))))

(defparameter *insert-commands* '(self-insert quoted-insert insert-digraph))

(defun prev-binding-is-insert-p ()
  (and (member *previous-binding* *insert-commands*) t))

(define-editor-command self-insert
    (:cursor cursor :modes (:insert) :n n :record (:insert :record)
     :keys ((key *current-key*) (merge (and (toplevel-p) (prev-binding-is-insert-p)))))
  (with-undo-group (cursor :merge merge)
    (buffer-insert (cursor-buffer cursor) (cursor-point cursor) (make-string n :initial-element key)))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command quoted-insert
    (:cursor cursor :modes (:insert) :n n :record (:insert :record)
     :keys ((key (read-input)) (merge (and (toplevel-p) (prev-binding-is-insert-p)))))
  (with-undo-group (cursor :merge merge)
    (buffer-insert (cursor-buffer cursor) (cursor-point cursor) (make-string n :initial-element key)))
  (setf (cursor-snapcol cursor) nil))

(define-editor-command insert-digraph
    (:cursor cursor :modes (:insert) :n n :record (:insert :record)
     :keys ((c1 (read-input)) (c2 (read-input)) (merge (and (toplevel-p) (prev-binding-is-insert-p)))))
  (let ((digraph (make-array 2 :initial-contents (list c1 c2) :element-type 'character)))
    (unless (valid-digraph-p digraph)
      (editor-error "Invalid digraph"))
    (with-undo-group (cursor :merge merge)
      (aif (digraph digraph)
           (progn
             (buffer-insert (cursor-buffer cursor) (cursor-point cursor)
                            (make-string n :initial-element it))
             (setf (cursor-snapcol cursor) nil))
           (editor-error "Digraph ~s is not bound" digraph)))))

(define-editor-command insert-newline
    (:cursor cursor :modes (:insert) :record (:insert :record))
  (with-undo-group cursor
    (clear-whitespace cursor)
    (buffer-insert (cursor-buffer cursor) (cursor-point cursor) (string #\Newline))
    (let ((bol (point-beginning-of-line (cursor-point cursor) 2)))
      (when (point-eol-p bol)
        (buffer-indent-at (cursor-buffer cursor) bol)))
    (buffer-indent-at (cursor-buffer cursor) (cursor-point cursor))
    (setf (cursor-point cursor) (point-back-to-indentation (cursor-point cursor))
          (cursor-snapcol cursor) nil)))

(define-editor-command open-line
    (:cursor cursor :modes (:insert) :record (:insert :record))
  (with-undo-group cursor
    (with-marks ((after (cursor-point cursor) :left-inserting))
      (with-cursor-properties (cursor :mark-type :right-inserting)
        (buffer-insert (cursor-buffer cursor) (copy-point after) (string #\Newline))
        (buffer-indent-at (cursor-buffer cursor) after))))
  (setf (cursor-snapcol cursor) nil))

(defun insert-open-delimiter-aux (cursor n open-delim close-delim)
  (with-undo-group cursor
    (with-cursor-properties (cursor :mark-type :right-inserting)
      (buffer-insert (cursor-buffer cursor) (cursor-point cursor)
                     (apply #'concatenate 'string
                            (nconc
                             (loop repeat n collect open-delim)
                             (loop repeat n collect close-delim)))))
    (setf (cursor-point cursor) (point-forward-char (cursor-point cursor) (* n (length open-delim)))
          (cursor-snapcol cursor) nil)))

(defun insert-closed-delimiter-aux (cursor n close-delim)
  (with-undo-group cursor
    (let ((end-same (cursor-point cursor)))
      (loop for i below n
            do (unless (loop with p = (copy-point end-same)
                             for ch across close-delim
                             always (eql (point-next p) ch)
                             finally (setf end-same p))
                 (return)))
      (with-marks ((end end-same :left-inserting))
        (buffer-replace (cursor-buffer cursor) (cursor-point cursor) end (apply #'concatenate 'string
                                                                                (loop repeat n collect close-delim)))
        (setf (cursor-point cursor) end
              (cursor-snapcol cursor) nil)))))

(macrolet ((def (open-delim close-delim open-name &optional (close-name nil close-name-p))
             `(progn
                (define-editor-command ,open-name
                    (:cursor cursor :modes (:insert) :n n :record (:insert :record))
                  (insert-open-delimiter-aux cursor n ,open-delim ,close-delim))
                ,@(when close-name-p
                    `((define-editor-command ,close-name
                          (:cursor cursor :modes (:insert) :n n :record (:insert :record))
                        (insert-closed-delimiter-aux cursor n ,close-delim)))))))
  (def "(" ")" insert-open-parenthesis insert-closed-parenthesis)
  (def "[" "]" insert-open-bracket insert-closed-bracket)
  (def "{" "}" insert-open-brace insert-closed-brace)
  (def "'" "'" insert-single-quote)
  (def "\"" "\"" insert-double-quote)
  (def "`" "`" insert-back-quote)
  (def "<" ">" insert-open-greater-than insert-closed-less-than)
  (def "⟨" "⟩" insert-open-angle-bracket insert-closed-angle-bracket)
  (def "⦗" "⦘" insert-open-shell-bracket insert-closed-shell-bracket)
  (def "⟦" "⟧" insert-open-double-bracket insert-closed-double-bracket)
  (def "⟪" "⟫" insert-open-double-angle-bracket insert-closed-double-angle-bracket)
  (def "‘" "’" insert-open-single-quote insert-closed-single-quote)
  (def "“" "”" insert-open-double-quote insert-closed-double-quote))

(define-editor-command insert-monograph
    (:cursor cursor :modes (:insert) :n n :record (:insert :record)
     :keys ((key (read-input))))
  (block nil
    (aif (char-meta key)
         (awhen (alternate-delimiter it)
           (insert-open-delimiter-aux cursor n (string (open-delimiter it)) (string (close-delimiter it)))
           (return))
         (awhen (alternate-delimiter key)
           (with-undo-group (cursor)
             (buffer-insert (cursor-buffer cursor) (cursor-point cursor) (make-string n :initial-element it)))
           (setf (cursor-snapcol cursor) nil)
           (return)))
    (editor-error "~a is not a monograph" (unkbd (list key)))))

