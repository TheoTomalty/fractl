(in-package "FED")

(defun repl-generate-prompt (prompt-designator buffer)
  (etypecase prompt-designator
    (string prompt-designator)
    (function (funcall prompt-designator buffer))
    (null "")))

(defun init-repl (buffer &key prompt history)
  (declare (type buffer buffer)
           (type (or null function string) prompt)
           (type symbol history))
  (setf (buffer-variable buffer 'repl-history) history
        (buffer-variable buffer 'repl-prompt) prompt
        (buffer-variable buffer 'repl-history-position) -1
        (buffer-variable buffer 'repl-history-prefix) ""
        (buffer-variable buffer 'repl-history-original) ""
        (buffer-line-prefix buffer 0) (repl-generate-prompt prompt buffer)))

(defmacro repl-accept-eval ((string cursor) &body body)
  (once-only (cursor)
    (with-gensyms (buffer)
      `(let* ((,buffer (cursor-buffer ,cursor))
              (,string (text-string (buffer-text ,buffer))))
         (push-history ,cursor ,string)
         (cursor-clear-record ,cursor)
         (with-undo-group ,cursor
           (buffer-delete-between ,buffer (text-pos-point (buffer-text ,buffer) 0) (text-pos-point (buffer-text ,buffer) -1)))
         (exit-frames t)
         (unwind-protect
             (progn ,@body)
           (setf (buffer-line-prefix ,buffer 0) (repl-generate-prompt (buffer-variable ,buffer 'repl-prompt) ,buffer)))))))

(defun push-history (cursor string)
  (declare (type cursor cursor) (type string string))
  (unless (string= "" string)
    (push (list string (cursor-pos cursor)) (repl-history (cursor-buffer cursor)))))

(defun repl-history (buffer)
  (let ((history (buffer-variable buffer 'repl-history)))
    (if (listp history)
        history
        (editor-variable history))))

(defun (setf repl-history) (new buffer)
  (let ((history (buffer-variable buffer 'repl-history)))
    (if (listp history)
        (setf (buffer-variable buffer 'repl-history) new)
        (setf (editor-variable history) new))))

(defun repl-prev/next-history (cursor repeatp direction)
  (let* ((nextp (ecase direction (:next t) (:prev nil)))
         (buffer (cursor-buffer cursor))
         (start (aif (and repeatp (buffer-variable buffer 'repl-history-position)) (1+ it) 0))
         (end (aif (and repeatp (buffer-variable buffer 'repl-history-position)) it 0))
         (prefix (or (and repeatp (buffer-variable buffer 'repl-history-prefix))
                     (text-substring (buffer-text buffer) 0 (cursor-pos cursor))))
         (repl-string (text-substring (buffer-text buffer) 0))
         (prefix-matcher (lambda (entry)
                           (let ((string (first entry)))
                             (and (not (string= repl-string string))
                                  (<= (length prefix) (length string))
                                  (string= prefix (subseq string 0 (length prefix))))))))
    (unless repeatp
      (setf (buffer-variable buffer 'repl-history-original) (text-substring (buffer-text buffer) 0 (buffer-length buffer))
            (buffer-variable buffer 'repl-history-prefix) prefix
            (buffer-variable buffer 'repl-history-position) -1))
    (aif (if nextp
             (unless (eql -1 end) 
               (or (position-if prefix-matcher (repl-history buffer) :end end :from-end t) -1))
             (position-if prefix-matcher (repl-history buffer) :start start))
         (progn
           (setf (buffer-variable buffer 'repl-history-position) it)
           (destructuring-bind (string pos)
               (if (<= 0 it)
                   (nth it (repl-history buffer))
                   (list (buffer-variable buffer 'repl-history-original) (length (buffer-variable buffer 'repl-history-prefix))))
             (with-undo-group cursor
               (buffer-replace buffer (text-pos-point (buffer-text buffer) 0) (text-pos-point (buffer-text buffer) -1) string)
               (setf (cursor-pos cursor) pos
                     (cursor-snapcol cursor) pos))))
         (editor-error "No more matches"))))

(define-editor-command repl-prev-history
    (:cursor cursor :modes (:normal :insert)
     :keys ((repeatp (member *previous-binding* '(repl-prev-history repl-next-history)))))
  (repl-prev/next-history cursor repeatp :prev))

(define-editor-command repl-next-history
    (:cursor cursor :modes (:normal :insert)
     :keys ((repeatp (member *previous-binding* '(repl-prev-history repl-next-history)))))
  (repl-prev/next-history cursor repeatp :next))