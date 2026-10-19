(in-package "FED")


(defclass buffer ()
  ((name
    :initarg :name
    :accessor buffer-name
    :type string)
   (text
    :initarg :text
    :initform (make-text)
    :accessor buffer-text
    :type text)
   (file
    :initarg :file
    :initform nil
    :accessor buffer-file
    :type (or null pathname))
   (cursors
    :documentation "The list of cursors whose CURSOR-BUFFER is this buffer."
    :initform nil
    :accessor buffer-cursors
    :type list)
   (keymap
    :initform *global-keymap*
    :accessor buffer-keymap
    :type hash-table)
   (locked-p
    :documentation "Prevents the cursor of a given frame from escaping the buffer using normal
commands."
    :initarg :locked-p
    :initform nil
    :reader buffer-locked-p
    :type boolean)
   (readonly-p
    :documentation "Prevents edits to the BUFFER-TEXT."
    :initarg :readonly-p
    :initform nil
    :accessor buffer-readonly-p
    :type boolean)
   (grammar
    :initarg :grammar
    :initform nil
    :reader buffer-grammar)
   (ast
    :documentation "The toplevel TEXT-AST of the last correct parse."
    :initform nil
    :accessor %buffer-ast
    :type (or null text-ast))
   (ast-valid-p
    :initform nil
    :accessor %buffer-ast-valid-p
    :type boolean)
   (parsed-p
    :documentation "Whether or not AST and AST-VALID-P are up to date (the buffer has been parsed at
least once, and no changes have been made since the last parse)."
    :initform nil
    :accessor buffer-parsed-p
    :type boolean)
   (variables
    :initform (make-hash-table)
    :reader buffer-variables
    :type hash-table)
   (paint-regions
    :initform nil
    :accessor buffer-paint-regions
    :type list)
   (node-attrs
    :initform (make-hash-table)
    :accessor buffer-node-attrs
    :type hash-table)
   (node-deps
    :initform (make-hash-table)
    :accessor buffer-node-deps
    :type hash-table)
   (undo-tree
    :initform (make-undo-tree)
    :reader buffer-undo-tree
    :type undo-tree)
   ;; Positions of previous / or ? search results
   (search-results
    :initform nil
    :accessor buffer-search-results
    :type (or (cons string vector) null))))

(defun current-buffer ()
  (window-buffer (current-window)))

(defun create-buffer (name &key window default-insert locked-p readonly-p)
  (let ((ret (make-instance 'buffer :name name :locked-p locked-p :readonly-p readonly-p)))
    (when default-insert
      (setf (buffer-variable ret 'default-insert) t))
    (pushnew ret (buffers))
    (if window
        (multiple-value-bind (buffer cursor) (push-buffer ret window)
          (when default-insert
            (let ((*no-record* nil))
              (insert-mode :cursor cursor)))
          (values buffer cursor))
        ret)))

(defun filename-buffer-name (filename)
  "Returns information about filename in 4 separate return values.  (1) The 
name of the corresponding buffer, if it is to be created.  (2) The pathname of
the file (which may not yet exist).  (3) The type of the file (either :FILE or
:DIRECTORY) that exists or inferred from the pathname.  (4) Whether or not the
file exists."
  (let ((pathname (merge-pathnames (pathname filename)))
        (file-type nil)
        (exists-p nil))
    (awhen (probe-file pathname)
      (setf pathname it
            exists-p t))
    (if (and (null (pathname-name pathname)) (null (pathname-type pathname)))
        (setf file-type :directory)
        (setf file-type :file))
    (values (cond ((pathname-type filename)
                   (format nil "~a.~a" (or (pathname-name filename) "") (pathname-type filename)))
                  ((pathname-name filename)
                   (pathname-name filename))
                  (t
                   (let ((directory (first (last (pathname-directory pathname)))))
                     (if (stringp directory) directory "[No Name]"))))
            pathname
            file-type
            exists-p)))

(defun create-file-buffer (filename &optional window)
  (multiple-value-bind (buffer-name pathname file-type exists-p) (filename-buffer-name filename)
    (unless (eql :file file-type)
      (editor-error "~s does not indicate a file"))
    (let ((ret
            (make-instance 'buffer
                           :name buffer-name
                           :file pathname
                           :text (aif exists-p
                                      (string-text (read-file-string pathname))
                                      (make-text))
                           :grammar (default-grammar (pathname-type pathname)))))
      (setf (buffer-variable ret 'number) :relative)
      (pushnew ret (buffers))
      (if window
          (push-buffer ret window)
          ret))))

(defun find-buffer (buffer-designator)
  (etypecase buffer-designator
    (buffer buffer-designator)
    (string (find buffer-designator (buffers) :key #'buffer-name :test #'equal))))

(defun buffer-windows (buffer)
  (mapcar #'cursor-window (buffer-cursors buffer)))

(defun buffer-length (buffer)
  (text-length (buffer-text buffer)))

(defun buffer-nlines (buffer)
  (1+ (coord-y (text-end-coord (buffer-text buffer)))))

(defun buffer-line-max-vcol (buffer line)
  ;; TODO: add line suffix
  (coord-x (point-vcoord (text-coord-point (buffer-text buffer) (coord -1 line)))))

(defun buffer-tabsize (buffer)
  (text-tabsize (buffer-text buffer)))

(defun buffer-variable (buffer name)
  (declare (type symbol name))
  (multiple-value-bind (val found-p) (gethash name (buffer-variables buffer))
    (if found-p
        (values val :buffer)
        (multiple-value-bind (val found-p) (editor-variable name)
          (if found-p
              (values val :editor)
              (values nil nil))))))

(defun (setf buffer-variable) (new buffer name)
  (declare (type symbol name))
  (setf (gethash name (buffer-variables buffer)) new))

(defmethod (setf buffer-grammar) (new (buffer buffer))
  (buffer-unparse buffer)
  (setf (slot-value buffer 'grammar) new
        (buffer-parsed-p buffer) nil)
  new)

(defun buffer-changed-p (buffer)
  (undo-tree-changed-p (buffer-undo-tree buffer)))

(defun ast-diff (new old &key removed-fn added-fn)
  (tree-diff new old :test 'eq :removed-fn removed-fn :added-fn added-fn
             :children-fn #'ast-children))

(defun tree-diff (new old &key (test 'eql) children-fn removed-fn added-fn)
  ;; 1. Traverse the nodes in each layer of the tree in tandem, starting with the root node,
  ;;    then all nodes at height 1, and so on, until there are no more nodes in the layers.
  ;; 2. Mark each node as "seen" as they are encountered.
  ;; 3. If a newly-seen node in one tree is present in the other tree then mark the node in
  ;;    both trees as "shared".  Also, remove all "seen" marks on children of the node in the
  ;;    tree where the node already existed (as if it had not been descended).
  ;; 4. Do not include "shared" nodes in the children for the next layer.
  ;; 5. Different nodes in either tree are those marked as "seen" but not "shared"
  (let ((new-marks (make-hash-table :test test)) ;; node -> (list* shared-p children)
        (old-marks (make-hash-table :test test))) ;; ^^
    (labels ((unmark-children (children marks)
               (loop for child in children
                     do (awhen (gethash child marks)
                          (remhash child marks)
                          (unmark-children (cdr it) marks))))
             (mark (node marks other-marks)
               (let ((shared (gethash node other-marks)))
                 (when shared
                   (setf (car shared) t)
                   (unmark-children (cdr shared) other-marks))
                 (setf (gethash node marks) (list* (and shared t) (funcall children-fn node))))))
      (loop with old-children = (list old) and new-children = (list new)
            while (or old-children new-children)
            for level from 0
            do (loop for node in old-children do (mark node old-marks new-marks))
               (loop for node in new-children do (mark node new-marks old-marks))
               (setf old-children (loop for node in old-children
                                        append (let ((entry (gethash node old-marks)))
                                                 (unless (car entry) (cdr entry)))))
               (setf new-children (loop for node in new-children
                                        append (let ((entry (gethash node new-marks)))
                                                 (unless (car entry) (cdr entry))))))
      (loop for node being the hash-keys of old-marks
            using (hash-value (shared-p))
            do (unless shared-p (funcall removed-fn node)))
      (loop for node being the hash-keys of new-marks
            using (hash-value (shared-p))
            do (unless shared-p (funcall added-fn node))))))

(defun buffer-paint (buffer left-type left right-type right attributes)
  (let ((region (make-region left-type left right-type right (make-text-face :attributes attributes))))
    (push region (buffer-paint-regions buffer))
    region))

(defun buffer-unpaint (buffer region)
  (setf (buffer-paint-regions buffer) (delete region (buffer-paint-regions buffer)))
  (delete-region region))

(defun buffer-paint-node (buffer node attributes &optional (attach node))
  (setf (gethash node (buffer-node-attrs buffer)) attributes)
  (unless (eq node attach)
    (push node (gethash attach (buffer-node-deps buffer)))))

(defun buffer-unpaint-node (buffer node)
  (remhash node (buffer-node-attrs buffer))
  (loop for dependant in (prog1 (gethash node (buffer-node-deps buffer))
                           (remhash node (buffer-node-deps buffer)))
        do (buffer-unpaint-node buffer dependant)))

(defun buffer-ast (buffer)
  (when (and (buffer-grammar buffer) (not (buffer-parsed-p buffer)))
    (print-and-ignore-error
      (multiple-value-bind (node success-p) (evm-run (buffer-text buffer) (buffer-grammar buffer))
        (if success-p
            (let ((*package* (setf (buffer-variable buffer 'package)
                                   (or (eval-ast node 'extract-package)
                                       *package*))))
              (setf (%buffer-ast-valid-p buffer) t)
              (if (%buffer-ast buffer)
                  (ast-diff node (%buffer-ast buffer)
                            :removed-fn (lambda (node) (buffer-unpaint-node buffer node))
                            :added-fn (lambda (node) (buffer-syntax-highlight buffer node)))
                  (labels ((aux (node)
                             (buffer-syntax-highlight buffer node)
                             (loop for child in (ast-children node)
                                   do (aux child))))
                    (aux node)))
              (setf (%buffer-ast buffer) node))
            (setf (%buffer-ast-valid-p buffer) nil)))))
  (if (buffer-grammar buffer)
      (values (when (%buffer-ast-valid-p buffer) (%buffer-ast buffer)) (%buffer-ast-valid-p buffer))
      (values nil t)))

(defun buffer-unparse (buffer)
  (loop for region in (buffer-paint-regions buffer)
        do (delete-region region))
  (point-invalidate-between (text-pos-point (buffer-text buffer) 0)
                            (text-pos-point (buffer-text buffer) -1))
  (setf (%buffer-ast buffer) nil
        (%buffer-ast-valid-p buffer) nil
        (buffer-node-attrs buffer) (make-hash-table)
        (buffer-node-deps buffer) (make-hash-table)))

(defvar *default-grammars* nil)

;; TODO: full mode definition
(defun define-default-grammar (extension grammar &optional (reader nil reader-p))
  (declare (type string extension))
  (setf *default-grammars* (delete extension *default-grammars* :key #'car :test #'string=))
  (push (list* extension grammar (when reader-p (list reader))) *default-grammars*))

(defun default-grammar (extension)
  (cdr (assoc extension *default-grammars* :test #'string=)))

(defun buffer-reader (buffer)
  (let ((grammar (buffer-grammar buffer)))
    (if (and grammar (consp grammar) (cdr grammar))
        (values (second grammar) t)
        (values nil nil))))

(defun buffer-reader-strict (buffer)
  (multiple-value-bind (reader found-p) (buffer-reader buffer)
    (if found-p reader (error "Reader not found"))))

(defun delete-buffer (buffer)
  (loop for window in (buffer-windows buffer)
        do (window-delete-buffer window buffer))
  (setf (buffers) (delete buffer (buffers)))
  (setf (tags) (delete buffer (tags) :key #'car))
  (delete-buffer-from-global-marks buffer (marks)))

;; TODO: this should be a function on text, not buffer
(defun buffer-line-prefix (buffer line)
  (if (= 0 line)
      (text-prefix (buffer-text buffer))
      ""))

(defun buffer-line-suffix (buffer line)
  (declare (ignore buffer line))
  "")

(defun (setf buffer-line-prefix) (new buffer line)
  (if (= 0 line)
      (setf (text-prefix (buffer-text buffer)) new)
      (unimplemented)))

(defun (setf buffer-line-suffix) (new buffer line)
  (declare (ignore new buffer line))
  (unimplemented))

(defun buffer-readjust-cursors (buffer)
  (loop for cursor in (buffer-cursors buffer)
        do (setf (cursor-point cursor) (cursor-point cursor))))

(defun buffer-insert (buffer point string)
  (assert (eq (point-text point) (buffer-text buffer)))
  (when (buffer-readonly-p buffer)
    (editor-error "Buffer is read-only"))
  (point-invalidate point)
  (let* ((pos (point-pos point)))
    (point-insert point string)
    (buffer-push-undo-edit buffer pos "" string))
  (buffer-invalidate-search-results buffer)
  (buffer-readjust-cursors buffer)
  (setf (buffer-parsed-p buffer) nil))

(defun buffer-delete-between (buffer start end)
  (when (buffer-readonly-p buffer)
    (editor-error "Buffer is read-only"))
  (let ((n (- (point-pos end) (point-pos start))))
    (assert (and (eq (point-text start) (buffer-text buffer)) (eq (point-text end) (buffer-text buffer))))
    (assert (<= 0 n))
    (when (< 0 n)
      (point-invalidate-between start end)
      (let* ((pos (point-pos start))
             (string (text-substring (point-text start) pos (+ pos n))))
        (point-delete start n)
        (buffer-push-undo-edit buffer pos string ""))
      (buffer-invalidate-search-results buffer)
      (buffer-readjust-cursors buffer)
      (setf (buffer-parsed-p buffer) nil)
      t)))

(defun buffer-delete-between-copy-register (buffer start end register type &optional force-append-type)
  (copy-into-register register (text-substring (buffer-text buffer) (point-pos start) (point-pos end)) type force-append-type)
  (buffer-delete-between buffer start end))

(defun buffer-delete (buffer point &optional (n 1))
  (buffer-delete-between buffer point (point-forward-char (copy-point point) n)))

(defun buffer-replace (buffer p1 p2 stringoid)
  (when (buffer-readonly-p buffer)
    (editor-error "Buffer is read-only"))
  (let* ((text (buffer-text buffer))
         (pos (point-pos p1))
         (old-string (text-substring text pos (point-pos p2)))
         (new-string (if (functionp stringoid) (funcall stringoid old-string) stringoid)))
    (point-invalidate-between p1 p2)
    (let* ((replace-length (min (length old-string) (length new-string)))
           (replace-string (subseq new-string 0 replace-length)))
      (when (< 0 replace-length)
        (setf p1 (point-replace p1 replace-string))
        (buffer-push-undo-edit buffer pos (subseq old-string 0 replace-length) replace-string)))
    (cond ((< (length new-string) (length old-string))
           (buffer-delete-between buffer p1 p2))
          ((< (length old-string) (length new-string))
           (buffer-insert buffer p1 (subseq new-string (length old-string)))))
    (buffer-readjust-cursors buffer)
    (buffer-invalidate-search-results buffer)
    (setf (buffer-parsed-p buffer) nil)))

(define-editor-command toggle-wrap
    (:cursor cursor)
  (let ((buffer (cursor-buffer cursor)))
    (setf (buffer-variable buffer 'wrap) (not (buffer-variable buffer 'wrap)))))

(define-editor-command toggle-number
    (:cursor cursor)
  (let ((buffer (cursor-buffer cursor)))
    (if (eql :absolute (buffer-variable buffer 'number))
        (setf (buffer-variable buffer 'number) nil)
        (setf (buffer-variable buffer 'number) :absolute))))

(define-editor-command toggle-relative-number
    (:cursor cursor)
  (let ((buffer (cursor-buffer cursor)))
    (if (eql :relative (buffer-variable buffer 'number))
        (setf (buffer-variable buffer 'number) nil)
        (setf (buffer-variable buffer 'number) :relative))))

(define-editor-command switch-buffer
    (:cursor cursor :n n)
  (awhen (nth n (window-buffers (cursor-window cursor)))
    (push-buffer it (cursor-window cursor))))

(define-editor-command reload-buffer
    (:cursor cursor :n n)
  (buffer-unparse (cursor-buffer cursor)))

(defun buffer-save (buffer)
  (aif (buffer-file buffer)
       (handler-case
           (progn
             (with-open-file (s it :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-string (text-string (buffer-text buffer)) s))
             (undo-tree-save (buffer-undo-tree buffer)))
         (file-error ()
           (editor-error "Could not write file")))
       (editor-error "Not a file buffer")))

(define-editor-command save-buffer
    (:cursor cursor)
  (let ((buffer (cursor-buffer cursor)))
    (if (buffer-changed-p buffer)
        (progn
          (buffer-save buffer)
          (echo-minibuffer "File saved"))
        (echo-minibuffer "File does not need saving"))))

(define-editor-command save-all-buffers ()
  (loop with files-saved = 0
        for buffer in (buffers)
        do (when (and (buffer-file buffer) (buffer-changed-p buffer))
             (buffer-save buffer)
             (incf files-saved))
        finally (if (= 0 files-saved)
                    (echo-minibuffer "No files needed saving")
                    (echo-minibuffer (format nil "Saved ~d files" files-saved)))))

(define-editor-command load-buffer
    (:cursor cursor)
  (let ((buffer (cursor-buffer cursor)))
    (multiple-value-bind (reader foundp) (buffer-reader buffer)
      (if foundp
          (let ((*package* *package*))
            (with-compilation-unit (:source-namestring (awhen (buffer-file buffer) (namestring it))
                                    :allow-other-keys t)
              (evm-eval (buffer-text buffer) reader))
            (echo-minibuffer "Buffer loaded"))
          (editor-error "Buffer does not have an associated reader")))))

(defun kill-buffer-aux (buffer &optional force)
  (if (and (not force) (buffer-file buffer) (buffer-changed-p buffer))
      (editor-error "Buffer is not saved")
      (delete-buffer buffer)))

(define-editor-command kill-buffer
    (:cursor cursor)
  (kill-buffer-aux (cursor-buffer cursor)))

(define-editor-command kill-buffer-force
    (:cursor cursor)
  (kill-buffer-aux (cursor-buffer cursor) t))

(defun switch-to-buffer (buffer)
  (let* ((windows (buffer-windows buffer))
         (window (if windows
                     (if (member (current-window) windows)
                         (current-window)
                         (first windows))
                     (full-frame-main-window (full-frame)))))
    (push-buffer buffer window)
    (switch-to-window window)
    buffer))

(defun buffer-status-indicator (buffer)
  (if (buffer-changed-p buffer) "[*]" "[-]"))

;; TODO: change name to "buffer"
(fsh:define-fsh-command "b" (&rest args)
  (flet ((designator-buffer (str)
           (aif (ignore-errors (parse-integer str))
                (or (nth it (buffers))
                    (fsh:shell-error "~d does not designate a buffer" it))
                (or (find-buffer str)
                    (fsh:shell-error "No buffer named ~a" str))))
         (usage (&optional (code 1))
           (format *error-output* "usage: buffer [-d] [BUFFER-DESIGNATOR]~%")
           (signal 'fsh:abort-fsh-command :exit-code code)))
    (if args
        (if (string= "-d" (first args))
            (if (= 2 (length args))
                (kill-buffer-aux (designator-buffer (second args)))
                (usage))
            (if (= 1 (length args))
                (switch-to-buffer (designator-buffer (first args)))
                (usage)))
        (loop for buffer in (buffers)
              and i from 0
              do (format t "~2d: ~a ~a~%" i (buffer-status-indicator buffer) (buffer-name buffer))))))
