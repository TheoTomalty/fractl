(in-package "FED")


(defstruct register
  (type     :point :type (member :point :line))
  (contents ""     :type string))

(defstruct (registers (:constructor %make-registers))
  (letter-registers nil :type hash-table)
  (number-registers nil :type (vector register 10))
  (default-register nil :type register)
  (delete-register  nil :type register))

(defstruct input-state
  (intermediate-table nil :type list)
  (previous-binding nil :type symbol)
  (count-prefix nil :type (or null (integer 0)))
  (register-prefix nil :type (or null character))
  (operator-prefix nil :type symbol)
  (operator-count-prefix nil :type (or null (integer 0)))
  (operator-register-prefix nil :type (or null character)))

(defclass editor ()
  ((stream
    :initarg :stream
    :reader editor-stream
    :type stream)
   (input-tty-p
    :initform nil
    :initarg :input-tty-p
    :reader editor-input-tty-p
    :type boolean)
   (full-frame
    :reader editor-full-frame
    :type full-frame)
   (repl-frame
    :reader editor-repl-frame
    :type repl-frame)
   (current-frame
    :accessor editor-current-frame
    :type frame)
   (render-state
    :initform (make-global-render-state)
    :accessor editor-render-state
    :type global-render-state)
   (hooks
    :initform (copy-tree *default-editor-hooks*)
    :accessor editor-hooks
    :type list)
   (buffers
    :initform nil
    :accessor editor-buffers
    :type list)
   (empty-buffer
    :reader editor-empty-buffer
    :type buffer)
   (input-state
    :initform (make-input-state)
    :reader editor-input-state
    :type input-state)
   (registers
    :initform (make-registers)
    :reader editor-registers
    :type registers)
   (recording-register
    :initform nil
    :accessor editor-recording-register
    :type (or null character))
   (last-replayed-register
    :initform nil
    :accessor editor-last-replayed-register
    :type (or null character))
   (tags
    :initform nil
    :accessor editor-tags
    :type list)
   (variables
    :initform (alist-hash-table (copy-tree *default-editor-variables*))
    :reader editor-variables
    :type hash-table)
   (timers
    :initform nil
    :accessor editor-timers
    :type list)
   (update-time
    :initform nil
    :accessor editor-update-time
    :type (or null (integer 0)))
   (marks
    :initform (make-global-marks)
    :reader editor-marks
    :type global-marks)))

(macrolet ((def (name editor-name &key read-only)
             `(progn
                (defun ,name ()
                  (,editor-name *editor*))
                ,@(unless read-only
                    `((defun (setf ,name) (new)
                        (setf (,editor-name *editor*) new)))))))
  ;; TODO: change (output-stream) to *editor-stream*?
  (def output-stream editor-stream :read-only t)
  (def input-tty-p editor-input-tty-p :read-only t)
  (def full-frame editor-full-frame :read-only t)
  (def repl-frame editor-repl-frame :read-only t)
  (def global-render-state editor-render-state :read-only t)
  (def buffers editor-buffers)
  (def empty-buffer editor-empty-buffer :read-only t)
  (def input-state editor-input-state :read-only t)
  (def registers editor-registers :read-only t)
  (def recording-register editor-recording-register)
  (def last-replayed-register editor-last-replayed-register)
  (def tags editor-tags)
  (def variables editor-variables :read-only t)
  (def marks editor-marks :read-only t))

(defun make-editor (stream termsize input-tty-p)
  (let* ((*editor* (make-instance 'editor :stream (make-editor-stream stream (coord-x termsize))
                                          :input-tty-p input-tty-p)))
    (setf (slot-value *editor* 'empty-buffer) (create-buffer " *empty*"))
    (let ((full-frame (make-full-frame termsize))
          (repl-frame (make-repl-frame termsize)))
      (setf (slot-value *editor* 'full-frame) full-frame
            (slot-value *editor* 'repl-frame) repl-frame
            (editor-current-frame *editor*) repl-frame)
      (push-buffer (create-buffer "*scratch*")
                   (full-frame-main-window full-frame))
      (fcl-repl nil)
      (fsh-repl nil)
      *editor*)))

(defun make-termio-editor ()
  (init-termio 0)
  (make-editor (sb-sys:make-fd-stream 0 :input t :output t) (termsize) t))

(defun make-test-editor (&optional (dims (coord 10 10)))
  (make-editor (make-string-output-stream) dims nil))

(defun editor-render-stream (editor)
  (editor-stream-io-stream (editor-stream editor)))

(defun render-stream ()
  (editor-render-stream *editor*))

(defun editor-variable (name)
  (declare (type symbol name))
  (multiple-value-bind (entry found-p) (gethash name (editor-variables *editor*))
    (values (first entry) found-p)))

(defun (setf editor-variable) (new name)
  (declare (type symbol name))
  (multiple-value-bind (entry found-p) (gethash name (editor-variables *editor*))
    (if found-p
        (setf (first entry) new)
        (setf (gethash name (editor-variables *editor*)) (list new nil)))))

(defun %define-editor-variable (name default-value doc)
  (declare (type symbol name) (type (or null string) doc))
  (let ((entry (list default-value doc)))
    (setf (assoc-value name *default-editor-variables*) entry)
    (when (and (boundp '*editor*) (not (hashash name (editor-variables *editor*))))
      (setf (gethash name (editor-variables *editor*)) entry)))
  name)

(define-editor-variable wrap nil
  "Whether or not lines that extend past the edge of the window are wrapped onto
the next line (if value is T), or displayed truncated (if value is NIL).")

(define-editor-variable number nil
  "Controls whether to display line numbers when rendering the buffer.
Possible values:
  NIL - do not display line numbers
  :ABSOLUTE - display absolute line numbers
  :RELATIVE - display line numbers relative to the cursor")

(define-editor-variable package nil
  "The package to use when evaluating forms within the buffer.")

(define-editor-variable default-insert nil
  "Prevents the aggregation of undo information while in insert mode.")

(define-editor-variable fill-column 80
  "The column at which to insert hard line-wraps when formatting.")

(define-editor-variable previous-edit-register #\"
  "Used by some insert-mode delete commands to group register insertions.")

(defclass editor-stream (sb-gray:fundamental-character-output-stream
                         sb-gray:fundamental-character-input-stream)
  ((io-stream
    :initarg :stream
    :reader editor-stream-io-stream
    :type stream)
   (capture-stream
    :initform (make-string-output-stream)
    :reader editor-stream-capture-stream
    :type string-stream)
   (capture-list
    :initform nil
    :accessor editor-stream-capture-list
    :type list)
   (capturing-p
    :initform nil
    :reader editor-stream-capturing-p
    :writer (setf %editor-stream-capturing-p)
    :type boolean)
   (line-column
    :initform 0
    :accessor editor-stream-line-column
    :reader sb-gray:stream-line-column
    :type (integer 0))
   (line-length
    :initarg :line-length
    :accessor editor-stream-line-length
    :reader sb-gray:stream-line-length
    :type (integer 0))
   (last-char
    :initarg nil
    :accessor editor-stream-last-char
    :type (or null character))
   (unread-char-p
    :initarg nil
    :accessor editor-stream-unread-char-p
    :type boolean)))

(defmethod sb-gray:stream-finish-output ((stream editor-stream))
  (unless (editor-stream-capturing-p stream)
    (finish-output (editor-stream-io-stream stream))))

(defmethod sb-gray:stream-force-output ((stream editor-stream))
  (unless (editor-stream-capturing-p stream)
    (force-output (editor-stream-io-stream stream))))

(defmethod sb-gray:stream-write-char ((stream editor-stream) character)
  (if (editor-stream-capturing-p stream)
      (write-char character (editor-stream-capture-stream stream))
      (write-char character (editor-stream-io-stream stream)))
  (if (eql #\Newline character)
      (setf (editor-stream-line-column stream) 0)
      (incf (editor-stream-line-column stream)))
  character)

(defmethod sb-gray:stream-write-string ((stream editor-stream) string &optional (start 0) end)
  (if (editor-stream-capturing-p stream)
      (write-string string (editor-stream-capture-stream stream) :start start :end end)
      (write-string string (editor-stream-io-stream stream) :start start :end end))
  (aif (position #\Newline string :start start :end end :from-end t)
       (setf (editor-stream-line-column stream) (- (or end (length string)) (1+ it)))
       (incf (editor-stream-line-column stream) (- (or end (length string)) start)))
  string)

(defmethod sb-gray:stream-read-char ((stream editor-stream))
  (exit-frames nil)
  (multiple-value-prog1
      (read-char (editor-stream-io-stream stream))
    ;; Reading a line resets the column
    (setf (editor-stream-line-column stream) 0)))

(defmethod sb-gray:stream-read-line ((stream editor-stream))
  (exit-frames nil)
  (multiple-value-prog1
      (read-line (editor-stream-io-stream stream))
    ;; Reading a line resets the column
    (setf (editor-stream-line-column stream) 0)))

(defmethod sb-gray:stream-read-char-no-hang ((stream editor-stream))
  (read-char-no-hang (editor-stream-io-stream stream)))

(defmethod sb-gray:stream-unread-char ((stream editor-stream) character)
  (unread-char character (editor-stream-io-stream stream)))

(defmethod sb-gray:stream-listen ((stream editor-stream))
  (listen (editor-stream-io-stream stream)))

(defun make-editor-stream (stream line-length)
  (make-instance 'editor-stream :stream stream :line-length line-length))

(defun editor-stream-send-message (stream string)
  (declare (type editor-stream stream) (type string string))
  (let ((capture-string (get-output-stream-string (editor-stream-capture-stream stream))))
    (unless (string= "" capture-string)
      (push capture-string (editor-stream-capture-list stream)))
    (push (list :message string) (editor-stream-capture-list stream)))
  nil)

(defun editor-stream-get-capture-string (stream include-messages-p)
  (declare (type editor-stream stream))
  (setf (editor-stream-line-column stream) 0)
  (with-output-to-string (s)
    (loop for elem in (nreverse (shiftf (editor-stream-capture-list stream) nil))
          do (etypecase elem
               (string
                (format s "~&~a" elem))
               (cons
                (assert (and (= 2 (length elem)) (eql :message (first elem))))
                (when include-messages-p
                  (format s "~&~a" (second elem))))))
    (let ((string (get-output-stream-string (editor-stream-capture-stream stream))))
      (unless (string= "" string)
        (format s "~&~a" string)))))

(defmethod (setf editor-stream-capturing-p) (new (stream editor-stream))
  (when (xor new (editor-stream-capturing-p stream))
    (setf (%editor-stream-capturing-p stream) new
          (editor-stream-line-column stream) 0)
    (unless new
      (write-string (editor-stream-get-capture-string stream nil) (editor-stream-io-stream stream))
      (finish-output (editor-stream-io-stream stream))))
  new)

(defun number-key-p (key)
  (when (char<= #\0 key #\9)
    (- (char-code key) (char-code #\0))))

(defun run-signal (key)
  ;; TODO: Does it make sense to go through the cursor here?
  (let* ((cursor (current-cursor))
         (mode (cursor-mode cursor)))
    (multiple-value-bind (binding foundp)
        (immediate-binding mode (buffer-keymap (cursor-buffer cursor)) key)
      (when foundp
        (let ((*no-record* t))
          (funcall binding :cursor cursor))))))

(defun run-key (key)
  (let ((n (number-key-p key))
        (state (input-state)))
    (if (and (input-state-count-prefix state) n)
        (setf (input-state-count-prefix state) (+ (* (input-state-count-prefix state) 10) n))
        (let* ((cursor (current-cursor))
               (mode (cursor-mode cursor)))
          (unless (input-state-intermediate-table state)
            (setf (input-state-intermediate-table state) (make-partial-keymap mode (buffer-keymap (cursor-buffer cursor)))))
          (multiple-value-bind (binding partial keys) (partial-keymap-binding (input-state-intermediate-table state) key)
            (setf (input-state-intermediate-table state) partial)
            (when (and (not binding) (not partial))
              (editor-error "~a is unbound" (unkbd keys)))
            (when binding
              (let ((*current-binding* binding)
                    (*previous-binding* (input-state-previous-binding state))
                    (*current-key* key))
                (setf (input-state-previous-binding state) binding)
                (let ((args
                        (unless (get binding 'prefix)
                          (let ((n (and (get binding 'count-prefix)
                                        (if (and (input-state-count-prefix state) (input-state-operator-count-prefix state))
                                            (* (input-state-count-prefix state) (input-state-operator-count-prefix state))
                                            (or (input-state-count-prefix state) (input-state-operator-count-prefix state)))))
                                (r (unless (input-state-operator-prefix state)
                                     (and (get binding 'register-prefix)
                                          (input-state-register-prefix state))))
                                (o (when (input-state-operator-prefix state)
                                     (and (get binding 'operator-prefix)
                                          (list (input-state-operator-prefix state)
                                                (or (input-state-register-prefix state)
                                                    (input-state-operator-register-prefix state)))))))
                            (setf (input-state-count-prefix state) nil
                                  (input-state-register-prefix state) nil
                                  (input-state-operator-count-prefix state) nil
                                  (input-state-operator-register-prefix state) nil
                                  (input-state-operator-prefix state) nil)
                            (nconc (when n (list :n n)) (when r (list :register r)) (when o (list :operator o)))))))
                  (unless (or (symbolp binding) (functionp binding))
                    (editor-error "The binding ~s is not a symbol or a function" binding))
                  (when (symbolp binding)
                    (unless (and (fboundp binding) (not (macro-function binding)))
                      (editor-error "The function ~s is unbound" binding)))
                  (let ((*debugger-hook* #'fcl-debugger))
                    (apply binding :cursor cursor args))))))))))

(defun add-hook (hooks type func)
  (aif (assoc type hooks)
       (unless (member func (cddr it))
         (if (second it)
             (push func (cddr it))
             (setf (cddr it) (nconc (cddr it) (list func)))))
       (error "No hook named ~s" type))
  func)

(defun remove-hook (hooks type func)
  (aif (assoc type hooks)
       (setf (cddr it) (delete func (cddr it)))
       (error "No hook named ~s" type))
  func)

(defun add-default-editor-hook (type func)
  (add-hook *default-editor-hooks* type func)
  (when (boundp '*editor*)
    (add-editor-hook type func))
  func)

(defun add-editor-hook (type func)
  (add-hook (editor-hooks *editor*) type func))

(defun remove-editor-hook (type func)
  (remove-hook (editor-hooks *editor*) type func))

(defun run-hooks (hooks type &rest args)
  (aif (assoc type hooks)
       (loop for func in (cddr it)
             do (print-and-ignore-error (apply func args)))
       (error "No hook named ~s" type)))

(defun run-editor-hooks (type &rest args)
  (apply #'run-hooks (editor-hooks *editor*) type args))

(defun encode-timeout (timeout)
  (destructuring-bind (n unit) timeout
    (ecase unit
      (:second (* n internal-time-units-per-second))
      (:millisecond (truncate (* n internal-time-units-per-second) 1000))
      (:microsecond (truncate (* n internal-time-units-per-second) 1000000)))))

(defun add-editor-timer (timeout function)
  (push (list (encode-timeout timeout) function)
        (editor-timers *editor*))
  (setf (editor-timers *editor*) (sort (editor-timers *editor*) #'< :key #'first))
  (unless (editor-update-time *editor*)
    (setf (editor-update-time *editor*) (get-internal-real-time)))
  function)

(defun remove-editor-timer (function)
  (setf (editor-timers *editor*) (delete function (editor-timers *editor*) :key #'second)))

(defun run-editor-timers (&optional at-least-one-p)
  (let ((new-time (get-internal-real-time)))
    (loop for entry in (editor-timers *editor*)
          do (decf (first entry) (max 0 (- new-time (editor-update-time *editor*)))))
    (setf (editor-update-time *editor*) new-time)
    (let ((entries (awhen (and at-least-one-p (pop (editor-timers *editor*))) (list it))))
      (loop while (awhen (caar (editor-timers *editor*)) (<= it 0))
            do (push (pop (editor-timers *editor*)) entries))
      (loop for (timout function) in (nreverse entries)
            do (print-and-ignore-error (funcall function))))))

(defun next-timeout ()
  (or (caar (editor-timers *editor*)) -1))

(defun render-and-draw ()
  (run-editor-hooks 'pre-render (current-frame))
  (frame-render (current-frame))
  (run-editor-hooks 'post-render (current-frame))
  (run-editor-hooks 'pre-draw (current-frame))
  (frame-draw (current-frame))
  (run-editor-hooks 'post-draw (current-frame)))

(defun read-input ()
  (if *pending-input*
      (pop *pending-input*)
      (loop (render-and-draw)
            (run-editor-hooks 'pre-read)
            (let ((char (read-input-or-signal (next-timeout))))
              (run-editor-hooks 'post-read)
              (run-editor-timers (null char))
              (cond ((null char))
                    ((char-signal char)
                     (run-signal char))
                    (t
                     (awhen (recording-register)
                       (let ((reg (get-register it)))
                         (setf (register-contents reg) (concatenate 'string (register-contents reg) (string char)))))
                     (return char)))))))

(defun run-key-toplevel (key)
  (handler-case
      (restart-case
          (run-key key)
        (abort-editor-command ()
          :report "Abort the editor command."))
    ((or editor-error editor-cancel) (e)
      (let ((state (input-state)))
        (setf (input-state-count-prefix state) nil
              (input-state-register-prefix state) nil
              (input-state-operator-count-prefix state) nil
              (input-state-operator-register-prefix state) nil
              (input-state-operator-prefix state) nil))
      (let ((cursor (current-cursor)))
        (when (and (typep e 'editor-cancel) (eql :visual (cursor-mode cursor)))
          (normal-mode :cursor cursor))
        (when (eql :operator (cursor-mode cursor))
          (setf (cursor-mode cursor) :normal))
        (when (typep e 'editor-error)
          (ignore-errors (echo-minibuffer (format nil "~a" e))))
        (write-char #\Bel (render-stream))
        (finish-output (render-stream))))))

(defun run-main ()
  (loop (let ((char (read-input)))
          (run-editor-hooks 'pre-command)
          (run-key-toplevel char)
          (run-editor-hooks 'post-command))))

(defstruct (non-debug-var-wrapper (:constructor make-non-debug-var-wrapper (value)))
  value)

(defmethod print-object ((obj non-debug-var-wrapper) stream)
  (write-string "[" stream)
  (print-object (non-debug-var-wrapper-value obj) stream)
  (write-string "]" stream))

(defun show-condition (condition s)
  (handler-case (format s "~a" condition)
    (error (e) (ignore-errors (format s "Error printing condition: ~a" e))))
  (let ((restarts (compute-restarts condition)))
    (when restarts
      (format s "~%~%Restarts:")
      (loop for restart in restarts and i from 0
            do (ignore-errors (format s "~% ~2d: [~a] ~a" i (restart-name restart) restart)))))
  (format s "~%~%Backtrace:")
  (loop for frame = (loop for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
                          while frame
                          when (eql 'invoke-debugger (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))
                          do (return (sb-di:frame-down frame))
                          finally (return (sb-di:top-frame)))
        then (sb-di:frame-down frame)
        and i from 0
        while frame
        do (ignore-errors
            (let* ((fun (sb-di:frame-debug-fun frame))
                   (lambda-list (sb-di:debug-fun-lambda-list fun))
                   (print-args (list (sb-di:debug-fun-name fun))))
              (loop with lambda-list-keyword = nil
                    for var in (if (listp lambda-list) lambda-list nil)
                    do (cond ((sb-di:debug-var-p var)
                              (push (sb-di:debug-var-value var frame) print-args))
                             ((and (consp var) (member (first var) '(:optional :rest :keyword)))
                              (unless (eql (first var) lambda-list-keyword)
                                (push (second (assoc (first var) '((:optional &optional) (:rest &rest) (:keyword &key)))) print-args))
                              (push (if (sb-di:debug-var-p (second var))
                                        (sb-di:debug-var-value (second var) frame)
                                        (make-non-debug-var-wrapper (second var)))
                                    print-args))
                             (t
                              (push (make-non-debug-var-wrapper (second var)) print-args))))
              (let ((*print-lines* 1))
                (format s "~% ~2d: ~s" i (nreverse print-args))))))
  (fresh-line s))

(defun editor-debugger (condition prev-hook)
  "Invoked if there is an error in the core logic of the editor."
  (declare (ignore prev-hook))
  (show-condition condition *debug-io*)
  (sb-ext:exit))

(defun fcl-debugger (condition prev-hook)
  "Invoked if there is an error in an editor command."
  (declare (ignore prev-hook))
  (let ((*debugger-hook* #'editor-debugger))
    (show-condition condition *debug-io*)
    (let ((*debug-level* (if (integerp *debug-level*) (1+ *debug-level*) 0))
          (*debug-restarts* (compute-restarts condition))
          (*debug-default-restart* (find-restart 'abort-editor-command condition)))
      (fcl-repl)
      (recursive-edit
        (run-main)))
    (sb-ext:exit)))

(defun editor-progv (editor)
  (values (list '*editor* '*editor-stream* 
                '*standard-output* '*standard-input* '*error-output* '*trace-output* '*debug-io*)
          (list editor (editor-stream editor)
                (make-synonym-stream '*editor-stream*)
                (make-synonym-stream '*editor-stream*)
                (make-synonym-stream '*editor-stream*)
                (make-synonym-stream '*editor-stream*)
                (make-synonym-stream '*editor-stream*))))

(defun init-termio-editor ()
  (multiple-value-bind (vars vals) (editor-progv (make-termio-editor))
    (loop for var in vars and val in vals do (setf (symbol-value var) val)))
  ;; (sb-profile:reset)
  (push (lambda ()
          ;; (sb-profile:report)
          (exit-frames t))
        sb-ext:*exit-hooks*)
  (run-editor-hooks 'editor-init))

(defun main ()
  (setf *debugger-hook* #'editor-debugger)
  (sb-ext:enable-debugger)
  (let ((argv (let ((it (cdr sb-ext:*posix-argv*)))
                (if (string= "--" (car it)) (cdr it) it))))
    (fsh:init-shell)
    (init-termio-editor)
    (setf *package* (find-package "FED"))
    (when argv
      (loop for arg in argv do (find-file arg)))
    (run-main)))

(define-editor-command count-prefix
    (:modes (:insert :normal :visual :operator) :prefix-p t)
  (setf (input-state-count-prefix (input-state)) (or (number-key-p *current-key*) 0)))

(define-editor-command register-prefix
    (:modes (:insert :normal :visual :operator) :prefix-p t
     :keys ((char (read-input))))
  (unless (eql #\Esc char)
    (if (valid-register-p char)
        (setf (input-state-register-prefix (input-state)) char)
        (editor-error "Invalid register ~s" char))))

(define-editor-command editor-exit ()
  (if (loop for buffer in (buffers)
            never (and (buffer-file buffer) (buffer-changed-p buffer)))
      (sb-ext:quit)
      (editor-error "Some files are unsaved")))

(define-editor-command editor-save-and-exit ()
  (save-all-buffers)
  (editor-exit))

(define-editor-command keyboard-quit ()
  (signal 'editor-cancel))
