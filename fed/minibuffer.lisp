(in-package "FED")


(defun create-minibuffer (window)
  (multiple-value-bind (buffer cursor) (push-buffer (create-buffer "*minibuffer*") window)
    (setf (mark-type (cursor-mark cursor)) :right-inserting)
    buffer))

(defun minibuffer ()
  (assert (full-frame))
  (full-frame-minibuffer (full-frame)))

(defun minibuffer-frame ()
  (full-frame))

(defun minibuffer-window ()
  (full-frame-minibuffer-window (full-frame)))

(defun minibuffer-window-used-p ()
  (not (eql (window-buffer (minibuffer-window)) (minibuffer))))

(defun clear-minibuffer ()
  (let ((*no-undo* t)
        (text (buffer-text (minibuffer))))
    (buffer-delete-between (minibuffer) (text-pos-point text 0) (text-pos-point text -1))))

(add-default-editor-hook 'pre-command 'clear-minibuffer)

(defun echo-minibuffer (string)
  (editor-stream-send-message (output-stream) string))

(defun add-minibuffer-string (string)
  (unless (string= "" string)
    (let ((*no-undo* t))
      (with-marks ((end (text-pos-point (buffer-text (minibuffer)) -1) :left-inserting))
        (unless (point-bol-p end)
          (buffer-insert (minibuffer) end (string #\Newline)))
        (buffer-insert (minibuffer) end string)))))

(defvar *minibuffer-keymap*)

(defun open-minibuffer (callback &optional (prompt "> ") (history 'minibuffer-history))
  ;; TODO: expand minibuffer when more than one line using edit hooks
  (unless (eql (current-frame) (minibuffer-frame))
    ;; TODO: implement minibuffer in repl-frame
    (unimplemented))
  (when (minibuffer-window-used-p)
    (editor-error "Minibuffer in use"))
  (let ((buffer (create-buffer "*minibuffer*" :window (minibuffer-window) :default-insert t
                               :locked-p t)))
    (init-repl buffer :prompt prompt :history history)
    (setf (buffer-keymap buffer) *minibuffer-keymap*
          (buffer-variable buffer 'minibuffer-callback) callback)
    (switch-to-window (minibuffer-window))))

(defun minibuffer-close (cursor reason)
  (declare (type (member :accept :close :cancel) reason))
  (assert (and (minibuffer-window-used-p)
               (eql (window-buffer (minibuffer-window)) (cursor-buffer cursor))))
  (let* ((buffer (cursor-buffer cursor))
         (string (text-string (buffer-text buffer)))
         (callback (buffer-variable buffer 'minibuffer-callback)))
    (push-history cursor string)
    (window-delete-buffer (minibuffer-window) buffer)
    (delete-buffer buffer)
    (funcall callback string reason)))

(define-editor-command minibuffer-accept
    (:cursor cursor :modes (:normal :insert))
  (minibuffer-close cursor :accept))

(define-editor-command minibuffer-cancel
    (:cursor cursor :modes (:normal :insert))
  (minibuffer-close cursor :cancel))

(define-editor-command minibuffer-ctrl-d
    (:cursor cursor :modes (:insert) :n n :register r :record (:insert :record)
     :keys ((merge (and (toplevel-p) (eql 'minibuffer-ctrl-d *previous-binding*)))))
  (if (and (= 0 (cursor-pos cursor)) (point-eob-p (cursor-point cursor)))
      (minibuffer-close cursor :close)
      (delete-forward-char :n n :register r :merge merge)))

(define-editor-command minibuffer-backspace
    (:cursor cursor :modes (:insert) :n n :register r :record (:insert :record)
     :keys ((merge (and (toplevel-p) (eql 'minibuffer-backspace *previous-binding*)))))
  (if (and (= 0 (cursor-pos cursor)) (point-eob-p (cursor-point cursor)))
      (minibuffer-close cursor :close)
      (delete-backward-char :n n :register r :merge merge)))
