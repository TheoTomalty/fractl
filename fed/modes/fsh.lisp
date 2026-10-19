(in-package "FED")


(define-default-grammar "sh" 'fsh::psi-main)

(define-rule-evaluator fsh::psi-cmd-name paint
  (let ((word (ignore-errors (eval-ast fsh::word))))
    (when (and (= 1 (length word)) (stringp (first word)))
      (if (fsh:shell-fboundp (first word))
          '(:foreground :green)
          '(:foreground :yellow)))))

(defvar *fsh-keymap* (copy-keymap *global-keymap*))
(define-key *fsh-keymap* "RET" (:insert :normal :visual) fsh-accept)
(define-key *fsh-keymap* "M-p" (:insert :normal) repl-prev-history)
(define-key *fsh-keymap* "M-n" (:insert :normal) repl-next-history)

(defun fsh-prompt (buffer)
  (declare (ignore buffer))
  (list "𝜓 " :foreground
        (if (equalp "0" (fsh:shell-variable "?")) :green :red)))

(defun fsh-repl (&optional (switch-p t))
  (let* ((frame (repl-frame))
         (window (repl-frame-window frame))
         (buffer (create-buffer "*fsh*" :window window :default-insert t)))
    (setf (buffer-grammar buffer) (default-grammar "sh")
          (buffer-keymap buffer) *fsh-keymap*)
    (init-repl buffer :prompt #'fsh-prompt)
    (when switch-p
      (switch-to-frame frame))
    buffer))

(define-editor-command fsh-accept
    (:cursor cursor)
  (repl-accept-eval (string cursor)
    (let* ((buffer (cursor-buffer cursor))
           (failed t))
      (unwind-protect
          (handler-case
              (progn
                (eval (evm-parse string (buffer-grammar buffer)))
                (setf failed nil))
            (fsh:shell-error (e)
              (format t "~a~%" e)))
        (when failed
          (fsh:set-exit-code 1))))))

(define-editor-command handle-sigchld ()
  (fsh::%handle-sigchld))

(define-rule-evaluator fsh::psi-word complete-function #'shell-complete)

(defun common-prefix (strings)
  (if strings
      (loop for i from 0
            for ch across (first strings)
            while (loop for str in (cdr strings)
                        always (and (<= (1+ i) (length str))
                                    (char= ch (aref str i))))
            finally (return (subseq (first strings) 0 i)))
      ""))

(defun complete-filename (node)
  (let* ((word (eval-ast node))
         (words (fsh::expand-word word))
         (string (if (cdr words)
                     (format nil "~{~a~^ ~}" (mapcar #'fsh::shell-escape words))
                     (let ((words (fsh::expand-word (append word (list "*")))))
                       (when words
                         (fsh::shell-escape (common-prefix words)))))))
    (when string
      (values t string (length string)))))

(defun complete-command-name (node)
  (let* ((word (eval-ast node))
         (words (fsh::expand-word word))
         (string (unless (= 1 (length words))
                   (unimplemented))))
    (when string
      (values t string (length string)))))

(defun complete-buffer (node)
  (let* ((word (eval-ast node))
         (words (fsh::expand-word word)))
    (when (= 1 (length words))
      (let* ((prefix (first words))
             (string (awhen (loop for buffer in (fed::buffers)
                                  for name = (fed::buffer-name buffer)
                                  when (and (<= (length prefix) (length name))
                                            (equal prefix (subseq name 0 (length prefix))))
                                  collect name)
                       (common-prefix it))))
        (when string (values t string (length string)))))))

(defun shell-complete (cursor node)
  (declare (ignore cursor))
  (when *ast-trace*
    (case (ast-symbol (first *ast-trace*))
      (fsh::psi-cmd-suffix
       ;; TODO: make a better system for custom command completion
       (if (and (cdr *ast-trace*)
                (eql 'fsh::psi-simple-command (ast-symbol (second *ast-trace*)))
                (equal "b" (ast-string (ast-variable (second *ast-trace*) 'fsh::name))))
           (complete-buffer node)
           (complete-filename node)))
      ((fsh::psi-cmd-name fsh::psi-cmd-word)
       (complete-command-name node)))))
