(in-package "FED")


(defun make-registers ()
  (%make-registers
   :letter-registers (let ((tbl (make-hash-table)))
                       (loop for code from (char-code #\a) to (char-code #\z)
                             do (setf (gethash (code-char code) tbl) (make-register)))
                       tbl)
   :number-registers (let ((regs (make-array 10)))
                       (loop for i below 10
                             do (setf (aref regs i) (make-register)))
                       regs)
   :default-register (make-register)
   :delete-register (make-register)))

(defun valid-register-p (name)
  (or (char<= #\a name #\z)
      (char<= #\A name #\Z)
      (char<= #\0 name #\9)
      (and (member name '(#\" #\- #\_)) t)))

(defun get-register (name)
  (cond ((or (char<= #\a name #\z) (char<= #\A name #\Z))
         (gethash (char-downcase name) (registers-letter-registers (registers))))
        ((char<= #\0 name #\9)
         (aref (registers-number-registers (registers))
               (- (char-code name) #.(char-code #\0))))
        ((char= name #\-)
         (registers-delete-register (registers)))
        ((char= name #\")
         (registers-default-register (registers)))
        (t (editor-error "No register named ~s" name))))

(defun get-register-contents (name)
  (if (eql #\_ name)
      (values "" :point)
      (let ((reg (get-register name)))
        (values (register-contents reg) (register-type reg)))))

(defun set-register-contents (name type string &optional force-append-type)
  (declare (type (member nil :before :after) force-append-type))
  (if (eql #\_ name)
      ""
      (let ((reg (get-register name)))
        (setf (register-type reg) type)
        (setf (register-contents reg)
              (cond ((or (eql :after force-append-type)
                         (char<= #\A name #\Z))
                     (concatenate 'string
                                  (register-contents reg)
                                  (when (eql :line type) (list #\Newline))
                                  string))
                    ((eql :before force-append-type)
                     (concatenate 'string
                                  string
                                  (when (eql :line type) (list #\Newline))
                                  (register-contents reg)))
                    (t string))))))

(defun copy-into-register (name string type &optional force-append-type)
  "Fills the register named NAME with STRING.  The default register #\\\" is set
to the contents of the named register unless the delete register or the
block-hole register is named.  If the named register is not one of the 0-9 digit
registers, and not the delete or black-hole registers, then the digit registers
are shifted up and the named register is copied into register #\\0 as well."
  (set-register-contents name type string force-append-type)
  (unless (or (eql #\- name) (eql #\_ name))
    (unless (char<= #\0 name #\9)
      (let ((number-registers (registers-number-registers (registers))))
        (setf (subseq number-registers 1) number-registers)
        (setf (aref number-registers 0) (multiple-value-bind (string type) (get-register-contents name)
                                          (make-register :type type :contents string)))))
    (unless (eql #\" name)
      (multiple-value-bind (string type) (get-register-contents name)
        (set-register-contents #\" type string)))))

(defun yank-aux (cursor p1 p2 type r)
  (let ((string (text-substring (buffer-text (cursor-buffer cursor)) (point-pos p1) (point-pos p2))))
    (copy-into-register r string type))
  (setf (cursor-point cursor) (ecase type (:point p1) (:line (point-back-to-indentation p1)))
        (cursor-snapcol cursor) nil))

(define-operator (yank-operator apply-yank-operator) (p1 p2 type)
    (:cursor cursor :register r)
  ;; COMPAT: always put the cursor at the beginning of yanked text (even for line-wise motions such as j and k)
  (yank-aux cursor p1 p2 type (or r #\")))

(define-editor-command yank-end-of-line
    (:cursor cursor :modes (:normal) :n n :register r)
  ;; COMPAT: Y is bound to yank-end-of-line by default (rather than yanking whole line)
  (yank-aux cursor (cursor-point cursor) (point-end-of-line (cursor-point cursor) n) :point (or r #\")))

(define-editor-command yank-line
    (:cursor cursor :modes (:visual) :n n :register r)
  (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
    (point-beginning-of-line p1)
    (point-end-of-line p2)
    (normal-mode :cursor cursor)
    (yank-aux cursor p1 p2 :line (or r #\"))))

(defun paste-aux (cursor mode n r before-p alt-p)
  (multiple-value-bind (string type) (get-register-contents r)
    (with-undo-group cursor
      (let* ((delete-between nil)
             (then-add-newline nil)
             (insert-point
               (if (eql :visual mode)
                   (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
                     (setf delete-between (list p1 p2 #\_ (cursor-vtype cursor)))
                     (when (and (eql :point (cursor-vtype cursor)) (eql :line type))
                       (setf then-add-newline t))
                     (setf before-p nil)
                     (normal-mode :cursor cursor)
                     p1)
                   (ecase type
                     (:point
                      (if before-p (cursor-point cursor) (cursor-point-after cursor)))
                     (:line
                      (if before-p
                          (point-beginning-of-line (cursor-point cursor))
                          (point-end-of-line (cursor-point cursor)))))))
             (point-and-has-newline-p (and (eql type :point) (position #\Newline string) t))
             (string (ecase type
                       (:point
                        (format nil "~{~a~}" (make-list n :initial-element string)))
                       (:line
                        (format nil "~@[~*~%~]~{~a~^~%~}~@[~*~%~]"
                                (not before-p)
                                (make-list n :initial-element string)
                                before-p))))
             (mark-type (if (or alt-p (and (eql :point type) (not point-and-has-newline-p)))
                            :left-inserting
                            :right-inserting)))
        (with-marks ((cursor-mark (cursor-point cursor))
                     (before insert-point :right-inserting)
                     (after insert-point :left-inserting))
          (with-cursor-properties (cursor :mark-type mark-type :mode :insert)
            (setf (cursor-point cursor) insert-point)
            (when delete-between
              (apply #'buffer-delete-between-copy-register (cursor-buffer cursor) delete-between))
            (when then-add-newline
              (buffer-insert (cursor-buffer cursor) (copy-point before) (string #\Newline)))
            (buffer-insert (cursor-buffer cursor) (copy-point before) string)
            ;; 1. Pasting by line always puts the cursor back to indentation.
            ;;    - Pasting normally puts the cursor at first line
            ;;    - Pasting with alt-p puts the cursor on line after last, if it exists, or last otherwise.
            ;; 2. Pasting point-wise has two options depending on if text contains a newline:
            ;;    - If it does not, put cursor on last character pasted (don't move if none) or after last char if alt-p.
            ;;    - If it does, put cursor on first char or after last char if alt-p
            (setf (cursor-point cursor)
                  (ecase type
                    (:point
                     (cond ((string= "" string)
                            (copy-point cursor-mark))
                           (alt-p
                            (copy-point after))
                           (point-and-has-newline-p
                            (copy-point before))
                           (t
                            (point-before (copy-point after) :normal))))
                    (:line
                     (if alt-p
                         (if before-p
                             (point-back-to-indentation (copy-point after))
                             (point-back-to-indentation (point-end-of-line (copy-point after) 2)))
                         (point-back-to-indentation (point-end-of-line (copy-point before) 2)))))))
          (setf (cursor-snapcol cursor) nil))))))

(define-editor-command paste-forward
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :register r :record (:visual :commit-record :normal :record-simple))
  (paste-aux cursor mode n (or r #\") nil nil))

(define-editor-command paste-backward
    (:cursor cursor :modes (:normal :visual :insert) :mode mode :n n :register r :record (:visual :commit-record :insert :record :normal :record-simple))
  (if (eql :insert mode)
      (with-undo-group cursor
        (buffer-insert (cursor-buffer cursor) (cursor-point cursor)
                       (apply #'concatenate 'string (make-list n :initial-element (get-register-contents (or r #\")))))
        (setf (cursor-snapcol cursor) nil))
      (paste-aux cursor mode n (or r #\") t nil)))

(define-editor-command paste-forward-alt
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :register r :record (:visual :commit-record :normal :record-simple))
  (paste-aux cursor mode n (or r #\") nil t))

(define-editor-command paste-backward-alt
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :register r :record (:visual :commit-record :normal :record-simple))
  (paste-aux cursor mode n (or r #\") t t))
  
(define-editor-command terminal-paste
    (:cursor cursor :modes (:insert :normal :visual :operator) :mode mode :operator operator :record (:insert :record)
     :keys ((text (let ((unpaste (name-key "unpaste")))
                    (coerce (loop for ch = (read-input) until (eql unpaste ch) collect ch) 'string)))))
  (if operator
      (editor-error "Can not paste in operator mode")
      (ecase mode
        (:insert
         (with-undo-group cursor
           (buffer-insert (cursor-buffer cursor) (cursor-point cursor) text)
           (setf (cursor-snapcol cursor) nil)))
        ((:normal :visual)
         (editor-error "unimplemented")))))

(define-editor-command record-register
    (:modes (:normal :visual)
     :keys ((r (unless (recording-register) (read-input)))))
  (unless (or (null r) (and (valid-register-p r) (not (eql #\- r))))
    (editor-error "Invalid register ~s" r))
  (if r
      (let ((reg (get-register r)))
        (setf (register-type reg) :point)
        (unless (char<= #\A r #\Z)
          (setf (register-contents reg) "")))
      (let ((reg (get-register (recording-register))))
        ;; TODO: this is a hack to remove the last 'q' from the recording
        (setf (register-contents reg) (subseq (register-contents reg) 0 (1- (length (register-contents reg)))))))
  (setf (recording-register) r))

(define-editor-command replay-register
    (:modes (:normal :visual) :n n
     :keys ((r (read-input))))
  (when (eql #\@ r)
    (aif (last-replayed-register)
         (setf r it)
         (editor-error "No last replayed register")))
  (unless (valid-register-p r)
    (editor-error "Invalid register ~s" r))
  (setf (last-replayed-register) r)
  (recursive-edit
    (loop repeat n
          do (let ((*pending-input* (loop for key across (get-register-contents r)
                                          collect key)))
               (loop while *pending-input*
                     do (run-key-toplevel (pop *pending-input*)))))))
