(in-package "FED")


(defparameter *char-mnemonics*
  #("NUL" "RET" "SPC" "TAB" "ESC" "DEL"
    "up-arrow" "down-arrow" "right-arrow" "left-arrow"
    "scroll-up" "scroll-down" "paste" "unpaste"
    "sigwinch" "sigchld"))

(defparameter *mnemonic-chars*
  #(#\Nul #\Return #\Space #\Tab #\Esc #\Del
    #\UE080 #\UE081 #\UE082 #\UE083
    #\UE084 #\UE085 #\UE086 #\UE087
    #\UE21C #\UE211))

(defun char-ext-p (char)
  (char<= #\UE000 char #\UEFFF))

(defun char-meta (char)
  "Get the non-meta character of the given CHAR"
  (let ((code (char-code char)))
    (cond ((char<= #\UE100 char #\UE17F)
           (code-char (logand #x7f code)))
          ((char<= #\UE180 char #\UE1FF)
           (code-char (logior #xe000 (logand code #xff)))))))

(defun meta-char (char)
  "Get the meta character corresponding to the given CHAR"
  (let ((code (char-code char)))
    (cond ((char<= #\U0 char #\U7F)
           (code-char (logior #xe100 code)))
          ((char<= #\UE080 char #\UE0FF)
           (code-char (logior #xe100 (logand code #xff)))))))

(defun char-signal (char)
  "Get the signal corresponding to CHAR, if it exists"
  (when (char<= #\UE200 char #\UE2FF)
    (logand (char-code char) #xff)))

(defun signal-char (sig)
  "Get the character corresponding to the signal SIG, if it exists"
  (when (<= sb-posix:isig sig sb-posix:sigrtmax)
    (logior sig #xe200)))

(defun name-key (name)
  (let ((length (length name)))
    (unless (< 0 length)
      (error "kbd parse error"))
    (or (awhen (position name *char-mnemonics* :test #'string=)
          (aref *mnemonic-chars* it))
        (case (aref name 0)
          (#\C
           (if (< 1 length)
               (or (when (and (char= #\- (aref name 1)) (= 3 length))
                     (caret-char (aref name 2)))
                   (error "kbd parse error"))
               #\C))
          (#\M
           (if (< 1 length)
               (or (when (and (char= #\- (aref name 1)) (<= 3 length))
                     (meta-char (name-key (subseq name 2))))
                   (error "kbd parse error"))
               #\M))
          (t
           (unless (= 1 length)
             (error "kbd parse error"))
           (aref name 0))))))

(defun kbd (keyspec)
  (labels ((parse-keys (keyspec)
             (let ((space-idx (position #\Space keyspec)))
               (if space-idx
                   (cons (name-key (subseq keyspec 0 space-idx))
                         (parse-keys (subseq keyspec (1+ space-idx))))
                   (list (name-key keyspec))))))
    (let ((ret (parse-keys keyspec)))
      (if (and (cdr ret) (loop for char in ret thereis (char-signal char)))
          (error "can not make key combination involving signals")
          ret))))

(defun unkbd (keys)
  (labels ((char-repr (char)
             (or (awhen (position char *mnemonic-chars*)
                   (aref *char-mnemonics* it))
                 (aif (char-meta char)
                      (format nil "M-~a" (char-repr it))
                      (aif (char-caret char)
                           (format nil "C-~c" (char-downcase it))
                           (string char))))))
    (format nil "~{~a~^ ~}" (loop for key in keys collect (char-repr key)))))

(defun immediate-binding (mode keymap key)
  (if keymap
      (multiple-value-bind (binding foundp) (gethash key (gethash mode keymap))
        (unless (symbolp binding)
          (error "binding is not immediate"))
        (values binding foundp))
      (values nil nil)))

(defun make-partial-keymap (mode keymap)
  (when keymap (list (gethash mode keymap))))

(defun partial-keymap-binding (partial-keymap key)
  (let ((keys (nconc (butlast partial-keymap) (list key))))
    (multiple-value-bind (binding foundp)
        (when partial-keymap
          (gethash key (car (last partial-keymap))))
      (if foundp
          (if (hash-table-p binding)
              (values nil (append keys (list binding)) keys)
              (values binding nil keys))
          (values nil nil keys)))))

(defun make-keymap ()
  (make-hash-table))

(defun copy-keymap (keymap)
  (copy-hash-table keymap :copy-value (lambda (val) (if (hash-table-p val) (copy-keymap val) val))))

(defvar *global-keymap*
  (let ((tbl (make-hash-table)))
    (loop for mode in editor-modes
          do (setf (gethash mode tbl) (make-keymap)))
    tbl))

(defun %define-key (keymap keys mode binding)
  (declare (type mode mode))
  (loop while keys
        with tbl = (gethash mode keymap)
        do (if (cdr keys)
               (let ((binding (gethash (car keys) tbl)))
                 (unless (hash-table-p binding)
                   (setf binding (make-keymap)
                         (gethash (car keys) tbl) binding))
                 (setf tbl binding))
               (setf (gethash (car keys) tbl) binding))
           (pop keys)))

(defmacro define-key (keymap key modes binding)
  (let ((keymap-sym (gensym "KEYMAP")))
    `(let ((,keymap-sym ,keymap))
       ,@(loop for mode in (if (listp modes) modes (list modes))
               collect `(%define-key ,keymap-sym (kbd ,key) ,mode ',binding)))))

(defmacro define-key-global (key modes binding)
  `(define-key *global-keymap* ,key ,modes ,binding))
