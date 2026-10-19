(in-package "FED")


(sb-alien:define-alien-routine ("init_termio" %init-termio) sb-alien:void (input-fd sb-alien:int))
(sb-alien:define-alien-routine ("termio_initialised_p" %termio-initialised-p) sb-alien:int)
(sb-alien:define-alien-routine ("activate_default_termios" %activate-default-termios) sb-alien:void)
(sb-alien:define-alien-routine ("activate_editor_termios" %activate-editor-termios) sb-alien:void)
(sb-alien:define-alien-routine ("write_signal" %write-signal) sb-alien:void (sig sb-alien:int))
(sb-alien:define-alien-routine ("read_input" %read-input) sb-alien:int (timeout sb-alien:int))
(sb-alien:define-alien-routine ("read_cursor_pos" %read-cursor-pos) sb-alien:int)
(sb-alien:define-alien-type nil (sb-alien:struct termsize (x sb-alien:int) (y sb-alien:int)))
(sb-alien:define-alien-routine ("termsize" %termsize) sb-alien:void (size (* (sb-alien:struct termsize))))

(defun read-input-or-signal (timeout)
  (assert (termio-initialised-p))
  (let* ((code (%read-input (if (<= 0 timeout) (truncate (* timeout 1000) internal-time-units-per-second) -1))))
    (when (<= 0 code) (code-char code))))

(defun init-termio (input-fd)
  (assert (not (termio-initialised-p)))
  (%init-termio input-fd)
  (sb-sys:enable-interrupt sb-posix:sigwinch
                           (lambda (sig &rest rest)
                             (declare (ignore rest))
                             (write-signal sig))))


(defun termio-initialised-p ()
  (ecase (%termio-initialised-p) (0 nil) (1 t)))

(defun activate-default-termios ()
  (assert (termio-initialised-p))
  (%activate-default-termios))

(defun activate-editor-termios ()
  (assert (termio-initialised-p))
  (%activate-editor-termios))

(defun write-signal (n)
  (assert (termio-initialised-p))
  (%write-signal n))

(defun read-cursor-pos ()
  (assert (termio-initialised-p))
  (%read-cursor-pos))

(defun termsize ()
  (assert (termio-initialised-p))
  (sb-alien:with-alien ((size (sb-alien:struct termsize)))
    (%termsize (sb-alien:addr size))
    (coord (sb-alien:slot size 'x) (sb-alien:slot size 'y))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sconcat (&rest sequences)
    (apply #'concatenate 'string sequences)))

(define-constant +csi-code+ (sconcat '(#\Esc) "["))

(defun csi-cursor-up (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dA" +csi-code+ n))

(defun csi-cursor-down (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dB" +csi-code+ n))

(defun csi-cursor-forward (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dC" +csi-code+ n))

(defun csi-cursor-backward (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dD" +csi-code+ n))

(defvar *move-absolute* nil)

(defun move-cursor (from to stream)
  (if *move-absolute*
      (progn
        (csi-cursor-set-position (1+ (coord-x to)) (1+ (coord-y to)) stream)
        to)
      (let ((x (- (coord-x to) (coord-x from)))
            (y (- (coord-y to) (coord-y from))))
        (when (/= 0 x)
          (if (< 0 x)
              (csi-cursor-forward x stream)
              (csi-cursor-backward (- x) stream)))
        (when (/= 0 y)
          (if (< 0 y)
              (csi-cursor-down y stream)
              (csi-cursor-up (- y) stream)))
        to)))

(defun csi-cursor-next-line (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dE" +csi-code+ n))

(defun csi-cursor-prev-line (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dF" +csi-code+ n))

(defun csi-cursor-horizontal-absolute (&optional (n 1) (stream *standard-output*))
  (format stream "~a~dG" +csi-code+ n))

(defun csi-cursor-set-position (x y &optional (stream *standard-output*))
  (format stream "~a~d;~dH" +csi-code+ y x))

(defun csi-erase-display-end (&optional (stream *standard-output*))
  (format stream "~a0J" +csi-code+))

(defun csi-erase-display-begin (&optional (stream *standard-output*))
  (format stream "~a1J" +csi-code+))

(defun csi-erase-display (&optional (stream *standard-output*))
  (format stream "~a2J" +csi-code+))

(defun csi-erase-line-end (&optional (stream *standard-output*))
  (format stream "~a0K" +csi-code+))

(defun csi-erase-line-begin (&optional (stream *standard-output*))
  (format stream "~a1K" +csi-code+))

(defun csi-erase-line (&optional (stream *standard-output*))
  (format stream "~a2K" +csi-code+))

(defun csi-sgr (n &optional (stream *standard-output*))
  (format stream "~a~dm" +csi-code+ n))

(defun csi-text-256-foreground (n &optional (stream *standard-output*))
  (format stream "~a38;5;~dm" +csi-code+ n))

(defun csi-text-256-background (n &optional (stream *standard-output*))
  (format stream "~a48;5;~dm" +csi-code+ n))

(defun csi-text-reset       (&optional (stream *standard-output*)) (csi-sgr 0 stream))
(defun csi-text-bold        (&optional (stream *standard-output*)) (csi-sgr 1 stream))
(defun csi-text-italic      (&optional (stream *standard-output*)) (csi-sgr 3 stream))
(defun csi-text-underline   (&optional (stream *standard-output*)) (csi-sgr 4 stream))
(defun csi-text-blink       (&optional (stream *standard-output*)) (csi-sgr 5 stream))
(defun csi-text-invert      (&optional (stream *standard-output*)) (csi-sgr 7 stream))
(defun csi-text-strike      (&optional (stream *standard-output*)) (csi-sgr 9 stream))
(defun csi-text-unbold      (&optional (stream *standard-output*)) (csi-sgr 22 stream))
(defun csi-text-unitalic    (&optional (stream *standard-output*)) (csi-sgr 23 stream))
(defun csi-text-ununderline (&optional (stream *standard-output*)) (csi-sgr 24 stream))
(defun csi-text-unblink     (&optional (stream *standard-output*)) (csi-sgr 25 stream))

(defparameter *ansi-16-colours*
  (let ((tbl (make-hash-table)))
    (setf (gethash :black          tbl) (list 30 40)
          (gethash :red            tbl) (list 31 41)
          (gethash :green          tbl) (list 32 42)
          (gethash :yellow         tbl) (list 33 43)
          (gethash :blue           tbl) (list 34 44)
          (gethash :magenta        tbl) (list 35 45)
          (gethash :cyan           tbl) (list 36 46)
          (gethash :white          tbl) (list 37 47)
          (gethash :default        tbl) (list 39 49)
          (gethash :bright-black   tbl) (list 90 100)
          (gethash :bright-red     tbl) (list 91 101)
          (gethash :bright-green   tbl) (list 92 102)
          (gethash :bright-yellow  tbl) (list 93 103)
          (gethash :bright-blue    tbl) (list 94 104)
          (gethash :bright-magenta tbl) (list 95 105)
          (gethash :bright-cyan    tbl) (list 96 106)
          (gethash :bright-white   tbl) (list 97 107))
    tbl))

(defparameter *ansi-256-colours*
  (let ((tbl (make-hash-table)))
    (setf (gethash :orange tbl) 208)
    tbl))

(defun csi-text-foreground (colour &optional (stream *standard-output*))
  (cond ((gethash colour *ansi-16-colours*)
         (csi-sgr (first (gethash colour *ansi-16-colours*)) stream))
        ((gethash colour *ansi-256-colours*)
         (csi-text-256-foreground (gethash colour *ansi-256-colours*) stream))
        (t (error "invalid colour"))))

(defun csi-text-foreground-default (&optional (stream *standard-output*))
  (csi-sgr 39 stream))

(defun csi-text-background (colour &optional (stream *standard-output*))
  (cond ((gethash colour *ansi-16-colours*)
         (csi-sgr (second (gethash colour *ansi-16-colours*)) stream))
        ((gethash colour *ansi-256-colours*)
         (csi-text-256-background (gethash colour *ansi-256-colours*) stream))
        (t (error "invalid colour"))))

(defun csi-text-background-default (&optional (stream *standard-output*))
  (csi-sgr 49 stream))

(defun csi-report-cursor (&optional (stream sb-sys:*stdout*))
  (format stream "~a6n" +csi-code+)
  (finish-output stream)
  (read-cursor-pos))

(defun csi-set-cursor-type (type &optional (stream *standard-output*))
  (declare (type cursor-type type))
  (let ((n (ecase type
             (:blinking-block     1)
             (:block              2)
             (:blinking-underline 3)
             (:underline          4)
             (:blinking-bar       5)
             (:bar                6))))
    (format stream "~a~d q" +csi-code+ n)))

(defun csi-save-cursor (&optional (stream *standard-output*))
  (format stream "~a7" #\Esc))

(defun csi-restore-cursor (&optional (stream *standard-output*))
  (format stream "~a8" #\Esc))

(defun csi-show-cursor (&optional (stream *standard-output*))
  (format stream "~a?25h" +csi-code+))

(defun csi-hide-cursor (&optional (stream *standard-output*))
  (format stream "~a?25l" +csi-code+))

(defun csi-enable-focus (&optional (stream *standard-output*))
  (format stream "~a?1004h" +csi-code+))

(defun csi-disable-focus (&optional (stream *standard-output*))
  (format stream "~a?1004l" +csi-code+))

(defun csi-enable-alt-screen (&optional (stream *standard-output*))
  (format stream "~a?1049h" +csi-code+))

(defun csi-disable-alt-screen (&optional (stream *standard-output*))
  (format stream "~a?1049l" +csi-code+))

(defun csi-enable-mouse (&optional (stream *standard-output*))
  (format stream "~a?1000h~a?1003h~a?1006h" +csi-code+ +csi-code+ +csi-code+))

(defun csi-disable-mouse (&optional (stream *standard-output*))
  (format stream "~a?1000l~a?1003l~a?1006l" +csi-code+ +csi-code+ +csi-code+))

(defun csi-enable-paste (&optional (stream *standard-output*))
  (format stream "~a?2004h" +csi-code+))

(defun csi-disable-paste (&optional (stream *standard-output*))
  (format stream "~a?2004l" +csi-code+))
