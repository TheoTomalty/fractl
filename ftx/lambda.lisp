(in-package "FTX")


(define-chars lambda-space "	 
")
(define-chars lambda-tmacro-char "\"'(),;`")
(define-chars lambda-constituent () (lambda-space lambda-tmacro-char #\\ #\|))
(define-chars lambda-token-end () (lambda-constituent #\\ #\|))

(defun char-digit (radix char)
  (let* ((code (char-code char))
         (ret (cond ((<= (char-code #\a) code (char-code #\z))
                     (- code (+ (char-code #\a) 10)))
                    ((<= (char-code #\A) code (char-code #\Z))
                     (- code (+ (char-code #\A) 10)))
                    ((<= (char-code #\0) code (char-code #\9))
                     (- code (char-code #\0)))
                    (t (error "~s is not a valid digit character" char)))))
    (unless (< ret radix)
      (error "~s is not a digit in the radix ~s" char radix))
    ret))

(defun combine-digits (radix digits)
  (let ((sum 0))
    (loop for d in digits
          do (setf sum (+ (* sum radix) (char-digit radix d))))
    (when digits
      sum)))

(defun unescape-symbol-string (s)
  (string-upcase s))

(define-syntax (lambda-sign :trace (if s (ecase s (#\+ 1) (#\- -1)) 1))
  (alt (char-class-trace s (#\+ #\-))
       (peek (alt (char-class ((#\0 #\9) #\.)) (eof)))))

(define-syntax (lambda-exponent :trace (list (ecase e (#\d 'double-float) (#\f 'single-float) (#\e 'float) (#\l 'long-float) (#\s 'short-float))
                                             (combine-digits 10 d)))
  (char-class-trace e (#\d #\f #\e #\l #\s))
  (match-trace s lambda-sign)
  (more (char-class-trace-list d ((#\0 #\9))))
  (peek (alt (char-class () ((#\0 #\9))) (eof))))

(define-syntax (lambda-decimal-integer :trace (* s (combine-digits 10 d)))
  (match-trace s lambda-sign)
  (more (char-class-trace-list d ((#\0 #\9))))
  (match ".")
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-integer :group (lambda-numeric-token) :trace (* s (combine-digits 10 d)))
  (match-trace s lambda-sign)
  (more (char-class-trace-list d ((#\0 #\9))))
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-ratio :group (lambda-numeric-token) :trace (/ (* s (combine-digits 10 d1)) (* s (combine-digits 10 d2))))
  (match-trace s lambda-sign)
  (more (char-class-trace-list d1 ((#\0 #\9))))
  (match "/")
  (more (char-class-trace-list d2 ((#\0 #\9))))
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-float :group (lambda-numeric-token)
                             :trace (coerce (/ (combine-digits 10 (append d1 d2)) (expt 10 (- (length d2) (if e (second e) 0))))
                                            (if e (first e) 'float)))
  (match-trace s lambda-sign)
  (alt (progn
         (many (char-class-trace-list d1 ((#\0 #\9))))
         (match ".")
         (more (char-class-trace-list d2 ((#\0 #\9))))
         (maybe (match-trace e lambda-exponent)))
       (progn
         (more (char-class-trace-list d1 ((#\0 #\9))))
         (maybe
          (match ".")
          (many (char-class-trace-list d2 ((#\0 #\9)))))
         (match-trace e lambda-exponent)))
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-numeric-token :trace d)
  (match-trace d lambda-decimal-integer))

(defun strip-multiple-escape (s)
  (subseq s 1 (- (length s) 1)))

(defun split-colons (parts)
  (loop for part in parts
        append (if (stringp part)
                   (let ((i 0))
                     (loop until (eql i (length part))
                           collect (if (eql #\: (aref part i))
                                       (let ((end (or (position-if-not (lambda (ch) (eql #\: ch)) part :start i)
                                                      (length part))))
                                         (prog1 (list 'colons (- end i)) (setf i end)))
                                       (let ((end (or (position #\: part :start i)
                                                      (length part))))
                                         (prog1 (subseq part i end) (setf i end))))))
                   (list part))))

(define-syntax (lambda-token-constituent :trace s)
  (more (char-class (lambda-constituent #\#)))
  (peek (alt (more (char-class () (lambda-constituent #\#))) (eof)))
  (trace-range s))

(define-syntax (lambda-token-single-escape :trace (if c (list 'escape (string c)) (error "Incomplete token")))
  (match "\\")
  (alt (any-trace c) (eof)))

(define-syntax (lambda-token-multiple-escape :trace (if e (error "Incomplete token") (list 'escape (strip-multiple-escape s))))
  (match "|")
  (many (char-class () (#\|)))
  (alt (match "|") (eof-trace e))
  (trace-range s))

(define-syntax (lambda-token :trace (split-colons p))
  (more
   (alt (match-trace-list p lambda-token-constituent)
        (match-trace-list p lambda-token-single-escape)
        (match-trace-list p lambda-token-multiple-escape)))
  (peek (alt (char-class (lambda-token-end)) (eof))))


(defun colons-part-p (part)
  (and (consp part) (eql 'colons (first part))))

(defun token-string (token)
  (apply #'concatenate 'string
         (loop for part in token
               collect (if (consp part)
                           (ecase (first part)
                             (escape (second part))
                             (colons (error "Unexpected colons")))
                           (string-upcase part)))))

(defun token-symbol (token &optional nointern-p)
  (when (< 1 (count-if #'colons-part-p token))
    (error "Unexpected colons"))
  (let* ((colons-idx (position-if #'colons-part-p token))
         (keyword-package (find-package "KEYWORD"))
         (package (if colons-idx
                      (let ((seq (subseq token 0 colons-idx)))
                        (if seq (token-string seq) keyword-package))
                      *package*))
         (extern nil))
    (when colons-idx
      (case (second (nth colons-idx token))
        (1 (unless (eql package keyword-package)
             (setf extern t)))
        (2 nil)
        (t (error "Too many colons")))
      (setf token (subseq token (1+ colons-idx))))
    (let ((symbol-name (token-string token)))
      (multiple-value-bind (sym type) (find-symbol symbol-name package)
        (when (and extern (not (eql :external type)))
          (error "Symbol ~s is not external in package ~s" symbol-name package))
        (if type
            sym
            (unless nointern-p
              (intern symbol-name package)))))))

(define-syntax (lamda-invalid-token :group (lambda-sexpr lambda-invalid-token) :trace (error "Invalid token"))
  (match "..")
  (many (match "."))
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-dot-token :trace (error "Invalid token"))
  (match ".")
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-number :group (lambda-sexpr) :trace n)
  (match-trace n lambda-numeric-token))

(define-syntax (lambda-symbol :group (lambda-sexpr) :trace (token-symbol token))
  (reject (alt (match lambda-numeric-token)
               (match lambda-dot-token)
               (match lambda-invalid-token)
               (match "#")))
  (match-trace token lambda-token))

;; TODO: technically "#|" can have a numeric prefix (but not inner ones)

(define-syntax lambda-line-comment
  (match ";")
  (many (char-class () (#\Newline)))
  (alt (char-class (#\Newline)) (eof)))

(define-syntax (lambda-block-comment :trace (when e (error "Incomplete block comment")))
  (match "#|")
  (many
   (alt (progn (more (char-class () (#\| #\#)))
               (peek (alt (char-class (#\| #\#)) (eof))))
        (progn (match "|")
               (peek (alt (char-class () (#\#)) (eof))))
        (progn (match "#")
               (peek (alt (char-class () (#\|)) (eof))))
        (match lambda-block-comment)))
  (alt (alt (match "|#") (eof-trace e))))

(define-syntax-macro lambda-whitespace
  (many
   (alt (progn (more (char-class (lambda-space)))
               (peek (alt (char-class () (lambda-space)) (eof))))
        (match-trace comment lambda-line-comment)
        (match-trace comment lambda-block-comment)))
  (peek (alt (char-class () (lambda-space #\; #\#))
             (progn (match "#") (alt (char-class () (#\|)) (eof)))
             (eof))))

;; TODO: This is a horrible hack to implement feature conditional sexprs. To do it properly, need a way to indicate parse dependence on dynamic variables
;;       so that they can be saved in the text object and restored for each parse (need to clear state to parse with new dynamic variables).
;; TODO: earmuffs
(defvar empty-sexpr nil)

(defun nonempty (s)
  (when (eql s 'empty-sexpr)
    (error "Next s-expression is missing due to feature flag"))
  s)

(defun read-sexpr-list (nodes)
  (let* ((list (cons nil nil))
         (head list))
    (loop for cons on nodes
          do (let ((node (car cons)))
               (if (eql 'lambda-dot-token (ftx:ast-symbol node))
                   (progn
                     (unless (and (cdr cons) (null (cddr cons)))
                       (error "Malformed dotted list"))
                     (rplacd head (ftx:eval-ast (cadr cons)))
                     (return nil))
                   (setf head (cdr (rplacd head (list (ftx:eval-ast node))))))))
    (cdr list)))

(define-syntax (lambda-list :group (lambda-sexpr)
                            :trace-lazy (if eof (error "Incomplete list") (read-sexpr-list sexprs)))
  (match "(")
  (match lambda-whitespace)
  (many
   (alt (match-trace-list sexprs lambda-sexpr)
        (match-trace-list sexprs lambda-dot-token))
   (match lambda-whitespace))
  (alt (match ")") (eof-trace eof)))

(define-syntax (lambda-string :group (lambda-sexpr)
                              :trace (progn
                                       (when eof1 (error "Incomplete string escape"))
                                       (when eof2 (error "Unmatched double quote"))
                                       (unescape (subseq s 1 (- (length s) 1)) :substitute nil)))
  (match "\"")
  (many
   (alt (char-class nil (#\" #\\))
        (progn (match "\\") (alt (any) (eof-trace eof1)))))
  (alt (match "\"") (eof-trace eof2))
  (trace-range s))

(define-syntax (lambda-maybe-sexpr :trace (if e (error "missing s-expression") (nonempty s)))
  (alt (match-trace s lambda-sexpr) (match-trace s lambda-dot-token)
       (peek (char-class-trace e (#\)))) (eof-trace e)))

(define-syntax (lambda-quote :group (lambda-sexpr) :trace (list 'quote s))
  (match "'")
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-backquote :group (lambda-sexpr) :trace (list 'quasiquote s))
  (match "`")
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-unquote-1 :group (lambda-sexpr) :trace (unquote s 0))
  (match ",")
  (peek (alt (char-class () (#\@ #\.)) (eof)))
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-unquote-2 :group (lambda-sexpr) :trace (unquote s 1))
  (match ",.")
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))
(define-syntax (lambda-unquote-3 :group (lambda-sexpr) :trace (unquote s 2))
  (match ",@")
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(defun token-character (token)
  (let* ((string (token-string token))
         (name (string-upcase string))
         (char (if (eql (length string) 1)
                   (aref string 0)
                   (name-char name))))
    (unless char (error "no character named ~s" name))
    char))

(defun sexpr-vector (prefix s)
  (when (and prefix (and (> prefix 0) (eql (length s) 0)))
    (error "Not enough elements"))
  (when (and prefix (< prefix (length s)))
    (error "Too many elements"))
  (apply #'vector
         (append s (when (and prefix (> prefix 0))
                     (make-list (- prefix (length s)) :initial-element (nth (- (length s) 1) s))))))

(defun sexpr-bitvector (prefix token)
  (progs
    (unless (eql (length token) 1) (error "invalid bitvector token"))
    (define bits (subseq (nth 0 token) 1))
    (define vec
            (sexpr-vector prefix
                          (loop :for ch :across bits
                                :collect (case ch
                                           ((#\0) 0)
                                           ((#\1) 1)
                                           (t
                                            (error
                                             "Bit vector element must be 0 or 1."))))))
    (return-from sexpr-bitvector
      (make-array (length vec) :element-type 'bit :initial-contents vec))
    (values)))

(defun sexpr-uninterned (token)
  (when (or (not (equalp (nth 0 token) (list 'colons 1)))
            (member-if
             (lambda (elt) (and (listp elt) (eql (nth 0 elt) 'colons)))
             (cdr token)))
    (error "Symbol contains package marker"))
  (make-symbol (token-string (cdr token))))

(define-syntax (lambda-sharp-prefix :trace (combine-digits 10 d))
  (many (char-class-trace-list d ((#\0 #\9))))
  (peek (alt (char-class nil ((#\0 #\9))) (eof))))

(define-syntax (lambda-invalid-sharp :group (lambda-sexpr) :trace (error "invalid sharpsign"))
  (match "#")
  (match lambda-sharp-prefix)
  (peek (alt (char-class () (#\\ #\' #\( #\* #\: #\. #\r #\R #\b #\B #\o #\O
                             #\x #\X #\c #\C #\a #\A #\s #\S #\p #\P #\+ #\-))
             (eof))))

(define-syntax (lambda-character :group (lambda-sexpr) :trace (token-character k))
  (match "#")
  (match lambda-sharp-prefix)
  (peek (match "\\"))
  (match-trace k lambda-token))

(define-syntax (lambda-function :group (lambda-sexpr) :trace (list 'function s))
  (match "#")
  (match lambda-sharp-prefix)
  (match "'")
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-vector :group (lambda-sexpr) :trace (sexpr-vector p s))
  (match "#")
  (match-trace p lambda-sharp-prefix)
  (peek (match "("))
  (match-trace s lambda-sexpr))

(define-syntax (lambda-bitvector :group (lambda-sexpr) :trace (sexpr-bitvector p k))
  (match "#")
  (match-trace p lambda-sharp-prefix)
  (peek (match "*"))
  (match-trace k lambda-token))
(define-syntax (lambda-uninterned :group (lambda-sexpr) :trace (sexpr-uninterned k))
  (match "#")
  (match lambda-sharp-prefix)
  (peek (match ":"))
  (match-trace k lambda-token))

(define-syntax (lambda-eval :group (lambda-sexpr)
                            :trace (if *read-eval* (eval s) (error "Can not read")))
  (match "#")
  (match lambda-sharp-prefix)
  (match ".")
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(defun sexpr-radix (p x)
  (destructuring-bind (d r) x
    (unless p
      (error "Missing radix"))
    (unless (and (<= 2 p) (<= p 36))
      (error "Invalid radix ~s" p))
    (if d d (* (nth 0 r)
               (/ (combine-digits p (nth 1 r)) (combine-digits p (nth 2 r)))))))

(define-syntax (lambda-radix-integer :trace (list s d (list #\1)))
  (match-trace s lambda-sign)
  (more (char-class-trace-list d ((#\0 #\9) (#\a #\z) (#\A #\Z))))
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-radix-ratio :trace (list s d1 d2))
  (match-trace s lambda-sign)
  (more (char-class-trace-list d1 ((#\0 #\9) (#\a #\z) (#\A #\Z))))
  (match "/")
  (more (char-class-trace-list d2 ((#\0 #\9) (#\a #\z) (#\A #\Z))))
  (peek (alt (char-class (lambda-token-end)) (eof))))

(define-syntax (lambda-radix-number :trace (list d r))
  (any)
  (alt (match-trace d lambda-decimal-integer)
       (match-trace r lambda-radix-integer)
       (match-trace r lambda-radix-ratio)))

(define-syntax (lambda-invalid-radix-number :trace (error "invalid number"))
  (reject (match lambda-radix-number))
  (match lambda-token))

(define-syntax (lambda-radix :group (lambda-sexpr) :trace (sexpr-radix prefix x))
  (match "#")
  (match-trace prefix lambda-sharp-prefix)
  (peek (char-class (#\r #\R)))
  (alt (match-trace x lambda-radix-number) (match-trace x lambda-invalid-radix-number)))

(define-syntax (lambda-binary :group (lambda-sexpr) :trace (sexpr-radix 2 x))
  (match "#")
  (match lambda-sharp-prefix)
  (peek (char-class (#\b #\B)))
  (alt (match-trace x lambda-radix-number) (match-trace x lambda-invalid-radix-number)))

(define-syntax (lambda-octal :group (lambda-sexpr) :trace (sexpr-radix 8 x))
  (match "#")
  (match lambda-sharp-prefix)
  (peek (char-class (#\o #\O)))
  (alt (match-trace x lambda-radix-number) (match-trace x lambda-invalid-radix-number)))

(define-syntax (lambda-hex :group (lambda-sexpr) :trace (sexpr-radix 16 x))
  (match "#")
  (match lambda-sharp-prefix)
  (peek (char-class (#\x #\X)))
  (alt (match-trace x lambda-radix-number) (match-trace x lambda-invalid-radix-number)))

(define-syntax (lambda-complex :group (lambda-sexpr)
                               :trace (if (and (listp s) (eql (length s) 2))
                                          (complex (first s) (second s))
                                          (error "Invalid format for complex number")))
  (match "#")
  (match lambda-sharp-prefix)
  (char-class (#\c #\C))
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-array :group (lambda-sexpr)
                             :trace (if p
                                        (make-array (loop for i below p
                                                          and l = (nonempty s) then (car l)
                                                          collect (length l))
                                                    :initial-contents (nonempty s))
                                        (error "Numeric prefix for array sexpressions is required")))
  (match "#")
  (match-trace p lambda-sharp-prefix)
  (char-class (#\a #\A))
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-structure :group (lambda-sexpr) :trace (error "unimplemented"))
  (match "#")
  (match-trace p lambda-sharp-prefix)
  (char-class (#\s #\S))
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-pathname :group (lambda-sexpr) :trace (parse-namestring s))
  (match "#")
  (match lambda-sharp-prefix)
  (char-class (#\p #\P))
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(defun feature-test (s)
  (etypecase s
    (symbol (member s *features*))
    (cons
     (ecase (car s)
       ((:and and) (every #'feature-test (cdr s)))
       ((:or or) (some #'feature-test (cdr s)))))))

(define-syntax (lambda-feature-p :group (lambda-sexpr)
                                 :trace-lazy (if (feature-test
                                                  (let ((*package* (find-package "KEYWORD")))
                                                    (eval-ast (nonempty f))))
                                                 (eval-ast (nonempty s))
                                                 'empty-sexpr))
  (match "#")
  (match lambda-sharp-prefix)
  (match "+")
  (match lambda-whitespace)
  (match-trace f lambda-maybe-sexpr)
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-feature-n :group (lambda-sexpr)
                                 :trace-lazy (if (feature-test
                                                  (let ((*package* (find-package "KEYWORD")))
                                                    (eval-ast (nonempty f))))
                                                 'empty-sexpr
                                                 (eval-ast (nonempty s))))
  (match "#")
  (match lambda-sharp-prefix)
  (match "-")
  (match lambda-whitespace)
  (match-trace f lambda-maybe-sexpr)
  (match lambda-whitespace)
  (match-trace s lambda-maybe-sexpr))

(define-syntax (lambda-unmatched-paren :trace (error "Unmatched parentheses"))
  (match ")"))

(define-syntax (lambda-read :trace-lazy s)
  (match lambda-whitespace)
  (alt (match-trace s lambda-sexpr)
       (match-trace s lambda-dot-token)
       (match-trace s lambda-unmatched-paren)
       (eof)))

(define-syntax (lambda-main :trace sexprs)
  (many
   (progn
     (match lambda-whitespace)
     (alt (match-trace-list sexprs lambda-sexpr)
          (match-trace-list sexprs lambda-dot-token)
          (match-trace-list sexprs lambda-unmatched-paren))))
  (match lambda-whitespace)
  (eof))
