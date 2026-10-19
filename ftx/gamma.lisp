(in-package "FTX")


(define-syntax gamma-ws
  (many (char-class (#\Space #\Tab)))
  (peek (alt (char-class nil (#\Space #\Tab)) (eof))))

(define-syntax gamma-ws-req
  (peek (char-class (#\Space #\Tab)))
  (match gamma-ws))

(define-syntax (gamma-number :trace (parse-integer s))
  (maybe (char-class (#\- #\+)))
  (more (char-class ((#\0 #\9))))
  (peek (alt (char-class nil ((#\0 #\9))) (eof)))
  (trace-range s))

(define-syntax (gamma-ident :trace (intern (string-upcase i)))
  (char-class ((#\A #\Z) (#\a #\z)))
  (many (char-class ((#\A #\Z) (#\a #\z) (#\0 #\9) #\-)))
  (peek (alt (char-class nil ((#\A #\Z) (#\a #\z) (#\0 #\9) #\-)) (eof)))
  (trace-range i))

(macrolet ((def (name string)
             `(define-syntax (,name :group (,name phi-keyword) :trace ',name)
                (match ,string)
                (match phi-symbol-end))))
  (def gamma-syntax-kwd "syntax")
  (def gamma-syntax-macro-kwd "syntax-macro")
  (def gamma-syntax-group-kwd "syntax-group")
  (def gamma-parse-kwd "parse")
  (def gamma-chars-kwd "chars"))

(macrolet ((def (name string)
             `(define-syntax (,name :trace ',name)
                (match ,string))))
  (def gamma-?-op "?")
  (def gamma-*-op "*")
  (def gamma-+-op "+")
  (def gamma-/-op "/")
  (def gamma-=-op "=")
  (def gamma-@-op "@")
  (def gamma-\:-op ":")
  (def gamma-\|-op "|")
  (def gamma-&-op "&")
  (def gamma-~-op "~")
  (def gamma-!-op "!")
  (def gamma-$-op "$")
  (def gamma-.-op ".")
  (def gamma-=@-op "=@")
  (def gamma-!@-op "!@"))

(define-syntax (gamma-char-class-char
                :trace (if c c (case e
                                 ((#\n) #\Newline)
                                 ((#\t) #\Tab)
                                 ((#\r) #\Return)
                                 ((#\0) #\Nul)
                                 (t e))))
  (alt (char-class-trace c nil (#\- #\[ #\] #\\ #\^))
       (progn
         (match "\\")
         (char-class-trace e (#\\ #\[ #\] #\0 #\n #\t #\r #\- #\^)))))

(define-syntax (gamma-char-class-range :trace (list c1 c2))
  (match-trace c1 gamma-char-class-char)
  (match "-")
  (match-trace c2 gamma-char-class-char))

(define-syntax (gamma-char-class-class :trace i)
  (match "[")
  (match-trace i gamma-ident)
  (match "]"))

(define-syntax (gamma-char-class :group gamma-char-class :trace `(char-class ,l))
  (match "[")
  (many (alt (match-trace-list l gamma-char-class-char)
             (match-trace-list l gamma-char-class-range)
             (match-trace-list l gamma-char-class-class)))
  (match "]"))

(define-syntax (char-class-inv :group gamma-char-class :trace `(char-class nil ,l))
  (match "[^")
  (many (alt (match-trace-list l gamma-char-class-char)
             (match-trace-list l gamma-char-class-range)
             (match-trace-list l gamma-char-class-class)))
  (match "]"))

(define-syntax (gamma-match-ident :trace m)
  (match-trace m gamma-ident))

(define-syntax (gamma-var-ident :trace v)
  (match-trace v gamma-ident))

(define-syntax (gamma-expr-ident :group gamma-expr-1 :trace `(match ,i))
  (match-trace i gamma-match-ident))

(define-syntax (gamma-expr-string :group gamma-expr-1 :trace `(match ,s))
  (alt (match-trace s phi-string-1) (match-trace s phi-string-2)))

(define-syntax (gamma-expr-sequence
                :group gamma-expr-1
                :trace (case (length l)
                         (0 nil)
                         (1 (first l))
                         (t `(progn ,@l))))
  (match "(")
  (match gamma-ws)
  (many (progn
          (match-trace-list l gamma-expr)
          (match gamma-ws)))
  (match ")"))

(define-syntax (gamma-expr-maybe :group gamma-expr-1 :trace `(maybe ,g))
  (match-trace g gamma-expr-1)
  (match gamma-ws)
  (match-trace op gamma-?-op))

(define-syntax (gamma-expr-many :group gamma-expr-1 :trace `(many ,g))
  (match-trace g gamma-expr-1)
  (match gamma-ws)
  (match-trace op gamma-*-op))

(define-syntax (gamma-expr-more :group gamma-expr-1 :trace `(more ,g))
  (match-trace g gamma-expr-1)
  (match gamma-ws)
  (match-trace op gamma-+-op))

(define-syntax (gamma-expr-trace-range :group gamma-expr-1 :trace `(trace-range ,i))
  (match-trace op gamma-@-op)
  (match gamma-ws)
  (match-trace i gamma-var-ident))

(define-syntax (gamma-expr-trace-push :group gamma-expr-1 :trace `(trace-push ,i))
  (match-trace op gamma-!-op)
  (match gamma-ws)
  (match-trace i gamma-var-ident))

(define-syntax (gamma-expr-trace-push-list :group gamma-expr-1 :trace `(trace-push-list ,i))
  (match-trace op gamma-!@-op)
  (match gamma-ws)
  (match-trace i gamma-var-ident))

(define-syntax (gamma-expr-match-trace
                :group gamma-expr-1
                :trace (cond (g `(match-trace ,i ,g))
                             (c `(char-class-trace ,i ,@(rest c)))
                             (e `(eof-trace ,i))
                             (a `(any-trace ,i))))
  (match-trace i gamma-var-ident)
  (match gamma-ws)
  (match-trace op gamma-=-op)
  (match gamma-ws)
  (alt (match-trace g gamma-match-ident)
       (match-trace g phi-string-1)
       (match-trace g phi-string-2)
       (match-trace c gamma-char-class)
       (progn (match-trace op gamma-$-op) (trace-push e t))
       (progn (match-trace op gamma-.-op) (trace-push a t))))

(define-syntax (gamma-expr-match-trace-list
                :group gamma-expr-1
                :trace (cond (g `(match-trace-list ,i ,g))
                             (c `(char-class-trace-list ,i ,@(rest c)))
                             (e `(eof-trace-list ,i))
                             (a `(any-trace-list ,i))))
  (match-trace i gamma-var-ident)
  (match gamma-ws)
  (match-trace op gamma-=@-op)
  (match gamma-ws)
  (alt (match-trace g gamma-match-ident)
       (match-trace g phi-string-1)
       (match-trace g phi-string-2)
       (match-trace c gamma-char-class)
       (progn (match-trace op gamma-$-op) (trace-push e t))
       (progn (match-trace op gamma-.-op) (trace-push a t))))

(define-syntax (gamma-expr-any :group gamma-expr-1 :trace '(any))
  (match-trace op gamma-.-op))

(define-syntax (gamma-expr-eof :group gamma-expr-1 :trace '(eof))
  (match-trace op gamma-$-op))

(define-syntax (gamma-expr-char-class :group gamma-expr-1 :trace g)
  (match-trace g gamma-char-class))

(define-syntax (gamma-expr-peek :group gamma-expr-1 :trace `(peek ,g))
  (match-trace op gamma-&-op)
  (match gamma-ws)
  (match-trace g gamma-expr-1))

(define-syntax (gamma-expr-reject :group gamma-expr-1 :trace `(reject ,g))
  (match-trace op gamma-~-op)
  (match gamma-ws)
  (match-trace g gamma-expr-1))

(define-syntax (gamma-expr-alt :group gamma-expr-2 :trace `(,e alt ,@l))
  (match-trace e gamma-expr-1)
  (match gamma-ws)
  (match-trace op gamma-\|-op)
  (match gamma-ws)
  (match-trace l gamma-expr-2))

(define-syntax (gamma-expr-intersperse :group gamma-expr-2 :trace `(,e intersperse ,@l))
  (match-trace e gamma-expr-1)
  (match gamma-ws)
  (match-trace op gamma-/-op)
  (match gamma-ws)
  (match-trace l gamma-expr-2))

(define-syntax (gamma-expr-one :group gamma-expr-2 :trace (list e))
  (match-trace e gamma-expr-1))

(defun apply-gamma-expr-precedence (list)
  (let ((tmp (pop list)))
    (labels ((precedence (sym)
               (ecase sym (intersperse 0) (alt 1)))
             (merge-node (parent op expr)
               (if parent
                   (if (and (eql 'alt op) (eql 'alt (first parent)))
                       (setf (cdr (last parent)) (list expr))
                       (setf (third parent) (list op (third parent) expr)))
                   (if (and (eql 'alt op) (and (consp tmp) (eql 'alt (first tmp))))
                       (setf (cdr (last tmp)) (list expr))
                       (setf tmp (list op tmp expr))))))
      (loop while list
            for op = (pop list)
            and expr = (pop list)
            do (loop for tree = tmp then (third tree)
                     and parent = nil then tree
                     while (and (consp tree) (member (first tree) '(alt intersperse)))
                     do (when (<= (precedence op) (precedence (first tree)))
                          (merge-node parent op expr)
                          (return))
                     finally (merge-node parent op expr)))
      tmp)))

(define-syntax (gamma-expr :trace (apply-gamma-expr-precedence e))
  (match-trace e gamma-expr-2))

(define-syntax (gamma-chars :group phi-stmt-tmp :args (r)
                            :trace (if complete-p
                                       `(define-chars ,name ,@(cdr c))
                                       (error "Incomplete chars statement")))
  (match-trace kwd gamma-chars-kwd)
  (maybe
   (match gamma-ws-req)
   (match-trace name gamma-ident)
   (maybe
    (match gamma-ws-req)
    (match-trace c gamma-char-class)
    (trace-push complete-p t)))
  (phi-stmt-end r))

(define-syntax (gamma-->-keyword :trace :trace)
  (match "->"))

(define-syntax (gamma-=>-keyword :trace :trace-lazy)
  (match "=>"))

(define-syntax (gamma-syntax :group phi-stmt-tmp :args (r)
                             :trace (if (or incomplete-trace-p (not complete-p))
                                        (error "Incomplete syntax statement")
                                        `(define-syntax (,name ,@(when groups-p `(:group ,groups))
                                                               ,@(when args-p `(:args ,args))
                                                               ,@(when trace-op (list trace-op trace-expr)))
                                           ,@(unprognify blk))))
  (match-trace kwd gamma-syntax-kwd)
  (maybe
   (phi-stmt-ws r)
   (match-trace name gamma-ident)
   (maybe (match "(")
          (phi-any-ws)
          (maybe
           (intersperse (match-trace-list args phi-symbol)
                        (progn (phi-any-ws) (match ",") (phi-any-ws)))
           (phi-any-ws) (maybe (match ",") (phi-any-ws)))
          (match ")")
          (trace-push args-p t))
   (maybe (match "[")
          (phi-any-ws)
          (maybe
           (intersperse (match-trace-list groups phi-symbol)
                        (progn (phi-any-ws) (match ",") (phi-any-ws)))
           (phi-any-ws) (maybe (match ",") (phi-any-ws)))
          (match "]")
          (trace-push groups-p t))
   (maybe
    (phi-stmt-ws r)
    (alt (match-trace trace-op gamma-->-keyword)
         (match-trace trace-op gamma-=>-keyword))
    (trace-push incomplete-trace-p t)
    (maybe
     (phi-stmt-ws r)
     (match-trace trace-expr phi-expr-1)
     (trace-push incomplete-trace-p nil)))
   (maybe
    (phi-stmt-ws r)
    (alt (match-trace blk phi-block r) (match-trace blk phi-brace-block))
    (trace-push complete-p t)))
  (phi-stmt-end r))

(define-syntax (gamma-syntax-macro :group phi-stmt-tmp :args (r)
                                 :trace (if complete-p
                                            `(define-syntax-macro ,name ,@(unprognify blk))
                                            (error "Incomplete syntaxule-macro statement")))
  (match-trace kwd gamma-syntax-macro-kwd)
  (maybe
   (phi-stmt-ws r)
   (match-trace name gamma-ident)
   (maybe
    (phi-stmt-ws r)
    (alt (match-trace blk phi-block r) (match-trace blk phi-brace-block))
    (trace-push complete-p t)))
  (phi-stmt-end r))

(define-syntax (gamma-syntax-group :group phi-stmt-tmp :args (r)
                            :trace (if complete-p
                                       `(define-syntax-group ,name)
                                       (error "Incomplete group statement")))
  (match-trace kwd gamma-syntax-group-kwd)
  (maybe
   (phi-stmt-ws r)
   (match-trace name gamma-ident)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (gamma-parse :group phi-stmt-tmp :args (r)
                            :trace (prognify gexprs))
  (match-trace kwd gamma-parse-kwd)
  (many
   (phi-stmt-ws r)
   (match-trace-list gexprs gamma-expr))
  (phi-stmt-end r))
