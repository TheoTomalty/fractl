(in-package "FTX")


(defstruct label
  (symbol nil :type symbol))

(defmacro maybe-or-set (name &body body)
  `(alt (progn ,@body) (trace-push ,name t)))

(macrolet ((def (name string)
             `(define-syntax (,name :group (,name phi-keyword) :trace ',name)
                (match ,string)
                (match phi-symbol-end))))
  (def phi-func-keyword "func")
  (def phi-macro-keyword "macro")
  (def phi-method-keyword "method")
  (def phi-generic-keyword "generic")
  (def phi-if-keyword "if")
  (def phi-elif-keyword "elif")
  (def phi-else-keyword "else")
  (def phi-package-keyword "package")
  (def phi-class-keyword "class")
  (def phi-struct-keyword "struct")
  (def phi-const-keyword "const")
  (def phi-const*-keyword "const*")
  (def phi-type-keyword "type")
  (def phi-condition-keyword "condition")
  (def phi-var-keyword "var")
  (def phi-param-keyword "param")
  (def phi-let-keyword "let")
  (def phi-macrolet-keyword "macrolet")
  (def phi-and-keyword "and")
  (def phi-=-keyword "=")
  (def phi-defer-keyword "defer")
  (def phi-handle-keyword "handle")
  (def phi-restart-keyword "restart"))

(define-chars phi-constituent nil "	 
\\|#@&`'.:,;()[]{}⟨⟩⦗⦘\"“”‘’")
(define-chars phi-punctuation ",.;")
(define-chars phi-operator "#`'@&")
(define-chars phi-close-delim ")]}⟩⦘")

(define-syntax-macro single-slash
  (match "/") (alt (char-class nil (#\/)) (eof)))

(define-syntax (phi-token-part :trace str)
  (more (alt (char-class (phi-constituent) (#\/)) (match single-slash)))
  (peek (alt (char-class nil (phi-constituent)) (match "//") (eof)))
  (trace-range str))
(define-syntax (phi-token-single-escape :trace (if ch `(escape ,(string ch)) (error "Incomplete token")))
  (match "\\") (alt (any-trace ch) (eof)))
(define-syntax (phi-token-multiple-escape :trace (if eof (error "Incomplete token") `(escape ,str)))
  (match "|") (many (char-class nil (#\|))) (alt (match "|") (eof-trace eof)) (trace-range str))
(define-syntax (phi-token-colons :trace `(colons ,(length str)))
  (more (match ":")) (peek (alt (char-class nil (#\:)) (eof))) (trace-range str))
(define-syntax phi-symbol-end
  (peek (maybe (match phi-token-colons)) (alt (char-class nil (phi-constituent #\\ #\| #\:)) (match "//") (eof))))
(define-syntax (phi-number :group (phi-number phi-expr-0) :trace (parse-integer str :radix 10))
  (more (char-class ((#\0 #\9))))
  (match phi-symbol-end)
  (trace-range str))
(define-syntax (phi-symbol :group (phi-symbol phi-expr-0) :trace (token-symbol parts))
  (reject (match phi-number))
  (maybe (match-trace-list parts phi-token-colons))
  (intersperse (more (alt (match-trace-list parts phi-token-part) (match-trace-list parts phi-token-single-escape) (match-trace-list parts phi-token-multiple-escape)))
               (match-trace-list parts phi-token-colons))
  (match phi-symbol-end))

(define-syntax-macro phi-colon-punctuation
  (peek (match phi-token-colons) (match phi-symbol-end))
  (match ":"))

(defun phi-unintern-parts (parts)
  (apply #'concatenate
         'string
         (loop for part in parts
               collect (etypecase part
                         (string (string-upcase part))
                         (cons
                          (assert (eql (first part) 'escape))
                          (destructuring-bind (string) (rest part)
                            string))))))

(define-syntax (phi-uninterned-symbol :trace (phi-unintern-parts parts))
  (reject (match phi-number))
  (more (alt (match-trace-list parts phi-token-part) (match-trace-list parts phi-token-single-escape) (match-trace-list parts phi-token-multiple-escape)))
  (match phi-symbol-end))

(define-syntax phi-comment
  (match "//")
  (many (char-class nil (#\Newline)))
  (peek (alt (match #\Newline) (eof))))

(defun phi-char (str)
  (assert (= 1 (length str)) nil "No char named ~s" str)
  (aref str 0))

(macrolet ((def (name open close &optional (transform 'identity))
             `(define-syntax (,name :group (,name phi-expr-0)
                                    :trace (cond (eof1 (error "Incomplete string escape"))
                                                 ((not closed-p) (error "Unmatched quotation mark"))
                                                 (t (,transform (unescape (subseq str 1 (1- (length str))))))))
                (match ,open)
                (many (alt (char-class nil (,close #\\)) (progn (match "\\") (alt (any) (eof-trace eof1)))))
                (alt (match-trace closed-p ,close) (eof))
                (trace-range str))))
  (def phi-string-1 #\" #\")
  (def phi-string-2 #\“ #\”)
  (def phi-char #\‘ #\’ phi-char))

(macrolet ((def (name open close)
             `(define-syntax (,name :trace (error ,(concatenate 'string "Invalid "
                                                                (string-downcase (subseq (symbol-name name) (1+ (position #\- (symbol-name name) :from-end t)))))))
                (match ,open)
                (many (alt (match phi-number) (match phi-symbol) (match-trace comment phi-comment) (match phi-colon-punctuation)
                           (char-class (#\Space #\Tab #\Newline phi-operator phi-punctuation #\” #\’))
                           (match-trace flbk phi-fallback-parens) (match-trace flbk phi-fallback-brackets)
                           (match-trace flbk phi-fallback-braces) (match-trace flbk phi-fallback-angles) (match-trace flbk phi-fallback-shells)
                           (match-trace flbk phi-string-1) (match-trace flbk phi-string-2) (match-trace flbk phi-char)))
                (alt (match-trace closed-p ,close)
                     (peek (char-class (phi-close-delim) (,close)))
                     (eof)))))
  (def phi-fallback-parens #\( #\))
  (def phi-fallback-brackets #\[ #\])
  (def phi-fallback-braces #\{ #\})
  (def phi-fallback-angles #\⟨ #\⟩)
  (def phi-fallback-shells #\⦗ #\⦘))

(defmacro phi-expr-1-end ()
  `(peek (alt (char-class (#\Space #\Tab #\Newline phi-punctuation phi-close-delim))
              (match phi-colon-punctuation)
              (match "//")
              (eof))))

(define-syntax (phi-expr-1-tmp :trace expr)
  (match-trace expr phi-expr-0)
  (phi-expr-1-end))

(define-syntax (phi-fallback-expr-1 :trace (error "Invalid expression ~s" s))
  (reject (match phi-expr-1-tmp))
  (more (alt (match phi-number) (match phi-symbol) (char-class (phi-operator #\” #\’))
             (match-trace flbk phi-fallback-parens) (match-trace flbk phi-fallback-brackets)
             (match-trace flbk phi-fallback-braces) (match-trace flbk phi-fallback-angles)
             (match-trace flbk phi-fallback-shells)
             (match-trace flbk phi-string-1) (match-trace flbk phi-string-2) (match-trace flbk phi-char)))
  (phi-expr-1-end)
  (trace-range s))

(define-syntax (phi-expr-1 :trace expr)
  "A sequence of one or more structured tokens other than colon, comma,
semicolon, or any closing structured delimiter, not separated by
whitespace.  Ends with one such not-included structured token or any
whitespace including comments."
  (alt (match-trace expr phi-expr-1-tmp) (match-trace expr phi-fallback-expr-1)))

(define-syntax phi-inline-ws
  (many (alt (char-class (#\Space #\Tab)) (match-trace comment phi-comment)))
  (peek (alt (char-class nil (#\Space #\Tab #\/)) (match single-slash) (eof))))

(define-syntax phi-any-ws
  (many (alt (char-class (#\Space #\Tab #\Newline)) (match-trace comment phi-comment)))
  (peek (alt (char-class nil (#\Space #\Tab #\Newline #\/)) (match single-slash) (eof))))

(defmacro phi-any-ws ()
  `(match-trace ws phi-any-ws))

(defmacro phi-inline-ws ()
  `(match-trace ws phi-inline-ws))

(define-syntax (phi-indent :return t)
  "A maximal region of whitespace (and comments) that includes at least
one newline.  Returns the amount of indentation before the next
non-whitespace character."
  (phi-inline-ws)
  (match #\Newline)
  (let ((i 0))
    (loop do (alt
              (progn (match #\Space) (incf i))
              (progn (match #\Tab) (incf i 4))
              (progn (match #\Newline) (setf i 0))
              (match-trace comment phi-comment)
              (progn
                (peek (alt (char-class nil (#\Space #\Tab #\Newline #\/))
                           (match single-slash)))
                (return i))
              (progn
                (eof)
                (return 0))))))

(defmacro phi-indent ()
  `(match-trace ws phi-indent))

(define-syntax (phi-stmt-ws :args (r))
  (alt (progn (phi-inline-ws) (peek (alt (char-class nil (#\Newline)) (eof))))
       (unless (< r (phi-indent)) (stop))))

(defmacro phi-stmt-ws (r)
  `(match-trace ws phi-stmt-ws ,r))

(define-syntax (phi-stmt-end :args (r))
  (alt (progn (phi-stmt-ws r) (match ";"))
       (peek (alt (progn (match phi-any-ws) (alt (match "}") (eof)))
                  (unless (<= (match phi-indent) r) (stop))))))

(defmacro phi-stmt-end (r)
  `(match phi-stmt-end ,r))

(define-syntax (phi-stmts-end :args (r))
  (peek (alt (progn (match phi-any-ws) (alt (match "}") (eof)))
             (unless (< (match phi-indent) r) (stop)))))

(defmacro phi-stmts-end (r)
  `(match phi-stmts-end ,r))

(define-syntax (phi-expr-stmt :args (r) :trace expr)
  (reject (alt (match phi-keyword) (match "#[") (match "@")))
  (match-trace expr phi-exprs-stmt-ws r)
  (phi-stmt-end r))

(define-syntax (phi-fallback-stmt :args (r) :trace (error "Invalid statement ~s" str))
  (intersperse (alt (match-trace-list exprs phi-expr-1)
                    (char-class (phi-punctuation phi-close-delim) (#\; #\}))
                    (match phi-colon-punctuation))
               (phi-stmt-ws r))
  (phi-stmt-end r)
  (trace-range str))

(defmacro phi-stmt-impl (stmt stmt-tmp-rule r)
  `(alt (match ";")
        ,@(loop for rule in (listify stmt-tmp-rule)
                collect `(match-trace ,stmt ,rule ,r))
        (progn
          (reject (alt ,@(loop for rule in (listify stmt-tmp-rule)
                               collect `(match ,rule ,r))))
          (match-trace ,stmt phi-fallback-stmt ,r))))

(define-syntax (phi-stmt :args (r) :trace stmt)
  "A sequence of one or more PHI-EXPR-1, or any of the structured tokens
not included in the PHI-EXPR-1 list other than the semicolon or the
closing brace, and separated by optional whitespace that must not
constitute an indentation of less than or equal to R.  There are three
ways a statement can end: (1) Whitespace of the above type followed by
a semicolon, in which case the semicolon is included; (2) Any
whitespace followed by a closing brace or EOF, neither of which is
included; (3) Whitespace constituting an indentation equal to or less
than R, not included.  An empty statement is also permitted only if it
is a lone semicolon."
  (phi-stmt-impl stmt (phi-stmt-tmp phi-expr-stmt) r))

(define-syntax (phi-package-stmt :args (r) :trace stmt)
  (phi-stmt-impl stmt phi-package-stmt-tmp r))

(define-syntax (phi-class-stmt :args (r) :trace stmt)
  (phi-stmt-impl stmt phi-class-stmt-tmp r))

(define-syntax (phi-struct-stmt :args (r) :trace stmt)
  (phi-stmt-impl stmt phi-struct-stmt-tmp r))

(define-syntax (phi-generic-stmt :args (r) :trace stmt)
  (phi-stmt-impl stmt phi-generic-stmt-tmp r))

(define-syntax (phi-stmt-separator :args (r))
  (alt (progn (phi-inline-ws) (peek (alt (char-class nil (#\Newline)) (eof))))
       (unless (<= r (phi-indent)) (stop))))

(defmacro phi-stmt-separator (r)
  `(match-trace ws phi-stmt-separator ,r))

(defmacro phi-stmts-impl (stmts stmt-rule r)
  `(progn
     (intersperse (match-trace-list ,stmts ,stmt-rule ,r)
                  (phi-stmt-separator ,r))
     (phi-stmts-end ,r)))

(define-syntax (phi-stmts :args (r) :trace stmts)
  "A sequence of one or more PHI-STMT, separated by optional whitespace
that must not constitute an indentation of less than R.  Ends either
with indentation of less than R, or any whitespace before a closing
brace or EOF."
  (phi-stmts-impl stmts phi-stmt r))

(define-syntax (phi-package-stmts :args (r) :trace stmts)
  (phi-stmts-impl stmts phi-package-stmt r))

(define-syntax (phi-class-stmts :args (r) :trace stmts)
  (phi-stmts-impl stmts phi-class-stmt r))

(define-syntax (phi-struct-stmts :args (r) :trace stmts)
  (phi-stmts-impl stmts phi-struct-stmt r))

(define-syntax (phi-generic-stmts :args (r) :trace stmts)
  (phi-stmts-impl stmts phi-generic-stmt r))

(defun phi-prognify (stmts)
  (if (loop for stmt in stmts thereis (label-p stmt))
      `(tagbody*
         ,@(loop for cons on stmts for stmt = (car cons)
                 collect (typecase stmt
                           (label (label-symbol stmt))
                           (symbol `(progn ,stmt))
                           (t stmt))))
      (prognify stmts)))

(defmacro phi-block-impl (stmts stmts-rule r)
  (with-gensyms (rn)
    `(progn
       (match ":")
       (alt (progn (phi-inline-ws) (eof))
            (let ((,rn (peek (phi-indent))))
              (when (< ,r ,rn)
                (phi-indent)
                (match-trace ,stmts ,stmts-rule ,rn)))))))

(define-syntax (phi-block :args (r) :trace (phi-prognify stmts))
  (phi-block-impl stmts phi-stmts r))

(define-syntax (phi-package-block :args (r) :trace stmts)
  (phi-block-impl stmts phi-package-stmts r))

(define-syntax (phi-class-block :args (r) :trace stmts)
  (phi-block-impl stmts phi-class-stmts r))

(define-syntax (phi-struct-block :args (r) :trace stmts)
  (phi-block-impl stmts phi-struct-stmts r))

(define-syntax (phi-generic-block :args (r) :trace stmts)
  (phi-block-impl stmts phi-generic-stmts r))

(defmacro phi-brace-block-impl (stmts stmts-rule)
  (with-gensyms (rn)
    `(progn
       (match "{")
       (alt (progn
              (phi-inline-ws) (peek (alt (char-class nil (#\Newline)) (eof)))
              (match-trace ,stmts ,stmts-rule 0))
            (let ((,rn (phi-indent)))
              (match-trace ,stmts ,stmts-rule ,rn)))
       (phi-any-ws)
       (alt (match "}") (eof)))))

(define-syntax (phi-brace-block :group (phi-brace-block phi-expr-0) :trace (phi-prognify stmts))
  "A delimited container for PHI-STMTS.  If the block starts with
indentation whitespace, then the statements are parsed using that
indentation level.  Otherwise, they are parsed using indentation level
0, meaning that any indentation indicates a continuation of the
previous statement unless an explicit semicolon is used."
  (phi-brace-block-impl stmts phi-stmts))

(define-syntax (phi-package-brace-block :trace stmts)
  (phi-brace-block-impl stmts phi-package-stmts))

(define-syntax (phi-class-brace-block :trace stmts)
  (phi-brace-block-impl stmts phi-class-stmts))

(define-syntax (phi-struct-brace-block :trace stmts)
  (phi-brace-block-impl stmts phi-struct-stmts))

(define-syntax (phi-generic-brace-block :trace stmts)
  (phi-brace-block-impl stmts phi-generic-stmts))

(define-syntax (phi-read :trace-lazy (cond (close-brace (error "Unmatched closing brace"))
                                           ((label-p stmt) (error "Label at toplevel"))
                                           (t stmt)))
  (phi-any-ws)
  (alt (match-trace stmt phi-stmt 0) (match-trace close-brace "}") (eof)))

(define-syntax (phi-main :trace (if close-brace (error "Unmatched closing brace") (phi-prognify stmts)))
  (many (phi-any-ws) (alt (match-trace-list stmts phi-stmt 0) (match-trace close-brace "}")))
  (phi-any-ws)
  (eof))

(defun fractl-eval (text)
  (evm-eval text 'phi-read))

(defun fractl-load (filespec)
  (evm-load filespec 'phi-read))

;; Simple Expressions

(define-syntax (phi-=>-keyword :trace 'phi-=>-keyword)
  (match "=>")
  (match phi-symbol-end))

(macrolet ((def (name expr ws end)
             (setf name (listify name))
             `(define-syntax (,@name :trace (if lambda-p
                                                `(lambda ,(if args (first args) (list arg))
                                                   ,@(phi-types-to-declares (second args))
                                                   ,@(unprognify expr))
                                                (phi-apply-precedence exprs)))
                (alt (progn
                       (reject (match phi-expr-1) ,ws (match-trace kwd phi-=>-keyword))
                       (intersperse ,expr ,ws))
                     (progn
                       (alt (match-trace args phi-arguments)
                            (match-trace arg phi-symbol))
                       ,ws
                       (match-trace kwd phi-=>-keyword)
                       (trace-push lambda-p t)
                       (maybe
                        ,ws
                        (match-trace expr ,(first name) ,@(getf (rest name) :args)))))
                ,end)))
  (def phi-exprs-any-ws
       (match-trace-list exprs phi-expr-1)
       (phi-any-ws)
       (peek (match phi-any-ws) (alt (char-class (phi-punctuation phi-close-delim)) (eof))))
  (def (phi-exprs-stmt-ws :args (r))
       (alt (match-trace-list exprs phi-expr-1) (match-trace-list exprs phi-block r))
       (phi-stmt-ws r)
       (peek (alt (phi-stmt-end r)
                  (progn (phi-stmt-ws r) (match ","))))))

(define-syntax (phi-expr-component-1 :group phi-expr-component :trace `(single ,expr))
  (match-trace expr phi-exprs-any-ws))
(define-syntax (phi-expr-component-2 :group phi-expr-component :trace `(splice ,expr))
  (match "...")
  (phi-any-ws)
  (match-trace expr phi-exprs-any-ws))
(define-syntax (phi-keyword-component :trace-lazy (token-symbol (list* '(colons 1) (mapcar #'eval-ast (ast-variable sym 'parts)))))
  (match-trace sym phi-symbol)
  (match #\:))
(define-syntax (phi-expr-component-3
                :group phi-expr-component
                :trace (if expr-p
                           `(keyword ,kwd ,expr)
                           (error "Missing expression in keyword component")))
  (match-trace kwd phi-keyword-component)
  (maybe
   (phi-any-ws)
   (match-trace expr phi-exprs-any-ws)
   (trace-push expr-p t)))

(defun phi-transform-values (components)
  (if (member 'splice components :key #'first)
      (if (= 1 (length components))
          (second (first components))
          `(multiple-value-call #'values
             ,@(loop for component in components
                     append (ecase (first component)
                              (single (list `(values ,(second component))))
                              (keyword (list (second component) `(values ,(third component))))
                              (splice (list (second component)))))))
      (let ((exprs (loop for component in components append (rest component))))
        (if (= 1 (length exprs))
            (first exprs)
            `(values ,@exprs)))))

(defun phi-transform-list (components)
  (aif (member 'splice components :key #'first)
       (if (cdr it)
           `(append
             ;; TODO: group adjacent list calls together
             ,@(loop for component in components
                     collect (ecase (first component)
                               (single `(list ,(second component)))
                               (keyword `(list ,(second component) ,(third component)))
                               (splice (second component)))))
           (let ((butlast (loop for component in (butlast components) append (rest component)))
                 (last (second (car (last components)))))
             (if butlast
                 `(list* ,@butlast ,last)
                 last)))
       `(list ,@(loop for component in components append (rest component)))))

(defun phi-transform-vector (components)
  (if (member 'splice components :key #'first)
      (error "Splice operator not supported for vectors")
      `(vector ,@(loop for component in components append (rest component)))))

(macrolet ((def (name open close transform)
             `(define-syntax (,name :group (,name phi-expr-0) :trace (,transform components))
                (match ,open)
                (phi-any-ws)
                (maybe
                 (intersperse (match-trace-list components phi-expr-component)
                              (progn (phi-any-ws) (match ",") (phi-any-ws)))
                 (phi-any-ws) (maybe (match ",") (phi-any-ws)))
                (match ,close))))
  (def phi-parens-expr #\( #\) phi-transform-values)
  (def phi-list-expr #\[ #\] phi-transform-list)
  (def phi-vector-expr #\⟨ #\⟩ phi-transform-vector))

;; Compound Expressions

(defun phi-transform-funcall (sym components &optional funcall-p)
  (let ((list-expr (phi-transform-list components))
        (sym-expr (if funcall-p sym `#',sym)))
    (ecase (first list-expr)
      (list (if funcall-p
                `(funcall ,sym ,@(rest list-expr))
                `(,sym ,@(rest list-expr))))
      (list* `(apply ,sym-expr ,@(rest list-expr)))
      (append
       (if (eql 'list (first (second list-expr)))
           `(apply ,sym-expr ,@(rest (second list-expr)) (append ,@(cddr list-expr)))
           `(apply ,sym-expr ,list-expr))))))

(define-syntax (phi-function-call
                :group phi-expr-0
                :trace-lazy (phi-transform-funcall
                             (eval-ast sym)
                             (mapcar #'eval-ast (ast-variable parens-expr 'components))))
  (match-trace sym phi-symbol)
  (match-trace parens-expr phi-parens-expr))

(define-syntax (phi-function-funcall
                :group phi-expr-0
                :trace-lazy (phi-transform-funcall
                             (eval-ast expr)
                             (mapcar #'eval-ast (ast-variable parens-expr 'components))
                             t))
  (reject (match phi-symbol))
  (match-trace expr phi-expr-0)
  (match-trace parens-expr phi-parens-expr))

(defun phi-nth-expression (index expr)
  (if (and (integerp index) (<= 0 index 9))
      `(,(aref #(first second third fourth fifth sixth seventh eighth ninth tenth) index) ,expr)
      `(nth ,index ,expr)))

(define-syntax (phi-nth-index :trace (if incomplete-p
                                         (error "Incomplete list index")
                                         index))
  (match "[")
  (phi-any-ws)
  (maybe-or-set incomplete-p
    (match-trace index phi-exprs-any-ws)
    (phi-any-ws))
  (match "]"))

(define-syntax (phi-nth :group phi-expr-0 :trace (phi-nth-expression index expr))
  (match-trace expr phi-expr-0)
  (match-trace index phi-nth-index))

(define-syntax (phi-aref-index :trace indices)
  (match "⟨")
  (phi-any-ws)
  (maybe
   (intersperse (match-trace-list indices phi-exprs-any-ws)
                (progn (phi-any-ws) (match ",") (phi-any-ws)))
   (phi-any-ws)
   (maybe (match ",") (phi-any-ws)))
  (match "⟩"))

(define-syntax (phi-aref :group phi-expr-0 :trace `(aref ,expr ,@indices))
  (match-trace expr phi-expr-0)
  (match-trace indices phi-aref-index))

(define-syntax (phi-gethash-index :trace (if incomplete-p
                                             (error "Incomplete hash-table index")
                                             index))
  (match "⦗")
  (phi-any-ws)
  (maybe-or-set incomplete-p
    (match-trace index phi-exprs-any-ws)
    (phi-any-ws))
  (match "⦘"))

(define-syntax (phi-gethash :group phi-expr-0 :trace `(gethash ,index ,expr))
  (match-trace expr phi-expr-0)
  (match-trace index phi-gethash-index))

(define-syntax (phi-function-ident :group phi-expr-0 :trace `#',sym)
  (match "#'")
  (match-trace sym phi-symbol))

(define-syntax (phi-function-sexpr :group phi-expr-0 :trace `#',sexpr)
  (match "#'")
  (peek (match "("))
  (match-trace sexpr lambda-sexpr))

(define-syntax (phi-quote-ident :group phi-expr-0 :trace `(quote ,sym))
  (match "'")
  (match-trace sym phi-symbol))

(define-syntax (phi-quote-sexpr :group phi-expr-0 :trace `(quote ,sexpr))
  (match "'")
  (peek (match "("))
  (match-trace sexpr lambda-sexpr))

(define-syntax (phi-quasiquote-ident :group phi-expr-0 :trace (list 'sb-sys::quasiquote sym))
  (match "`")
  (match-trace sym phi-symbol))

(define-syntax (phi-quasiquote-sexpr :group phi-expr-0 :trace (list 'sb-sys::quasiquote sexpr))
  (match "`")
  (peek (match "("))
  (match-trace sexpr lambda-sexpr))

(define-syntax (phi-lisp :group phi-expr-0 :trace sexpr)
  (match "#")
  (peek (match "("))
  (match-trace sexpr lambda-sexpr))

;; Operators

(defun %define-phi-operator (symbol precedence unary-p right-associative-p merge num-args)
  (setf (get symbol 'phi-operator-precedence) precedence)
  (when unary-p
    (setf (get symbol 'phi-unary-operator-p) t))
  (when right-associative-p
    (setf (get symbol 'phi-right-associative-operator-p) t))
  (when merge
    (setf (get symbol 'phi-operator-merge) merge))
  (when (< 0 num-args)
    (setf (get symbol 'phi-num-operator-args) num-args))
  symbol)

(defmacro define-phi-operator (symbol precedence &rest keys &key unary-p right-associative-p merge (num-args 0))
  (if (listp symbol)
      `(progn
         ,@(loop for symbol in symbol collect `(define-phi-operator ,symbol ,precedence ,@keys)))
      `(%define-phi-operator ',symbol ,precedence ,unary-p ,right-associative-p ,merge ,num-args)))

(defconstant-eqx inequality-operators '(= is == === ==== < <= >= >))

(define-phi-operator -> 0 :right-associative-p t)
(define-phi-operator (return assert) 1 :unary-p t :right-associative-p t)
(define-phi-operator return-from 1 :num-args 1 :unary-p t :right-associative-p t)
(define-phi-operator <- 2 :right-associative-p t)
(define-phi-operator or 3 :merge 'phi-merge-associative)
(define-phi-operator and 4 :merge 'phi-merge-associative)
(define-phi-operator not 5 :unary-p t :right-associative-p t)
(define-phi-operator #.inequality-operators 6 :merge 'phi-merge-inequalities)
(define-phi-operator + 7 :merge 'phi-merge-associative)
(define-phi-operator - 7 :merge 'phi-merge-left-associative)
(define-phi-operator * 8 :merge 'phi-merge-associativ)
(define-phi-operator / 8 :merge 'phi-merge-left-associativ)
(define-phi-operator % 8)

(defun phi-merge-associative (symbol lhs rhs)
  `(,symbol ,@(if (and (consp lhs) (eql symbol (first lhs))) (rest lhs) (list lhs))
            ,@(if (and (consp rhs) (eql symbol (first rhs))) (rest rhs) (list rhs))))

(defun phi-merge-left-associative (symbol lhs rhs)
  `(,symbol ,@(if (and (consp lhs) (eql symbol (first lhs))) (rest lhs) (list lhs)) ,rhs))

(defun phi-merge-inequalities (symbol lhs rhs)
  (let ((operators nil)
        (arguments nil))
    (labels ((aux (expr)
               (cond ((and (consp expr) (eql 'phi-chained-inequality (first expr)))
                      (setf operators (nconc operators (second expr))
                            arguments (nconc arguments (cddr expr))))
                     ((and (consp expr) (member (first expr) inequality-operators))
                      (assert (cdr expr))
                      (setf operators (nconc operators (make-list (1- (length (cdr expr))) :initial-element (first expr)))
                            arguments (nconc arguments (cdr expr))))
                     (t
                      (setf arguments (nconc arguments (list expr)))))))
      (aux lhs)
      (setf operators (nconc operators (list symbol)))
      (aux rhs)
      (cond ((and (member (first operators) '(= < <= >= >))
                  (loop for op in (rest operators) always (eql op (first operators ))))
             `(,(first operators) ,@arguments))
            ((= 1 (length operators))
             `(,(first operators) ,@arguments))
            (t
             `(phi-chained-inequality ,operators ,@arguments))))))

(defun phi-operator-precedence (symbol)
  (and (symbolp symbol) (get symbol 'phi-operator-precedence)))

(defun phi-operator-p (symbol)
  (and (phi-operator-precedence symbol) t))

(defun phi-operator-merge (symbol)
  (and (phi-operator-p symbol) (get symbol 'phi-operator-merge)))

(defun phi-operator-unary-p (symbol)
  (and (phi-operator-p symbol) (get symbol 'phi-unary-operator-p)))

(defun phi-operator-right-associative-p (symbol)
  (and (phi-operator-p symbol) (get symbol 'phi-right-associative-operator-p)))

(defun phi-operator-num-args (symbol)
  (and (phi-operator-p symbol) (or (get symbol 'phi-num-operator-args) 0)))

(defun phi-apply-precedence (exprs)
  (when exprs
    (let (;; List of (precedence symbol lhs)
          (tree nil)
          (exprs exprs))
      (labels ((make-entry (precedence operator args) (list* precedence operator args))
               (make-final-entry (args) (make-entry -1 nil args))
               (entry-precedence (entry) (first entry))
               (entry-operator (entry) (second entry))
               (entry-args (entry) (cddr entry))
               (setloc (loc new-tree)
                 (if loc
                     (setf (cdr loc) new-tree)
                     (setf tree new-tree)))
               (insert-tree (tree operator operator-args rhs rhs-p &optional loc)
                 (assert tree)
                 (let ((entry (first tree))
                       (precedence (phi-operator-precedence operator))
                       (unary-p (phi-operator-unary-p operator))
                       (right-associative-p (phi-operator-right-associative-p operator)))
                   (assert precedence)
                   (if (or (= -1 (entry-precedence entry))
                           (if right-associative-p
                               (< precedence (entry-precedence entry))
                               (<= precedence (entry-precedence entry))))
                       (setloc loc
                               (list (make-entry precedence operator
                                                 (append operator-args
                                                         (if unary-p
                                                             (assert (and (= 1 (length tree)) (null (entry-args (first tree))))
                                                                     nil "Unary operator does not take a left-hand expression")
                                                             (list (reduce-tree tree)))))
                                     (make-final-entry (when rhs-p (list rhs)))))
                       (insert-tree (cdr tree) operator operator-args rhs rhs-p tree))))
               (reduce-tree (tree)
                 (assert tree)
                 (let ((entry (first tree)))
                   (if (= -1 (entry-precedence entry))
                       (aif (entry-args entry)
                            (progn (assert (= 1 (length it))) (first it))
                            (error "Missing right-hand expression"))
                       (let ((ret (append (list* (entry-operator entry)
                                                 (entry-args entry))
                                          (list (reduce-tree (cdr tree))))))
                         (aif (phi-operator-merge (first ret))
                              (apply it ret)
                              ret)))))
               (convert-case-val (val)
                 (if (consp val)
                     (progn
                       (assert (and (eql 'values (first val))
                                    (loop for subval in (rest val) always (symbolp subval)))
                               nil "Invalid case specifier ~s" val)
                       (rest val))
                     (if (member val '(t nil))
                         (list val)
                         val)))
               (case-stmt-p (stmt)
                 (and (consp stmt) (eql '-> (first stmt))))
               (process-case-macro (form)
                 (if (and (consp form) (member (first form) '(case ecase typecase etypecase)))
                     (let* ((expr (progn (assert (cdr form) nil "Missing arguments to ~s" (first form))
                                         (second form)))
                            (stmts (cddr form))
                            (pos (or (position-if-not #'case-stmt-p stmts) (length stmts)))
                            (cases (subseq stmts 0 pos))
                            (default (phi-prognify (subseq stmts pos))))
                       (assert (not (some #'case-stmt-p default))
                               nil "Case statement found in default block")
                       `(,(ecase (first form)
                            ((case ecase) (if default 'case 'ecase))
                            ((typecase etypecase) (if default 'typecase 'etypecase)))
                         ,expr
                         ,@(loop for stmt in cases
                                 collect (list* (if (member (first form) '(case ecase))
                                                    (convert-case-val (second stmt))
                                                    (second stmt))
                                                (unprognify (third stmt))))
                         ,@(when default
                             (list `(t ,@(unprognify default))))))
                     form))
               (functionify-forms (forms)
                 (assert (< 0 (length forms)))
                 (block nil
                   (when (symbolp (first forms))
                     (let* ((lambda-list (handler-case (sb-introspect:function-lambda-list (first forms))
                                           (undefined-function ())))
                            (pos (position-if (lambda (x) (member x lambda-list-keywords)) lambda-list)))
                       (when (and pos
                                  (eql '&body (nth pos lambda-list))
                                  (= (1+ pos) (length (rest forms))))
                         (return (process-case-macro (nconc (butlast forms) (unprognify (lastcar forms))))))))
                   forms))
               (convert-loop-var (form)
                 (cond ((symbolp form)
                        form)
                       ((and (consp form) (eql 'list (first form)))
                        (mapcar #'convert-loop-var (rest form)))
                       ((and (consp form) (eql 'list* (first form)))
                        (nconc (mapcar #'convert-loop-var (butlast (rest form)))
                               (convert-loop-var (lastcar (rest form)))))
                       (t (error "Invalid var clause in LOOP"))))
               (process-loop-macro (form)
                 (let ((ret (copy-list form)))
                   (loop for cons on ret for subform = (car cons)
                         do (when (and (symbolp subform)
                                       (member (symbol-name subform) '("FOR" "WITH" "AND") :test #'string=)
                                       (cdr cons))
                              (setf (cadr cons) (convert-loop-var (cadr cons)))))
                   ret))
               (pop-compound-form ()
                 (if (eql 'loop (first exprs))
                     (values (process-loop-macro (shiftf exprs nil)) t)
                     (let ((forms (loop while (and exprs (not (phi-operator-p (first exprs))))
                                        collect (pop exprs))))
                       (case (length forms)
                         (0 (values nil nil))
                         (1 (values (first forms) t))
                         (t (values (functionify-forms forms) t)))))))
        (multiple-value-bind (form form-p) (pop-compound-form)
          (setf tree (list (make-final-entry (when form-p (list form))))))
        (loop while exprs
              do (let* ((operator (pop exprs))
                        (args (loop repeat (phi-operator-num-args operator) collect (pop exprs))))
                   (multiple-value-bind (form form-p) (pop-compound-form)
                     (insert-tree tree operator args form form-p))))
        (reduce-tree tree)))))

(defmacro phi-chained-inequality ((&rest symbols) expr &rest exprs)
  (assert (eql (length symbols) (length exprs))
          nil "Chained inequality must have one symbol for every expression after the first")
  (labels ((aux (symbols expr exprs &optional extra-binding)
             (if (cdr exprs)
                 (with-gensyms (tmp)
                   `(let (,@(when extra-binding (list extra-binding)) (,tmp ,(first exprs)))
                      (and (,(first symbols) ,expr ,tmp)
                           ,(aux (cdr symbols) tmp (cdr exprs)))))
                 `(,(first symbols) ,(if extra-binding (second extra-binding) expr) ,(first exprs)))))
    (if exprs
        (with-gensyms (tmp)
          (aux symbols tmp exprs (list tmp expr)))
        expr)))

;; Statements

(macrolet ((def (name symbol word &optional standalone-p (expression-p t))
             `(define-syntax (,name ,@(unless standalone-p '(:group phi-stmt-tmp))
                                    :args (r)
                                    :trace (if complete-p
                                               (list* ',symbol ,@(when expression-p `(expr)) (unprognify blk))
                                               (error "Incomplete ~s statement" ,(symbol-name symbol))))
                (match-trace kwd ,word)
                (maybe
                 (phi-stmt-ws r)
                 ,@(let ((blk `(progn (phi-stmt-ws r)
                                      (alt (match-trace blk phi-block r)
                                           (match-trace blk phi-brace-block)
                                           (match-trace blk phi-expr-1))
                                      (trace-push complete-p t))))
                     (if expression-p
                         `((match-trace expr phi-expr-1)
                           (maybe
                            (phi-stmt-ws r)
                            ,blk))
                         `(,blk))))
                (phi-stmt-end r))))
  (def phi-if-component if phi-if-keyword t)
  (def phi-elif elif phi-elif-keyword t)
  (def phi-else else phi-else-keyword t nil))

(define-syntax (phi-if :group phi-stmt-tmp :args (r)
                       :trace (if elif-parts
                                  `(cond (,(second if-part) ,@(cddr if-part))
                                         ,@(loop for elif-part in elif-parts collect `(,(second elif-part) ,@(cddr elif-part)))
                                         ,@(when else-part (list `(t ,@(cdr else-part)))))
                                  `(if ,(second if-part) ,(prognify (cddr if-part)) ,@(when else-part (list (prognify (cdr else-part)))))))
  (match-trace if-part phi-if-component r)
  (many
   (phi-stmt-separator r)
   (match-trace-list elif-parts phi-elif r))
  (maybe
   (phi-stmt-separator r)
   (match-trace else-part phi-else r))
  (reject (phi-stmt-separator r)
          (alt (match-trace kwd phi-elif-keyword) (match-trace kwd phi-else-keyword))))

(define-syntax (phi-lambda-list-keyword :trace (if name
                                                   (let ((kwd (concatenate 'string "&" (string-upcase name)))
                                                         (cl (find-package "COMMON-LISP")))
                                                     (or (loop for sym in lambda-list-keywords
                                                               thereis (and (eql cl (symbol-package sym))
                                                                            (string= kwd (symbol-name sym))
                                                                            sym))
                                                         (error "Invalid lambda-list keyword")))
                                                   (error "Missing name in lambda-list keyword")))
  (match "&")
  (maybe (match-trace name phi-token-part))
  (match phi-symbol-end))

(define-syntax (phi-argument-component-pattern :trace pattern)
  (match "[") (match-trace pattern phi-arguments-inner) (match "]"))

(define-syntax (phi-argument-component-keyword :trace (list kwd kwd-p sym/pattern pattern-p))
  (match "(")
  (phi-any-ws)
  (match-trace kwd phi-keyword-component)
  (trace-push kwd-p t)
  (phi-any-ws)
  (alt (match-trace sym/pattern phi-symbol)
       (progn (match-trace sym/pattern phi-argument-component-pattern) (trace-push pattern-p t)))
  (phi-any-ws)
  (match ")"))

(define-syntax (phi-argument-component-predicate :trace (list predicate predicate-p incomplete-p))
  (match "{")
  (phi-any-ws)
  (maybe-or-set incomplete-p
    (match-trace predicate phi-symbol)
    (trace-push predicate-p t)
    (phi-any-ws))
  (match "}"))

(define-syntax (phi-argument-component :trace (let (kwd kwd-p predicate predicate-p)
                                                (awhen keyword-quadruple
                                                  (setf (values kwd kwd-p sym/pattern pattern-p) (values-list it)))
                                                (awhen predicate-triple
                                                  (setf (values predicate predicate-p incomplete-p) (values-list it)))
                                                (if incomplete-p
                                                    (error "Incomplete argument")
                                                    (list kwd kwd-p sym/pattern pattern-p predicate predicate-p expr expr-p typ typ-p))))
  (alt (match-trace sym/pattern phi-symbol)
       (progn (match-trace sym/pattern phi-argument-component-pattern) (trace-push pattern-p t))
       (progn
         (match-trace keyword-quadruple phi-argument-component-keyword)))
  (maybe (match-trace predicate-triple phi-argument-component-predicate))
  (maybe (match ":")
         (maybe-or-set incomplete-p
           (phi-any-ws)
           (match-trace typ phi-type-expr)
           (trace-push typ-p t)))
  (maybe (phi-any-ws)
         (match-trace op phi-=-keyword)
         (maybe-or-set incomplete-p
           (phi-any-ws)
           (match-trace expr phi-exprs-any-ws)
           (trace-push expr-p t))))

(defun phi-transform-argument-components (components)
  (let ((llkwd nil)
        (types nil))
    (list (loop for component in components
                collect (if (member component lambda-list-keywords)
                            (setf llkwd component)
                            (destructuring-bind (keyword keyword-p sym/pattern pattern-p predicate predicate-p default default-p type type-p) component
                              (when (consp sym/pattern)
                                (setf types (nconc (nreverse (second sym/pattern))))
                                (setf sym/pattern (first sym/pattern)))
                              (when type-p
                                (assert (symbolp sym/pattern) nil "Can not apply type to a pattern")
                                (push (list sym/pattern type) types))
                              (macrolet ((argument-error ()
                                           `(error "Malformed argument list")))
                                (ecase llkwd
                                  ((nil)
                                   (if (or keyword-p predicate-p default-p)
                                       (argument-error)
                                       sym/pattern))
                                  (&allow-other-keys
                                   (argument-error))
                                  ((&environment &whole)
                                   (if (or keyword-p pattern-p predicate-p default-p)
                                       (argument-error)
                                       sym/pattern))
                                  (&aux
                                   (if (or keyword-p pattern-p predicate-p)
                                       (argument-error)
                                       (if default-p
                                           (list sym/pattern default)
                                           sym/pattern)))
                                  ((&rest &body)
                                   (if (or keyword-p predicate-p default-p)
                                       (argument-error)
                                       sym/pattern))
                                  (&optional
                                   (if keyword-p
                                       (argument-error)
                                       (if (or pattern-p predicate-p default-p)
                                           (append (list sym/pattern)
                                                   (when (or predicate-p default-p) (list default))
                                                   (when predicate-p (list predicate)))
                                           sym/pattern)))
                                  (&key
                                   (when (and pattern-p (not keyword-p)) (argument-error))
                                   (if (or keyword-p predicate-p default-p)
                                       (append (list (if keyword-p
                                                         (list keyword sym/pattern)
                                                         sym/pattern))
                                               (when (or predicate-p default-p) (list default))
                                               (when predicate-p (list predicate)))
                                       sym/pattern)))))))
          (nreverse types))))

(define-syntax (phi-arguments-inner :trace (phi-transform-argument-components args))
  (phi-any-ws)
  (maybe
   (many
    (alt (progn (match-trace-list args phi-lambda-list-keyword)
                (phi-any-ws))
         (progn (match-trace-list args phi-argument-component)
                (phi-any-ws) (match ",") (phi-any-ws))))
   (alt (match-trace-list args phi-lambda-list-keyword)
        (match-trace-list args phi-argument-component))
   (phi-any-ws)
   (maybe
    (match ",")
    (phi-any-ws))))

(define-syntax (phi-arguments-tmp :trace args)
  (match "(")
  (match-trace args phi-arguments-inner)
  (match ")"))
(define-syntax (phi-arguments-fallback :trace (error "Invalid arguments list"))
  (reject (match phi-arguments-tmp))
  (match phi-fallback-parens))

(define-syntax (phi-arguments :trace args)
  (alt (match-trace args phi-arguments-tmp) (match-trace args phi-arguments-fallback)))

(defun phi-types-to-declares (types)
  (when types
    (list `(declare ,@(loop for (sym type) in types
                            collect `(type ,type ,sym))))))

(define-syntax (phi-func :group phi-stmt-tmp :args (r)
                         :trace (if complete-p
                                    `(defun ,name ,(first args)
                                       ,@(phi-types-to-declares (second args))
                                       ,@(unprognify blk))
                                    (error "Incomplete func statement")))
  (match-trace kwd phi-func-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (maybe
    (match-trace args phi-arguments)
    (maybe
     (phi-stmt-ws r)
     (alt (match-trace blk phi-block r) (match-trace blk phi-brace-block))
     (trace-push complete-p t))))
  (phi-stmt-end r))

(define-syntax (phi-method-qualifiers :trace qualifiers)
  (match "[")
  (phi-any-ws)
  (maybe
   (intersperse (match-trace-list qualifiers phi-symbol)
                (progn (phi-any-ws) (match ",") (phi-any-ws)))
   (phi-any-ws) (maybe (match ",") (phi-any-ws)))
  (match "]"))

(defmacro phi-method-impl (name complete-p qualifiers args blk r)
  `(progn
     (match-trace kwd phi-method-keyword)
     (maybe
      (match-trace ,qualifiers phi-method-qualifiers))
     (maybe
      (phi-stmt-ws ,r)
      ,@(let ((rest `((match-trace ,args phi-arguments)
                      (maybe
                       (phi-stmt-ws ,r)
                       (alt (match-trace ,blk phi-block ,r) (match-trace ,blk phi-brace-block))
                       (trace-push ,complete-p t)))))
          (if name
              `((match-trace ,name phi-symbol)
                (maybe ,@rest))
              rest)))
     (phi-stmt-end ,r)))

(defun phi-method-trace-impl (qualifiers args blk)
  (destructuring-bind (lambda-list types) args
    (let* ((pos (or (position-if (lambda (x) (member x lambda-list-keywords)) lambda-list)
                    (length lambda-list)))
           (req (subseq lambda-list 0 pos))
           (rest (subseq lambda-list pos)))
      (loop for cons on req
            do (awhen (assoc (car cons) types)
                 (setf types (remove (car cons) types :key #'car)
                       (car cons) (list (car cons) (second it)))))
      `(,@qualifiers (,@req ,@rest) ,@(phi-types-to-declares types) ,@(unprognify blk)))))

(define-syntax (phi-method :group phi-stmt-tmp :args (r)
                           :trace (if complete-p
                                      `(defmethod ,name ,@(phi-method-trace-impl qualifiers args blk))
                                      (error "Incomplete method statement")))
  (phi-method-impl name complete-p qualifiers args blk r))

(define-syntax (phi-generic-stmt-method :group phi-generic-stmt-tmp :args (r)
                                        :trace (if complete-p
                                                   `(:method ,@(phi-method-trace-impl qualifiers args blk))
                                                   (error "Incomplete method statement")))
  (phi-method-impl nil complete-p qualifiers args blk r))

(define-syntax (phi-block-or-expression :args (r) :trace (if missing-p (error "Missing expression") expr))
  (phi-stmt-ws r)
  (alt (match-trace expr phi-block r)
       (match-trace expr phi-brace-block)
       (progn
         (match-trace kwd phi-=-keyword)
         (trace-push missing-p t)
         (maybe
          (phi-stmt-ws r)
          (match-trace expr phi-exprs-stmt-ws r)
          (trace-push missing-p nil))))
  (phi-stmt-end r))

(define-syntax (phi-macro :group phi-stmt-tmp :args (r)
                          :trace (if complete-p
                                     (if args-p
                                         `(defmacro ,name ,(first args)
                                            ,@(phi-types-to-declares (second args))
                                            ,@(unprognify expr))
                                         `(define-symbol-macro ,name ,expr))
                                     (error "Incomplete macro statement")))
  (match-trace kwd phi-macro-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (maybe
    (match-trace args phi-arguments)
    (trace-push args-p t))
   (maybe
    (match-trace expr phi-block-or-expression r)
    (trace-push complete-p t)))
  (phi-stmt-end r))

(define-syntax (phi-package :group phi-stmt-tmp :args (r)
                            :trace (if complete-p
                                       (if stmts-p
                                           `(defpackage ,name ,@stmts)
                                           `(in-package ,name))
                                       (error "Incomplete package statement")))
  (match-trace kwd phi-package-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-uninterned-symbol)
   (trace-push complete-p t)
   (maybe
    (phi-stmt-ws r)
    (alt (match-trace stmts phi-package-block r)
         (match-trace stmts phi-package-brace-block))
    (trace-push stmts-p t)))
  (phi-stmt-end r))

(define-syntax (phi-class-parents :trace symbols)
  (match "(")
  (phi-any-ws)
  (maybe
   (intersperse (match-trace-list symbols phi-symbol)
                (progn (phi-any-ws) (match ",") (phi-any-ws)))
   (phi-any-ws) (maybe (match ",") (phi-any-ws)))
  (match ")"))

(define-syntax (phi-class :group phi-stmt-tmp :args (r)
                          :trace (if complete-p
                                     `(,(ecase kwd
                                          (phi-class-keyword 'defclass)
                                          (phi-condition-keyword 'define-condition))
                                       ,name ,parents
                                       ,(loop for stmt in stmts
                                              when (eql :slot (first stmt)) collect (second stmt))
                                       ,@(loop for stmt in stmts
                                               unless (eql :slot (first stmt))
                                               collect stmt))
                                     (error "Incomplete class statement")))
  (alt (match-trace kwd phi-class-keyword)
       (match-trace kwd phi-condition-keyword))
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (trace-push complete-p t)
   (maybe
    (match-trace parents phi-class-parents))
   (maybe
    (phi-stmt-ws r)
    (alt (match-trace stmts phi-class-block r)
         (match-trace stmts phi-class-brace-block))))
  (phi-stmt-end r))

(define-syntax (phi-struct :group phi-stmt-tmp :args (r)
                           :trace (if complete-p
                                      (let ((options
                                              (loop for stmt in stmts
                                                    unless (member (first stmt) '(:slot :documentation))
                                                    collect (if (= 1 (length stmt))
                                                                (first stmt)
                                                                stmt))))
                                        `(defstruct ,(if options (list* name options) name)
                                           ,@(loop for stmt in stmts
                                                   when (eql :documentation (first stmt))
                                                   collect (second stmt))
                                           ,@(loop for stmt in stmts
                                                   when (eql :slot (first stmt))
                                                   collect (second stmt))))
                                      (error "Incomplete struct statement")))
  (match-trace kwd phi-struct-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (trace-push complete-p t)
   (maybe
    (phi-stmt-ws r)
    (alt (match-trace stmts phi-struct-block r)
         (match-trace stmts phi-struct-brace-block))))
  (phi-stmt-end r))

(define-syntax (phi-generic :group phi-stmt-tmp :args (r)
                            :trace (if complete-p
                                       `(defgeneric ,name ,(first args) ,@stmts)
                                       (error "Incomplete generic statement")))
  (match-trace kwd phi-generic-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (maybe
    (match-trace args phi-arguments)
    (trace-push complete-p t)
    (maybe
     (phi-stmt-ws r)
     (alt (match-trace stmts phi-generic-block r)
          (match-trace stmts phi-generic-brace-block)))))
  (phi-stmt-end r))

(define-syntax (phi-const :group phi-stmt-tmp :args (r)
                          :trace (if complete-p
                                     `(defconstant ,name ,expr)
                                     (error "Incomplete const statement")))
  (match-trace kwd phi-const-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (maybe
    (phi-stmt-ws r)
    (match-trace op phi-=-keyword)
    (maybe
     (phi-stmt-ws r)
     (match-trace expr phi-exprs-stmt-ws r)
     (trace-push complete-p t))))
  (phi-stmt-end r))

(define-syntax (phi-const*-eqx :trace expr)
  (match "[")
  (phi-any-ws)
  (match-trace expr phi-exprs-any-ws)
  (phi-any-ws)
  (match "]"))

(define-syntax (phi-const* :group phi-stmt-tmp :args (r)
                           :trace (if complete-p
                                      `(defconstant-eqx ,name ,expr
                                                        ,@(when eqx-expr (list eqx-expr)))
                                      (error "Incomplete const* statement")))
  (match-trace kwd phi-const*-keyword)
  (maybe
   (match-trace eqx-expr phi-const*-eqx))
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (maybe
    (phi-stmt-ws r)
    (match-trace op phi-=-keyword)
    (maybe
     (phi-stmt-ws r)
     (match-trace expr phi-exprs-stmt-ws r)
     (trace-push complete-p t))))
  (phi-stmt-end r))

(define-syntax (phi-var :group phi-stmt-tmp :args (r)
                        :trace (if complete-p
                                   `(defvar ,name ,@(when expr-p (list expr)))
                                   (error "Incomplete var statement")))
  (match-trace kwd phi-var-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (trace-push complete-p t)
   (maybe
    (phi-stmt-ws r)
    (match-trace op phi-=-keyword)
    (trace-push complete-p nil)
    (maybe
     (phi-stmt-ws r)
     (match-trace expr phi-exprs-stmt-ws r)
     (trace-push expr-p t)
     (trace-push complete-p t))))
  (phi-stmt-end r))

(define-syntax (phi-type :group phi-stmt-tmp :args (r)
                         :trace (if complete-p
                                    `(deftype ,name ,(if (second args)
                                                         (error "Types unsupported in arguments")
                                                         (first args))
                                       ,expr)
                                    (error "Incomplete type statement")))
  (match-trace kwd phi-type-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (maybe
    (match-trace args phi-arguments))
   (maybe
    (phi-stmt-ws r)
    (match-trace op phi-=-keyword)
    (maybe
     (phi-stmt-ws r)
     (match-trace expr phi-exprs-stmt-ws r)
     (trace-push complete-p t))))
  (phi-stmt-end r))

(define-syntax (phi-type-expr :trace typ)
  (match-trace typ phi-expr-1))

(define-syntax (phi-let-clause-values :trace value-indicators)
  (match "(") (phi-any-ws)
  (maybe
   (intersperse (progn (match-trace-list value-indicators phi-symbol)
                       (maybe (match ":") (phi-inline-ws)
                              (trace-push-list value-indicators -1)
                              (match-trace-list value-indicators phi-type-expr)))
                (progn (phi-any-ws) (match ",") (phi-any-ws))))
  (phi-any-ws)
  (maybe (match ",") (phi-any-ws))
  (match ")"))

(define-syntax (phi-let-clause-list :trace list-pattern)
  (match "[")
  (match-trace list-pattern phi-arguments-inner)
  (match "]"))

(define-syntax (phi-let-clause :args (r) :trace (let ((declares nil))
                                                  (assert full-p nil "Incomplete LET clause")
                                                  (list (ecase lhs-kind
                                                          (symbol
                                                           (when value-type-p
                                                             (setf declares (list (list 'type value-type name))))
                                                           `(values ,name))
                                                          (function `(function ,name ,@(first args)))
                                                          (values
                                                           `(values
                                                             ,@(loop with inds = value-indicators
                                                                     while inds
                                                                     collect (let ((sym (pop inds)))
                                                                               (when (eql -1 (first inds))
                                                                                 (pop inds)
                                                                                 (push (list 'type (pop inds) sym) declares))
                                                                               sym))))
                                                          (list `(list ,@(first list-pattern))))
                                                        (aif (second args)
                                                             `(progn ,@(phi-types-to-declares it) ,@(unprognify expr))
                                                             expr)
                                                        (append (phi-types-to-declares (second list-pattern))
                                                                (nreverse declares)))))
  (maybe
   (alt (progn (match-trace name phi-symbol)
               (trace-push lhs-kind symbol)
               (maybe (match ":") (phi-inline-ws)
                      (match-trace value-type phi-type-expr)
                      (trace-push value-type-p t))
               (maybe (match-trace args phi-arguments)
                      (trace-push lhs-kind function)))
        (progn (match-trace value-indicators phi-let-clause-values)
               (trace-push lhs-kind values))
        (progn (match-trace list-pattern phi-let-clause-list)
               (trace-push lhs-kind list)))
   (maybe
    (phi-stmt-ws r)
    (match-trace op phi-=-keyword)
    (maybe
     (phi-stmt-ws r)
     (match-trace expr phi-exprs-stmt-ws r)
     (trace-push full-p t))))
  (peek (alt (progn (phi-stmt-ws r) (match ","))
             (phi-stmt-end r))))

(defun phi-transform-clauses (clauses form)
  (let ((declares nil))
    (fcl::apply-bindings
     (loop while clauses
           collect (loop with first = t
                         while (and clauses (or first (eql 'phi-and-keyword (first clauses))))
                         initially (assert (eql 'phi-let-keyword (first clauses)))
                         collect (progn
                                   (setf first nil)
                                   (assert (cdr clauses))
                                   (destructuring-bind (lhs rhs declares1) (second clauses)
                                     (setf declares (nconc declares declares1))
                                     (setf clauses (cddr clauses))
                                     (list lhs rhs)))))
     (if declares
         `(progn (declare ,@declares) ,@(unprognify form))
         form))))

(define-syntax (phi-let :group phi-stmt-tmp :args (r) :trace (if incomplete-p
                                                                 (error "Incomplete let statement")
                                                                 (phi-transform-clauses clauses (phi-prognify stmts))))
  (intersperse
   (progn
     (match-trace-list clauses phi-let-keyword)
     (maybe-or-set incomplete-p
       (phi-stmt-ws r)
       (intersperse (match-trace-list clauses phi-let-clause r)
                    (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r) (trace-push-list clauses phi-and-keyword)))
       (maybe (phi-stmt-ws r) (match ","))
       (many
        (phi-stmt-end r)
        (phi-stmt-separator r)
        (match-trace-list clauses phi-and-keyword)
        (maybe-or-set incomplete-p
          (phi-stmt-ws r)
          (intersperse (match-trace-list clauses phi-let-clause r)
                       (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r) (trace-push-list clauses phi-and-keyword)))
          (maybe (phi-stmt-ws r) (match ",")))))
     (phi-stmt-end r)
     (reject (phi-stmt-separator r) (match phi-and-keyword)))
   (phi-stmt-separator r))
  (reject (phi-stmt-separator r) (match phi-let-keyword))
  (maybe (phi-stmt-separator r)
         (match-trace stmts phi-stmts r))
  (phi-stmts-end r))

(define-syntax (phi-macrolet-clause :args (r)
                                    :trace (if complete-p
                                               (list (if args-p
                                                         (list* name (first args))
                                                         name)
                                                     (aif (second args)
                                                          `(progn ,@(phi-types-to-declares it) ,@(unprognify expr))
                                                          expr))
                                               (error "Incomplete macrolet clause")))
  (match-trace name phi-symbol)
  (maybe
   (match-trace args phi-arguments)
   (trace-push args-p t))
  (maybe
   (match-trace expr phi-block-or-expression r)
   (trace-push complete-p t))
  (peek (alt (phi-stmt-end r)
             (progn (phi-stmt-ws r) (match ",")))))

(define-syntax (phi-macrolet :group phi-stmt-tmp :args (r) :trace (fcl::apply-macro-bindings clauses (phi-prognify stmts)))
  (match-trace kwd phi-macrolet-keyword)
  (maybe
   (phi-stmt-ws r)
   (intersperse (match-trace-list clauses phi-macrolet-clause r)
                (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r))))
  (phi-stmt-end r)
  (many
   (phi-stmt-separator r)
   (match-trace kwd phi-and-keyword)
   (maybe
    (phi-stmt-ws r)
    (intersperse (match-trace-list clauses phi-macrolet-clause r)
                 (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r))))
   (phi-stmt-end r))
  (reject (phi-stmt-separator r) (match phi-and-keyword))
  (maybe (phi-stmt-separator r)
         (match-trace stmts phi-stmts r))
  (phi-stmts-end r))

(define-syntax (phi-declare-list :trace `(declare ,@exprs))
  (match "[")
  (phi-any-ws)
  (maybe
   (intersperse (match-trace-list exprs phi-exprs-any-ws)
                (progn (phi-any-ws) (match ",") (phi-any-ws)))
   (phi-any-ws) (maybe (match ",") (phi-any-ws)))
  (match "]"))

(define-syntax (phi-declare :group (phi-stmt-tmp phi-generic-stmt-tmp) :args (r) :trace stmt)
  (match "#")
  (match-trace stmt phi-declare-list)
  (phi-stmt-end r))

(define-syntax (phi-label :group phi-stmt-tmp :args (r)
                          :trace (if complete-p (make-label :symbol sym) (error "Incomplete label")))
  (match "@")
  (maybe
   (match-trace sym phi-symbol)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-defer :group phi-stmt-tmp :args (r)
                          :trace (if complete-p
                                     `(unwind-protect ,(phi-prognify stmts) ,@(unprognify expr))
                                     (error "Incomplete defer statement")))
  (match-trace kwd phi-defer-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace expr phi-exprs-stmt-ws r)
   (trace-push complete-p t))
  (phi-stmt-end r)
  (maybe (phi-stmt-separator r)
         (match-trace stmts phi-stmts r))
  (phi-stmts-end r))

(define-syntax (phi-as-keyword :trace 'phi-as-keyword)
  (match "as")
  (match phi-symbol-end))

(define-syntax (phi-handle-clause :args (r)
                                  :trace (if missing-p
                                             (error "Incomplete handle statement")
                                             `(,typ ,(if arg-p (list arg) nil) ,@(unprognify expr))))
  (match-trace typ phi-type-expr)
  (maybe
   (phi-stmt-ws r)
   (match-trace kwd phi-as-keyword)
   (trace-push missing-p t)
   (maybe
    (phi-stmt-ws r)
    (match-trace arg phi-symbol)
    (trace-push arg-p t)
    (trace-push missing-p nil)))
  (maybe
   (match-trace expr phi-block-or-expression r))
  (phi-stmt-end r))

(define-syntax (phi-handle-else-clause :args (r) :trace (if complete-p
                                                            `(:no-error ,(first args) ,@(phi-types-to-declares (second args)) ,@(unprognify expr))
                                                            (error "Incomplete handle else statement")))
  (match-trace args phi-arguments)
  (maybe
   (phi-stmt-ws r)
   (match-trace expr phi-exprs-stmt-ws r)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-handle :group phi-stmt-tmp :args (r) :trace (if incomplete-p
                                                                    (error "Incomplete handle statement")
                                                                    `(handler-case ,(phi-prognify stmts) ,@clauses)))
  (match-trace kwd phi-handle-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace-list clauses phi-handle-clause r)
   (many
    (phi-stmt-separator r)
    (match-trace kwd phi-and-keyword)
    (maybe-or-set incomplete-p
      (phi-stmt-ws r)
      (match-trace-list clauses phi-handle-clause r)))
   (maybe
    (phi-stmt-separator r)
    (match-trace kwd phi-else-keyword)
    (maybe-or-set incomplete-p
      (phi-stmt-ws r)
      (match-trace-list clauses phi-handle-else-clause r))))
  (phi-stmt-end r)
  (reject (phi-stmt-separator r) (alt (match phi-and-keyword) (match phi-else-keyword)))
  (maybe (phi-stmt-separator r)
         (match-trace stmts phi-stmts r))
  (phi-stmts-end r))

(define-syntax (phi-restart-clause :args (r) :trace (if complete-p
                                                        `(,name ,(first args) ,@(phi-types-to-declares (second args)) ,@(unprognify expr))
                                                        (error "Incomplete restart clause")))
  (match-trace name phi-symbol)
  (maybe
   (match-trace args phi-arguments)
   (trace-push complete-p t)
   (maybe
    (match-trace expr phi-block-or-expression r)))
  (phi-stmt-end r))

(define-syntax (phi-restart :group phi-stmt-tmp :args (r) :trace (if incomplete-p
                                                                     (error "Incomplete restart statement")
                                                                     `(restart-case ,(phi-prognify stmts) ,@clauses)))
  (match-trace kwd phi-restart-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match-trace-list clauses phi-restart-clause r)
    (many
     (phi-stmt-separator r)
     (match-trace kwd phi-and-keyword)
     (maybe-or-set incomplete-p
       (phi-stmt-ws r)
       (match-trace-list clauses phi-restart-clause r))))
  (phi-stmt-end r)
  (reject (phi-stmt-separator r) (match phi-and-keyword))
  (maybe (phi-stmt-separator r)
         (match-trace stmts phi-stmts r))
  (phi-stmts-end r))

(macrolet ((def (name string)
             `(define-syntax (,name :group (,name phi-package-keyword) :trace ',name)
                (match ,string)
                (match phi-symbol-end))))
  (def phi-nicknames-keyword "nicknames")
  (def phi-use-keyword "use")
  (def phi-from-keyword "from")
  (def phi-shadow-keyword "shadow")
  (def phi-shadowing-keyword "shadowing")
  (def phi-import-keyword "import")
  (def phi-export-keyword "export")
  (def phi-intern-keyword "intern")
  (def phi-size-keyword "size"))

(define-syntax (phi-package-stmt-tmp :args (r)
                                     :trace (list* (ecase kwd
                                                     (phi-nicknames-keyword :nicknames)
                                                     (phi-use-keyword :use)
                                                     (phi-shadow-keyword :shadow)
                                                     (phi-export-keyword :export)
                                                     (phi-intern-keyword :intern))
                                                   symbols))
  (alt (match-trace kwd phi-nicknames-keyword)
       (match-trace kwd phi-use-keyword)
       (match-trace kwd phi-shadow-keyword)
       (match-trace kwd phi-export-keyword)
       (match-trace kwd phi-intern-keyword))
  (phi-stmt-ws r)
  (maybe
   (intersperse (match-trace-list symbols phi-uninterned-symbol)
                (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r)))
   (maybe (phi-stmt-ws r) (match ",")))
  (phi-stmt-end r))

(define-syntax (phi-package-stmt-from :group phi-package-stmt-tmp :args (r)
                                      :trace (if complete-p
                                                 (list* (if shadow-p :shadowing-import-from :import-from) pkg symbols)
                                                 (error "Incomplete from-import statement")))
  (match-trace kwd phi-from-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace pkg phi-uninterned-symbol)
   (maybe
    (phi-stmt-ws r)
    (maybe
     (match-trace shadow-p phi-shadowing-keyword)
     (phi-stmt-ws r))
    (match-trace kwd phi-import-keyword)
    (trace-push complete-p t)
    (maybe
     (phi-stmt-ws r)
     (intersperse (match-trace-list symbols phi-uninterned-symbol)
                  (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r)))
     (maybe (phi-stmt-ws r) (match ",")))))
  (phi-stmt-end r))

(define-syntax (phi-package-stmt-documentation :group phi-package-stmt-tmp :args (r) :trace `(:documentation ,str))
  (alt (match-trace str phi-string-1) (match-trace str phi-string-2))
  (phi-stmt-end r))

(define-syntax (phi-package-stmt-size :group phi-package-stmt-tmp :args (r) :trace (if num `(:size ,num) (error "Incomplete size statement")))
  (match-trace kwd phi-size-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace num phi-number))
  (phi-stmt-end r))

(macrolet ((def (name string)
             `(define-syntax (,name :group (,name phi-class-keyword) :trace ',name)
                (match ,string)
                (match phi-symbol-end))))
  (def phi-default-initargs-keyword "default-initargs")
  (def phi-metaclass-keyword "metaclass")
  (def phi-report-keyword "report")
  (def phi-slot-class-keyword "slot"))

(define-syntax (phi-class-stmt-documentation :group phi-class-stmt-tmp :args (r) :trace `(:documentation ,str))
  (alt (match-trace str phi-string-1) (match-trace str phi-string-2))
  (phi-stmt-end r))

(define-syntax (phi-class-stmt-metaclass :group phi-class-stmt-tmp :args (r) :trace (if complete-p `(:metaclass ,name) (error "Incomplete metaclass statement")))
  (match-trace kwd phi-metaclass-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-class-stmt-report :group phi-class-stmt-tmp :args (r) :trace (if complete-p `(:report ,expr) (error "Incomplete report statement")))
  (match-trace kwd phi-report-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace expr phi-exprs-stmt-ws r)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-class-stmt-slot-keywords :trace components)
  (match "[")
  (phi-any-ws)
  (maybe
   (intersperse (match-trace-list components phi-expr-component)
                (progn (phi-any-ws) (match ",") (phi-any-ws)))
   (phi-any-ws) (maybe (match ",") (phi-any-ws)))
  (match "]"))

(define-syntax (phi-class-stmt-slot :group phi-class-stmt-tmp :args (r)
                                    :trace (if incomplete-p
                                               (error "Incomplete slot statement")
                                               `(:slot (,name ,@(loop for component in components
                                                                      append (if (eql 'keyword (first component))
                                                                                 (rest component)
                                                                                 (error "Slot options must be keyword arguments")))
                                                              ,@(when typ-p `(:type ,typ))
                                                              ,@(when expr-p `(:initform ,expr))))))
  (match-trace kwd phi-slot-class-keyword)
  (maybe
   (match-trace components phi-class-stmt-slot-keywords))
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match-trace name phi-symbol)
    (maybe
     (match ":")
     (maybe-or-set incomplete-p
       (phi-stmt-ws r)
       (match-trace typ phi-type-expr)
       (trace-push typ-p t)))
    (maybe
     (phi-stmt-ws r)
     (match-trace kwd phi-=-keyword)
     (maybe-or-set incomplete-p
       (phi-stmt-ws r)
       (match-trace expr phi-exprs-stmt-ws r)
       (trace-push expr-p t))))
  (phi-stmt-end r))

(macrolet ((def (name string)
             `(define-syntax (,name :group (,name phi-struct-keyword) :trace ',name)
                (match ,string)
                (match phi-symbol-end))))
  (def phi-conc-name-keyword "conc-name")
  (def phi-constructor-keyword "constructor")
  (def phi-copier-keyword "copier")
  (def phi-predicate-keyword "predicate")
  (def phi-include-keyword "include")
  (def phi-print-object-keyword "print-object")
  (def phi-print-function-keyword "print-function")
  (def phi-type-struct-keyword "type")
  (def phi-named-keyword "named")
  (def phi-initial-offset-keyword "initial-offset")
  (def phi-readonly-keyword "readonly")
  (def phi-slot-struct-keyword "slot"))

(define-syntax (phi-struct-stmt-documentation :group phi-struct-stmt-tmp :args (r) :trace `(:documentation ,str))
  (alt (match-trace str phi-string-1) (match-trace str phi-string-2))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-conc-name :group phi-struct-stmt-tmp :args (r)
                                          :trace (if complete-p
                                                     `(:conc-name ,sym)
                                                     (error "Incomplete conc-name statement")))
  (match-trace kwd phi-conc-name-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-uninterned-symbol)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-constructor :group phi-struct-stmt-tmp :args (r)
                                            :trace (if (and args-p (second args))
                                                       (error "Types not supported in constructor")
                                                       `(:constructor ,@(when name-p (list* name (when args-p (list (first args))))))))
  (match-trace kwd phi-constructor-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace name phi-symbol)
   (trace-push name-p t)
   (maybe
    (phi-stmt-ws r)
    (match-trace args phi-arguments)
    (trace-push args-p t)))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-copier :group phi-struct-stmt-tmp :args (r) :trace `(:copier ,@(when sym-p (list sym))))
  (match-trace kwd phi-copier-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-symbol)
   (trace-push sym-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-predicate :group phi-struct-stmt-tmp :args (r) :trace `(:predicate ,@(when sym-p (list sym))))
  (match-trace kwd phi-predicate-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-symbol)
   (trace-push sym-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-include-slot
                :trace (if (or incomplete1-p incomplete2-p)
                           (error "Incomplete include slot specifier")
                           `(,name ,expr ,@(when typ-p (list :type typ)) ,@(when read-only-p (list :read-only t)))))
  (maybe
   (match-trace read-only-p phi-readonly-keyword)
   (phi-any-ws))
  (match-trace name phi-symbol)
  (maybe
   (match ":")
   (trace-push incomplete1-p t)
   (maybe
    (phi-any-ws)
    (match-trace typ phi-type-expr)
    (trace-push typ-p t)
    (trace-push incomplete1-p nil)))
  (maybe
   (phi-any-ws)
   (match-trace kwd phi-=-keyword)
   (trace-push incomplete2-p t)
   (maybe
    (phi-any-ws)
    (match-trace expr phi-exprs-any-ws)
    (trace-push incomplete2-p nil))))

(define-syntax (phi-struct-stmt-include :group phi-struct-stmt-tmp :args (r)
                                        :trace (if complete-p
                                                   `(:include ,sym ,@slots)
                                                   (error "Incomplete include statement")))
  (match-trace kwd phi-include-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-symbol)
   (trace-push complete-p t)
   (maybe
    (phi-stmt-ws r)
    (match "(")
    (phi-any-ws)
    (maybe
     (intersperse (match-trace-list slots phi-struct-stmt-include-slot)
                  (progn (phi-any-ws) (match ",") (phi-any-ws)))
     (phi-any-ws) (maybe (match ",") (phi-any-ws)))
    (match ")")))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-print-object :group phi-struct-stmt-tmp :args (r) :trace `(:print-object ,@(when sym-p (list sym))))
  (match-trace kwd phi-print-object-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-symbol)
   (trace-push sym-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-print-function :group phi-struct-stmt-tmp :args (r) :trace `(:print-function ,@(when sym-p (list sym))))
  (match-trace kwd phi-print-function-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-symbol)
   (trace-push sym-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-type :group phi-struct-stmt-tmp :args (r)
                                     :trace (if complete-p
                                                `(:type ,sym)
                                                (error "Incomplete type statement")))
  (match-trace kwd phi-type-struct-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace sym phi-symbol)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-named :group phi-struct-stmt-tmp :args (r) :trace '(:named))
  (match-trace kwd phi-named-keyword)
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-initial-offset :group phi-struct-stmt-tmp :args (r)
                                               :trace (if complete-p
                                                          `(:initial-offset ,num)
                                                          (error "Incomplete initial-offset statement")))
  (match-trace kwd phi-initial-offset-keyword)
  (maybe
   (phi-stmt-ws r)
   (match-trace num phi-number)
   (trace-push complete-p t))
  (phi-stmt-end r))

(define-syntax (phi-struct-stmt-slot :group phi-struct-stmt-tmp :args (r)
                                     :trace (if incomplete-p
                                                (error "Incomplete slot statement")
                                                `(:slot (,name ,expr ,@(when typ-p `(:type ,typ)) ,@(when read-only-p `(:read-only t))))))
  (maybe
   (match-trace read-only-p phi-readonly-keyword)
   (phi-stmt-ws r))
  (match-trace kwd phi-slot-struct-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match-trace name phi-symbol)
    (maybe
     (match ":")
     (maybe-or-set incomplete-p
       (phi-stmt-ws r)
       (match-trace typ phi-type-expr)
       (trace-push typ-p t)))
    (maybe
     (phi-stmt-ws r)
     (match-trace kwd phi-=-keyword)
     (maybe-or-set incomplete-p
       (phi-stmt-ws r)
       (match-trace expr phi-exprs-stmt-ws r))))
  (phi-stmt-end r))

(macrolet ((def (name string)
             `(define-syntax (,name :group (,name phi-generic-keyword) :trace ',name)
                (match ,string)
                (match phi-symbol-end))))
  (def phi-argument-precedence-order-keyword "argument-precedence-order")
  (def phi-method-combination-keyword "method-combination")
  (def phi-generic-function-class-keyword "generic-function-class")
  (def phi-method-class-keyword "method-class"))

(define-syntax (phi-generic-stmt-argument-precedence-order :group phi-generic-stmt-tmp :args (r)
                                                           :trace (if incomplete-p
                                                                      (error "Incomplete argument-precedence-order statement")
                                                                      `(:argument-precedence-order ,@syms)))
  (match-trace kwd phi-argument-precedence-order-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match "(")
    (phi-any-ws)
    (maybe
     (intersperse (match-trace-list syms phi-symbol)
                  (progn (phi-any-ws) (match ",") (phi-any-ws)))
     (phi-any-ws) (maybe (match ",") (phi-any-ws)))
    (match ")"))
  (phi-stmt-end r))

(define-syntax (phi-generic-stmt-documentation :group phi-generic-stmt-tmp :args (r) :trace `(:documentation ,str))
  (alt (match-trace str phi-string-1) (match-trace str phi-string-2))
  (phi-stmt-end r))

(define-syntax (phi-generic-stmt-method-combination :group phi-generic-stmt-tmp :args (r)
                                                    :trace (if incomplete-p
                                                               (error "Incomplete method-combination statement")
                                                               `(:method-combination ,sym)))
  (match-trace kwd phi-method-combination-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match-trace sym phi-symbol))
  (phi-stmt-end r))

(define-syntax (phi-generic-stmt-generic-function-class :group phi-generic-stmt-tmp :args (r)
                                                        :trace (if incomplete-p
                                                                   (error "Incomplete generic-function-class statement")
                                                                   `(:generic-function-class ,sym)))
  (match-trace kwd phi-generic-function-class-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match-trace sym phi-symbol))
  (phi-stmt-end r))

(define-syntax (phi-generic-stmt-method-class :group phi-generic-stmt-tmp :args (r)
                                              :trace (if incomplete-p
                                                         (error "Incomplete method-class statement")
                                                         `(:method-class ,sym)))
  (match-trace kwd phi-method-class-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (match-trace sym phi-symbol))
  (phi-stmt-end r))

(define-syntax (phi-let-gensym-keyword :group (phi-let-gensym-keyword phi-keyword) :trace 'phi-let-gensym-keyword)
  (match "let-gensym")
  (match phi-symbol-end))

(define-syntax (phi-let-gensym :group phi-stmt-tmp :args (r) :trace (if incomplete-p
                                                                        (error "Incomplete let statement")
                                                                        `(with-gensyms ,syms ,@(unprognify (phi-prognify stmts)))))
  (match-trace-list clauses phi-let-gensym-keyword)
  (maybe-or-set incomplete-p
    (phi-stmt-ws r)
    (intersperse (match-trace-list syms phi-symbol)
                 (progn (phi-stmt-ws r) (match ",") (phi-stmt-ws r)))
    (maybe (phi-stmt-ws r) (match ",")))
  (phi-stmt-end r)
  (maybe (phi-stmt-separator r)
         (match-trace stmts phi-stmts r))
  (phi-stmts-end r))
