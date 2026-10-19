(in-package "FED")


(define-default-grammar "fr" 'ftx::phi-main 'ftx::phi-read)

(define-group-attributes ftx::phi-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-=>-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-as-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-package-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-class-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-struct-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-generic-keyword '(:foreground :yellow))
(define-group-attributes ftx::phi-string-1 '(:foreground :green))
(define-group-attributes ftx::phi-string-2 '(:foreground :green))
(define-group-attributes ftx::phi-char '(:foreground :green))
(define-group-attributes ftx::phi-comment '(:foreground :bright-black))
(define-group-attributes ftx::phi-number '(:foreground :magenta))
(define-group-attributes ftx::phi-keyword-component '(:foreground :orange))
(define-group-attributes ftx::phi-lambda-list-keyword '(:foreground :yellow))

(define-group-attributes ftx::phi-fallback-stmt '(:underline t))
(define-group-attributes ftx::phi-fallback-parens '(:underline t))
(define-group-attributes ftx::phi-fallback-brackets '(:underline t))
(define-group-attributes ftx::phi-fallback-braces '(:underline t))
(define-group-attributes ftx::phi-fallback-angles '(:underline t))
(define-group-attributes ftx::phi-fallback-shells '(:underline t))
(define-group-attributes ftx::phi-fallback-expr-1 '(:underline t))
(define-group-attributes ftx::phi-arguments-fallback '(:underline t))

(define-group-attributes ftx::gamma-->-keyword '(:foreground :yellow))
(define-group-attributes ftx::gamma-=>-keyword '(:foreground :yellow))

(define-rule-evaluator ftx::phi-main comment-prefix "//")

(macrolet ((def (rule &rest attrs)
             `(define-rule-evaluator ,rule paint
                (awhen (and (boundp 'ftx::name) (symbol-value 'ftx::name))
                  (lambda (paint) (funcall paint it '(:foreground ,@attrs)))))))
  (def ftx::phi-var :cyan)
  (def ftx::phi-const :cyan)
  (def ftx::phi-const* :cyan)
  (def ftx::phi-func :blue :bold t)
  (def ftx::phi-macro :blue :bold t)
  (def ftx::phi-method :blue :bold t)
  (def ftx::phi-type :blue :italic t)
  (def ftx::phi-class :blue :italic t)
  (def ftx::gamma-syntax :blue :bold t)
  (def ftx::gamma-syntax-macro :blue :bold t)
  (def ftx::gamma-syntax-group :blue :bold t))

(define-rule-evaluator ftx::phi-symbol nointern-symbol
  (ignore-errors (ftx::token-symbol (mapcar #'eval-ast ftx::parts) t)))

(define-rule-evaluator ftx::phi-expr-1-tmp nointern-symbol
  (and (boundp 'ftx::expr) (eval-ast ftx::expr 'nointern-symbol)))
(define-rule-evaluator ftx::phi-expr-1 nointern-symbol
  (and (boundp 'ftx::expr) (eval-ast ftx::expr 'nointern-symbol)))

(define-rule-evaluator ftx::phi-symbol paint
  (let ((parts (ignore-errors (mapcar #'eval-ast ftx::parts))))
    (when parts
      (cond ((and (= 1 (length parts))
                  (stringp (first parts))
                  (or (string= (string-upcase (first parts)) "T")
                      (string= (string-upcase (first parts)) "NIL")))
             '(:foreground :magenta))
            ((and (consp (first parts)) (eql 'ftx::colons (first (first parts))))
             '(:foreground :orange))))))

(defun paint-exprs (exprs)
  (loop with pairs = nil
        with first-after-op = t
        for cons on exprs
        for node = (car cons)
        do (let ((symbol (eval-ast node 'nointern-symbol)))
             (cond ((ftx::phi-operator-p symbol)
                    (setf first-after-op t)
                    (push (list node '(:foreground :yellow)) pairs))
                   ((and (shiftf first-after-op nil)
                         (or (special-operator-p symbol) (macro-function symbol))
                         (consp (cdr cons))
                         (not (ftx::phi-operator-p (eval-ast (cadr cons) 'nointern-symbol))))
                    (push (list node '(:foreground :yellow)) pairs))))
        finally (when pairs
                  (return
                    (lambda (paint)
                      (loop for (node attrs) in pairs
                            do (funcall paint node attrs)))))))

(define-rule-evaluator ftx::phi-exprs-any-ws paint
  (paint-exprs (and (boundp 'ftx::exprs) ftx::exprs)))

(define-rule-evaluator ftx::phi-exprs-stmt-ws paint
  (paint-exprs (and (boundp 'ftx::exprs) ftx::exprs)))

(define-rule-evaluator ftx::phi-package extract-package
  (when (and (boundp 'ftx::name) (not (and (boundp 'ftx::stmts-p) ftx::stmts-p)))
    (find-package (eval-ast ftx::name))))

(define-rule-evaluator ftx::phi-stmt extract-package
  (eval-ast ftx::stmt 'extract-package))

(define-rule-evaluator ftx::phi-main extract-package
  (when (boundp 'ftx::stmts)
    (loop for node in ftx::stmts
          do (awhen (eval-ast node 'extract-package)
               (return it)))))

(define-rule-evaluator ftx::phi-main comment-prefix "//")

(define-rule-evaluator ftx::phi-number number-formatter #'princ-to-string)

(defun phi-string-escape (str &optional (open #\") (close #\"))
  (loop with i = 0 and escape-chars = (list* (list close close) '())
        while (< i (length str))
        nconc (let ((next-escape (position-if (lambda (ch) (assoc ch escape-chars)) str :start i)))
                (prog1 (if next-escape
                           (list (subseq str i next-escape) "\\" (string (assoc-value (aref str next-escape) escape-chars)))
                           (list (subseq str i (length str))))
                  (setf i (if next-escape (1+ next-escape) (length str)))))
        into chunks
        finally (return (apply #'concatenate 'string (string open) (nconc chunks (list (string close)))))))

(defun phi-string-formatter (open close)
  (lambda (node buffer)
    (let ((str (handler-case (eval-ast node) (error (e) (editor-error "~a" e)))))
      (phi-string-escape (format-paragraph-aux str (buffer-variable buffer 'fill-column)) open close))))

(define-rule-evaluator ftx::phi-string-1 formatter
  (phi-string-formatter #\" #\"))

(define-rule-evaluator ftx::phi-string-2 formatter
  (phi-string-formatter #\“ #\”))

(define-rule-evaluator ftx::phi-symbol complete-function #'phi-complete)

(defun phi-complete (cursor node)
  (lambda-complete-token cursor (mapcar #'eval-ast (ast-variable node 'ftx::parts))))


(define-rule-evaluator ftx::phi-main toplevel-p t)

(macrolet ((def-rule-list (&rest names)
             (prognify
              (loop for name in names
                    append `((define-rule-evaluator ,name delimited-p t)
                             (define-rule-evaluator ,name missing-close-delim-p (not (boundp 'ftx::closed-p)))
                             (define-rule-evaluator ,name sexpr-p t)
                             (define-rule-evaluator ,name list-p t)))))
           (def-rule-string (&rest names)
             (prognify
              (loop for name in names
                    append `((define-rule-evaluator ,name delimited-p t)
                             (define-rule-evaluator ,name missing-close-delim-p (not (boundp 'ftx::closed-p)))
                             (define-rule-evaluator ,name sexpr-p t)))))
           (def-rule-sexpr (&rest names)
             (prognify
              (loop for name in names
                    append `((define-rule-evaluator ,name sexpr-p t)))))
           (def-group-sexpr (&rest names)
             (prognify
              (loop for name in names
                    append `((define-group-evaluator ,name sexpr-p t))))))
  (def-rule-sexpr ftx::phi-number ftx::phi-symbol ftx::phi-uninterned-symbol ftx::phi-lambda-list-keyword ftx::phi-as-keyword)
  (def-group-sexpr ftx::phi-keyword ftx::phi-=>-keyword ftx::phi-package-keyword ftx::phi-class-keyword ftx::phi-struct-keyword
                   ftx::phi-generic-keyword)
  (def-rule-list ftx::phi-fallback-parens ftx::phi-fallback-brackets ftx::phi-fallback-braces ftx::phi-fallback-angles ftx::phi-fallback-shells
                 ftx::phi-parens-expr ftx::phi-list-expr ftx::phi-vector-expr
                 ftx::phi-nth-index ftx::phi-aref-index ftx::phi-gethash-index
                 ftx::phi-arguments-tmp ftx::phi-method-qualifiers ftx::phi-class-parents ftx::phi-declare-list ftx::phi-class-stmt-slot-keywords
                 ftx::phi-argument-component-pattern ftx::phi-argument-component-keyword ftx::phi-argument-component-predicate)
  (def-rule-string ftx::phi-string-1 ftx::phi-string-2 ftx::phi-char))

