(in-package "FED")


(defvar *rule-attributes* (make-hash-table))
(defvar *group-attributes* (make-hash-table))

(defmacro define-rule-attributes (name attributes)
  (declare (type symbol name))
  `(setf (gethash ',name *rule-attributes*) ,attributes))

(defmacro define-group-attributes (name attributes)
  (declare (type symbol name))
  `(setf (gethash ',name *group-attributes*) ,attributes))

(defun buffer-syntax-highlight (buffer node)
  (when (ast-ambiguous-p node)
    (return-from buffer-syntax-highlight nil))
  (macrolet ((maybe-return (expr)
               `(multiple-value-bind (attrs found-p) ,expr
                  (when found-p (return-from attributes attrs)))))
    (let ((attributes
            (block attributes
              (multiple-value-bind (rule found-p) (ast-rule node)
                (when found-p
                  (maybe-return (gethash rule *rule-attributes*))
                  (loop for group in (rule-groups rule)
                        do (maybe-return (gethash group *group-attributes*)))))
              (maybe-return (gethash (ast-symbol node) *group-attributes*))
              (aif (eval-ast node 'paint)
                   (etypecase it
                     (function
                      (funcall it (lambda (pnode attrs) (buffer-paint-node buffer pnode attrs node)))
                      (return-from buffer-syntax-highlight))
                     (cons (return-from attributes it)))
                   (return-from buffer-syntax-highlight)))))
      (buffer-paint-node buffer node attributes))))

(defun spaces (n)
  (make-string n :initial-element #\Space))

(defun buffer-line-indentation-points (buffer line)
  (let ((left (text-vcoord-point (buffer-text buffer) (coord 0 line))))
    (values left (point-back-to-indentation (copy-point left)))))

(defun buffer-line-indentation (buffer line)
  (coord-x (point-vcoord (nth-value 1 (buffer-line-indentation-points buffer line)))))

(defun buffer-line-indent (buffer line &optional prev-indent)
  (or (lambda-indent buffer (text-vcoord-point (buffer-text buffer) (coord 0 line)))
      prev-indent
      (loop for line from (1- line) downto 0
            for p2 = (point-back-to-indentation
                      (text-vcoord-point (buffer-text buffer) (coord 0 line)))
            do (unless (point-eol-p p2)
                 (return (coord-x (point-vcoord p2)))))
      0))

(defun buffer-indent-line-with (buffer line indent)
  (multiple-value-bind (p1 p2) (buffer-line-indentation-points buffer line)
    (buffer-replace buffer p1 p2 (spaces indent))
    indent))

(defun buffer-indent-line (buffer line &optional prev-indent)
  (buffer-indent-line-with buffer line (buffer-line-indent buffer line prev-indent)))

(defun buffer-indent-at (buffer point)
  (buffer-indent-line buffer (point-line point)))

(defun buffer-indent-between (buffer p1 p2)
  (loop with prev-indent = nil
        for line from (point-line p1) to (point-line p2)
        do (setf prev-indent (buffer-indent-line buffer line prev-indent))))

(define-operator (increment-indent apply-increment-indent) (p1 p2)
    (:cursor cursor :record t :venterpos venterpos)
  (let ((buffer (cursor-buffer cursor)))
    (with-undo-group (cursor :pos venterpos)
      (setf (cursor-point cursor) p1)
      (back-to-indentation :cursor cursor)
      (loop for i from (point-line p1) to (point-line p2)
            do (buffer-insert buffer (text-vcoord-point (buffer-text buffer) (coord 0 i))
                              (make-string 4 :initial-element #\Space))))))

(define-operator (decrement-indent apply-decrement-indent) (p1 p2)
    (:cursor cursor :record t :venterpos venterpos)
  (let ((buffer (cursor-buffer cursor)))
    (with-undo-group (cursor :pos venterpos)
      (setf (cursor-point cursor) p1)
      (back-to-indentation :cursor cursor)
      (loop for i from (point-line p1) to (point-line p2)
            do (let* ((left (text-vcoord-point (buffer-text buffer) (coord 0 i)))
                      (right (point-back-to-indentation (copy-point left)))
                      (indent (coord-x (point-vcoord right))))
                 (buffer-replace buffer left right (make-string (max 0 (- indent 4)) :initial-element #\Space)))))))

(define-operator (indent-operator apply-indent-operator) (p1 p2)
    (:cursor cursor :record t :venterpos venterpos)
  (with-undo-group (cursor :pos venterpos)
    (setf (cursor-point cursor) p1)
    (back-to-indentation :cursor cursor)
    (buffer-indent-between (cursor-buffer cursor) p1 p2)))

(defun ast-point-config (node point evaluator &optional (include-end-p t))
  (labels ((aux (node parents)
             (when (and (point<= (ast-origin node) point)
                        (if include-end-p
                            (point<= point (ast-offset node))
                            (point< point (ast-offset node))))
               (aif (eval-ast node evaluator)
                    (values it node parents)
                    (loop with parents = (cons node parents)
                          for child in (ast-children node)
                          do (multiple-value-bind (ret node parents) (aux child parents)
                               (when node
                                 (return (values ret node parents)))))))))
    (aux node nil)))

(defvar *ast-trace*)

(define-editor-command indent-line
    (:cursor cursor :modes (:insert) :record (:insert :record))
  (let* ((buffer (cursor-buffer cursor))
         (line (cursor-line cursor))
         (move-cursor-p (<= (coord-x (cursor-vcoord cursor))
                            (coord-x (point-vcoord (point-back-to-indentation (cursor-point cursor)))))))
    (with-undo-group cursor
      (buffer-indent-line buffer line)
      (when move-cursor-p
        (back-to-indentation :cursor cursor))
      (setf (cursor-snapcol cursor) nil))))

(define-editor-command smart-tab
    (:cursor cursor :modes (:insert) :record (:insert :record))
  (let* ((buffer (cursor-buffer cursor))
         (line (cursor-line cursor))
         (current-indent (coord-x (point-vcoord (point-back-to-indentation (cursor-point cursor))))))
    (if (<= (coord-x (cursor-vcoord cursor)) current-indent)
        (with-undo-group cursor
          (buffer-indent-line-with buffer line
                                   (* (ceiling (1+ current-indent) (buffer-tabsize buffer))
                                      (buffer-tabsize buffer)))
          (back-to-indentation :cursor cursor))
        (multiple-value-bind (complete-function node parents)
            (ast-point-config (or (buffer-ast buffer) (editor-error "Syntax not enabled"))
                              (cursor-point cursor)
                              'complete-function)
          (unless node
            (editor-error "Nothing to complete at point"))
          (multiple-value-bind (completed-p string position)
              (let ((*ast-trace* parents))
                (funcall complete-function cursor node))
            (when completed-p
              (with-undo-group cursor
                (with-marks ((left-mark (ast-origin node) :right-inserting))
                  (buffer-replace (cursor-buffer cursor) (ast-origin node) (ast-offset node) string)
                  (setf (cursor-point cursor) (point-forward-char (copy-point left-mark) position)
                        (cursor-snapcol cursor) nil)))))))))

(defparameter *open-delimiters* "([{⟨⦗⟦⟪<“‘")
(defparameter *close-delimiters* ")]}⟩⦘⟧⟫>”’")
(defparameter *symmetric-delimiters* "|\"'`")

(defun open-delimiter-p (ch)
  (and (find ch *open-delimiters*) t))

(defun close-delimiter-p (ch)
  (and (find ch *close-delimiters*) t))

(defun symmetric-delimiter-p (ch)
  (and (find ch *symmetric-delimiters*) t))

(defun delimiter-p (ch)
  (or (open-delimiter-p ch)
      (close-delimiter-p ch)
      (symmetric-delimiter-p ch)))

(defun alternate-delimiter (delim)
  (case delim
    (#\< #\⟨)
    (#\( #\⦗)
    (#\[ #\⟦)
    (#\{ #\⟪)
    (#\' #\’)
    (#\` #\‘)
    (#\" #\“)
    ;; TODO: what character to use for alternate double quote?
    (#\> #\⟩)
    (#\) #\⦘)
    (#\] #\⟧)
    (#\} #\⟫)))

(defun close-delimiter (ch)
  (aif (position ch *open-delimiters*)
       (aref *close-delimiters* it)
       (when (or (position ch *close-delimiters*)
                 (position ch *symmetric-delimiters*))
         ch)))

(defun open-delimiter (ch)
  (aif (position ch *close-delimiters*)
       (aref *open-delimiters* it)
       (when (or (position ch *open-delimiters*)
                 (position ch *symmetric-delimiters*))
         ch)))

(defun matching-delimiter (ch)
  (aif (position ch *open-delimiters*)
       (aref *close-delimiters* it)
       (aif (position ch *close-delimiters*)
            (aref *open-delimiters* it)
            (and (position ch *symmetric-delimiters*) ch))))

(defun buffer-delimited-node-at (buffer point direction &optional (open-p t) (close-p t))
  (labels ((point-delimits-node-p (node)
             (let ((at-open-p
                     (and open-p
                          (ecase direction
                            (:forward (point= (ast-origin node) point))
                            (:backward (and (point-peek-prev point)
                                            (point= (ast-origin node) (point-skip-prev (copy-point point))))))))
                   (at-close-p
                     (and close-p
                          (ecase direction
                            (:forward
                             (and (point-peek point)
                                  (point= (ast-offset node) (point-skip (copy-point point)))))
                            (:backward
                             (point= (ast-offset node) point))))))
               (and (or at-open-p at-close-p)
                    (eval-ast node 'delimited-p)
                    (or (and at-open-p (not (eval-ast node 'missing-open-delim-p)))
                        (and at-close-p (not (eval-ast node 'missing-close-delim-p)))))))
           (aux (node)
             (when (and (point<= (ast-origin node) point)
                        (point<= point (ast-offset node)))
               (loop for child in (ast-children node)
                     do (awhen (aux child)
                          (return it))
                     finally (return
                               (when (point-delimits-node-p node)
                                 node))))))
    (awhen (buffer-ast buffer)
      (aux it))))

(defun buffer-search-delim (buffer point direction delim &optional (n 1) (allow-escape-syntax-p t))
  (assert (delimiter-p delim))
  (setf point (copy-point point))
  (let ((open-delim (open-delimiter delim))
        (close-delim (close-delimiter delim))
        (peek (ecase direction (:forward #'point-peek) (:backward #'point-peek-prev)))
        (skip (ecase direction (:forward #'point-skip) (:backward #'point-skip-prev)))
        (ast-near (ecase direction (:forward #'ast-origin) (:backward #'ast-offset)))
        (ast-far (ecase direction (:forward #'ast-offset) (:backward #'ast-origin))))
    (assert (and open-delim close-delim) nil "~s is not a known delimiter" delim)
    (loop with level = 0
          for ch = (funcall peek point)
          while ch
          do (aif (and (delimiter-p ch)
                       (buffer-delimited-node-at buffer point direction))
                  (if (point= point (funcall ast-near it))
                      (if (and (char= delim ch) (= 0 (decf n)))
                          (return point)
                          (merge-point point (funcall ast-far it)))
                      (if allow-escape-syntax-p
                          (if (and (char= delim ch) (= 0 (decf n)))
                              (return point)
                              (progn
                                (setf level 0)
                                (merge-point point (funcall ast-far it))))
                          (return nil)))
                  (if (and (char= delim ch)
                           (= 0 level)
                           (= 0 (decf n)))
                      (return point)
                      (progn
                        (unless (char= open-delim close-delim)
                          (cond ((char= open-delim ch)
                                 (incf level))
                                ((char= close-delim ch)
                                 (decf level))))
                        (setf level (ecase direction
                                      (:forward (max 0 level))
                                      (:backward (min 0 level))))
                        (funcall skip point)))))))

(defun buffer-match-delim (buffer point direction &optional match-symmetric-p)
  (let ((delim (ecase direction (:forward (point-peek point)) (:backward (point-peek-prev point))))
        (skip (ecase direction (:forward #'point-skip) (:backward #'point-skip-prev))))
    (when (or (ecase direction (:forward (open-delimiter-p delim)) (:backward (close-delimiter-p delim)))
              (symmetric-delimiter-p delim))
      (aif (buffer-delimited-node-at buffer point direction (eql direction :forward) (eql direction :backward))
           (ecase direction (:forward (ast-offset it)) (:backward (ast-origin it)))
           (unless (and (not match-symmetric-p) (symmetric-delimiter-p delim))
             (awhen (buffer-search-delim buffer (funcall skip (copy-point point)) direction (matching-delimiter delim) 1 nil)
               (funcall skip it)))))))

(defun search-delimiter-aux (cursor n operator delim direction)
  (unless (delimiter-p delim)
    (editor-error "~s is not a delimiter" delim))
  (aif (buffer-search-delim (cursor-buffer cursor)
                            (ecase direction
                              (:forward (cursor-point-after cursor))
                              (:backward (cursor-point cursor)))
                            direction delim n)
       (if operator
           (order-points :point (cursor-point cursor) it)
           (progn
             (ecase direction
               (:forward (setf (cursor-point cursor) it))
               (:backward (setf (cursor-point-after cursor) it)))
             (setf (cursor-snapcol cursor) nil)))
       (editor-error "Delimiter ~s not found" delim)))

(defun decode-delimiter (key)
  (aif (char-meta key)
       (or (alternate-delimiter it)
           (editor-error "Unknown meta-key delimiter ~s" it))
       key))

(define-editor-command search-delimiter-forward
    (:cursor cursor :modes (:normal :visual) :n n :operator operator :record (:visual :record)
     :keys ((delim (read-input))))
  (case delim
    ((#\' #\`) (search-mark-aux cursor n operator delim :forward))
    (t (search-delimiter-aux cursor n operator (decode-delimiter delim) :forward))))

(define-editor-command search-delimiter-backward
    (:cursor cursor :modes (:normal :visual) :n n :operator operator :record (:visual :record)
     :keys ((delim (read-input))))
  (case delim
    ((#\' #\`) (search-mark-aux cursor n operator delim :backward))
    (t (search-delimiter-aux cursor n operator (decode-delimiter delim) :backward))))

(defun in/around-delimiter-aux (cursor n open-delim close-delim around-p)
  (let* ((buffer (cursor-buffer cursor))
         (point (buffer-search-delim buffer (cursor-point-after cursor) :backward open-delim n))
         (next-point nil))
    (if point
        (point-skip-prev point)
        (setf point (buffer-search-delim buffer (cursor-point cursor) :forward open-delim n)))
    (unless (and point (setf next-point (buffer-match-delim buffer point :forward t)))
      (editor-error "Delimiters ~a~a not found" open-delim close-delim))
    (if around-p
        (values :point point next-point)
        (values :point (point-skip point) (point-skip-prev next-point)))))

(macrolet ((def (open-delim close-delim in-name around-name)
             `(progn
                (define-text-object ,in-name
                    (:cursor cursor :n n :record (:visual :record))
                  (in/around-delimiter-aux cursor n ,open-delim ,close-delim nil))
                (define-text-object ,around-name
                    (:cursor cursor :n n :record (:visual :record))
                  (in/around-delimiter-aux cursor n ,open-delim ,close-delim t)))))
  (def #\( #\) in-parenthesis around-parenthesis)
  (def #\[ #\] in-bracket around-bracket)
  (def #\{ #\} in-brace around-brace)
  (def #\' #\' in-single-quote around-single-quote)
  (def #\" #\" in-double-quote around-double-quote)
  (def #\` #\` in-back-quote around-back-quote)
  (def #\< #\> in-narrow-angle-bracket around-narrow-angle-bracket)
  (def #\⟨ #\⟩ in-angle-bracket around-angle-bracket)
  (def #\⦗ #\⦘ in-shell-bracket around-shell-bracket)
  (def #\⟦ #\⟧ in-double-bracket around-double-bracket)
  (def #\⟪ #\⟫ in-double-angle-bracket around-double-angle-bracket)
  (def #\‘ #\’ in-rich-single-quote around-rich-single-quote)
  (def #\“ #\” in-rich-double-quote around-rich-double-quote))

(define-editor-command match-delimiter
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :n-nullable-p t :operator operator :record (:visual :record))
  (let ((buffer (cursor-buffer cursor)))
    (if n
        (let* ((nlines (buffer-nlines buffer))
               (line (cond ((< 100 n) (editor-error "Percentage too large"))
                           ((= 100 n) (1- nlines))
                           (t (truncate (* nlines (/ (1- n) 100)))))))
          (if operator
              (select-lines (cursor-buffer cursor) (cursor-line cursor) line)
              (progn
                (setf (cursor-vcoord cursor) (coord 0 line))
                (back-to-indentation :cursor cursor))))
        (aif (buffer-match-delim buffer (cursor-point cursor) :forward)
             (if operator
                 (values :point (cursor-point cursor) it)
                 (setf (cursor-point-after cursor) (progn (window-set-jump (cursor-window cursor)) it)
                       (cursor-snapcol cursor) nil))
             (aif (buffer-match-delim buffer (cursor-point-after cursor) :backward)
                  (if operator
                      (values :point it (cursor-point-after cursor))
                      (setf (cursor-point cursor) (progn (window-set-jump (cursor-window cursor)) it)
                            (cursor-snapcol cursor) nil))
                  (editor-error "No matching delimiter"))))))

(defun buffer-comment-prefix (buffer point)
  (assert (eql (buffer-text buffer) (point-text point)))
  (labels ((aux (node)
             (when (and (point<= (ast-origin node) point) (point<= point (ast-offset node)))
               (or (loop for child in (ast-children node)
                         do (awhen (aux child)
                              (return it)))
                   (eval-ast node 'comment-prefix)))))
    (awhen (buffer-ast buffer)
      (aux it))))

(defun uncomment-region (cursor comment-string p1 p2 venterpos)
  (let ((buffer (cursor-buffer cursor)))
    (with-undo-group (cursor :pos venterpos)
      (save-excursion cursor
        (with-cursor-properties (cursor :mode :insert)
          (with-marks ((m2 p2 :left-inserting))
            (setf (cursor-point cursor) p1)
            (back-to-indentation :cursor cursor)
            (loop do (unless (point-eol-p (cursor-point cursor))
                       (buffer-delete buffer (cursor-point cursor) (length comment-string))
                       (when (eql #\Space (point-peek (cursor-point cursor)))
                         (buffer-delete buffer (cursor-point cursor) 1)))
                  until (<= (point-line m2) (cursor-line cursor))
                  do (next-line :cursor cursor)
                     (back-to-indentation :cursor cursor))))))))

(defun comment-region (cursor comment-string p1 p2 venterpos)
  (let ((buffer (cursor-buffer cursor)))
    (with-undo-group (cursor :pos venterpos)
      (save-excursion cursor
        (with-cursor-properties (cursor :mode :insert)
          (with-marks ((m2 p2 :left-inserting))
            (setf (cursor-point cursor) p1)
            (when (point< (point-back-to-indentation (cursor-point cursor)) (cursor-point cursor))
              (buffer-insert buffer (cursor-point cursor) (string #\Newline))
              (indent-line :cursor cursor))
            (beginning-of-line :cursor cursor)
            (loop do (unless (point-eol-p (point-back-to-indentation (cursor-point cursor)))
                       (buffer-insert buffer (cursor-point cursor) (format nil "~a " comment-string)))
                  until (<= (point-line m2) (cursor-line cursor))
                  do (next-line :cursor cursor)
                     (beginning-of-line :cursor cursor)
                  finally (unless (point= (point-end-of-line (copy-point m2)) m2)
                            (buffer-insert buffer m2 (string #\Newline))
                            (setf (cursor-point cursor) m2)
                            (indent-line :cursor cursor)))))))))

(define-operator (comment-operator apply-comment-operator) (p1 p2)
    (:cursor cursor :record t :venterpos venterpos)
  (let ((buffer (cursor-buffer cursor)))
    (aif (buffer-comment-prefix buffer p1)
         (if (loop with point = (point-back-to-indentation (copy-point p1))
                   always (or (point-eol-p point)
                              (loop for ch across it
                                    do (if (eql ch (point-peek point))
                                           (point-skip point)
                                           (return nil))
                                    finally (return t)))
                   until (<= (point-line p2) (point-line point))
                   do (point-end-of-line point 2)
                      (point-back-to-indentation point))
             (uncomment-region cursor it p1 p2 venterpos)
             (comment-region cursor it p1 p2 venterpos))
         (editor-error "Could not determine comment prefix")))
  (setf (cursor-snapcol cursor) nil))

(defun definition-location (buffer point &optional (include-end-p t))
  (unless (buffer-ast buffer)
    (editor-error "Not a Lisp buffer"))
  (let ((node (point-enclosing-node-in point (buffer-ast buffer) '(nointern-symbol)
                                       :right-inclusive include-end-p
                                       :recursive nil)))
    (when node
      (with-buffer-package buffer (symbol-definition-location (eval-ast node 'nointern-symbol))))))

(defun symbol-definition-location (symbol)
  (block nil
    (let* (;; TODO: implement interactive selection on multiple matches
           (source (or (loop for type in '(:class :structure :condition
                                           :macro :function :generic-function :method
                                           :type :variable :constant :symbol-macro)
                             do (awhen (sb-introspect:find-definition-sources-by-name symbol type)
                                  (return (first it))))
                       (return)))
           (pathname (or (awhen (sb-introspect:definition-source-pathname source)
                           (translate-logical-pathname it))
                         (return)))
           (plist (sb-introspect:definition-source-plist source))
           (character-offset (sb-introspect:definition-source-character-offset source))
           (form-path (sb-introspect:definition-source-form-path source))
           (pos nil))
      (flet ((next-sexpr-start-from (pos)
               (awhen (evm-parse (read-file-string pathname) 'ftx::lambda-read :start pos)
                 (ast-origin it))))
        (cond ((setf pos (getf plist :fed-pos))
               (values pathname pos))
              (character-offset
               (awhen (next-sexpr-start-from character-offset)
                 (values pathname it)))
              (form-path
               (awhen (next-sexpr-start-from
                       (with-open-file (s pathname)
                         (let ((*read-suppress* t))
                           (loop repeat (first form-path) do (read s nil))
                           (file-position s))))
                 (values pathname it))))))))

(define-editor-command goto-tag
    (:cursor cursor :modes (:normal :visual :insert) :mode mode)
  (let ((cursor-point (cursor-point cursor)))
    (multiple-value-bind (pathname pos) (definition-location (cursor-buffer cursor) (cursor-point cursor) (eql :insert mode))
      (if pathname
          (progn
            (window-set-jump (cursor-window cursor))
            (multiple-value-bind (buffer cursor) (find-file pathname)
              (declare (ignore buffer))
              (setf (cursor-pos cursor) pos
                    (cursor-snapcol cursor) nil)))
          (editor-error "Could not find definition"))
      (push (cons (cursor-buffer cursor) (make-mark-left-inserting cursor-point)) (tags)))))

(define-editor-command pop-tag
    (:cursor cursor :modes (:normal :visual :insert))
  (if (tags)
      (destructuring-bind (buffer . mark) (pop (tags))
        (unwind-protect
            (progn
              (window-set-jump (cursor-window cursor))
              (switch-to-buffer buffer)
              (let ((cursor (current-cursor)))
                (setf (cursor-point cursor) mark
                      (cursor-snapcol cursor) nil)))
          (delete-mark mark)))
      (editor-error "No more tags")))

(macrolet ((def (name ast-point point-cmp)
             `(defun ,name (point parent cmp)
                (labels ((aux (node)
                           (loop for child in (sort (ast-children node)
                                                    (lambda (n1 n2) (,point-cmp (,ast-point n1) (,ast-point n2))))
                                 do (when (,point-cmp point (,ast-point child))
                                      (if (funcall cmp child)
                                          (return child)
                                          (awhen (aux child)
                                            (return it)))))))
                  (awhen (aux parent)
                    (,ast-point it))))))
  (def point-prev-node-start-in ast-origin point>)
  (def point-prev-node-end-in ast-offset point>)
  (def point-next-node-start-in ast-origin point<)
  (def point-next-node-end-in ast-offset point<))

(defun point-enclosing-node-in (point parent cmp &key (inclusive nil inclusive-p) (left-inclusive t) (right-inclusive t) (recursive t) trace)
  (when inclusive-p
    (setf left-inclusive inclusive
          right-inclusive inclusive))
  (let ((left-point-cmp (if left-inclusive #'point<= #'point<))
        (right-point-cmp (if right-inclusive #'point<= #'point<))
        (cmp (if (listp cmp)
                 (lambda (node) (loop for evaluator in cmp thereis (eval-ast node (the symbol evaluator))))
                 cmp)))
    (labels ((aux (node parents)
               (when (and (funcall left-point-cmp (ast-origin node) point)
                          (funcall right-point-cmp point (ast-offset node)))
                 (let ((matched-p (funcall cmp node)))
                   (if (or recursive (not matched-p))
                       (loop with new-parents = (when trace (cons node parents))
                             for child in (ast-children node)
                             do (multiple-value-bind (node parents) (aux child new-parents)
                                  (when node
                                    (return (values node parents))))
                             finally (return (if matched-p
                                                 (values node parents)
                                                 (values nil nil))))
                       node)))))
      (multiple-value-bind (node parents) (aux parent nil)
        (if trace (values node parents) node)))))

(macrolet ((def (name point-search type)
             (let ((evaluator (ecase type (:sexpr 'sexpr-p) ((:list :defun) 'list-p)))
                   (toplevel-p (eql type :defun)))
               `(defun ,name (point node)
                  (awhen (and node (or ,@(unless toplevel-p `((point-enclosing-node-in point node '(list-p) :inclusive nil)))
                                       (when (eval-ast node 'toplevel-p)
                                         node)))
                    (,point-search point it (lambda (node) (eval-ast node ',evaluator))))))))
  (def point-next-sexpr point-next-node-end-in :sexpr)
  (def point-prev-sexpr point-prev-node-start-in :sexpr)
  (def point-next-list point-next-node-end-in :list)
  (def point-prev-list point-prev-node-start-in :list)
  (def point-next-defun point-next-node-end-in :defun)
  (def point-prev-defun point-prev-node-start-in :defun))

(defun point-up-list (point node)
  (when node
    (let ((list (point-enclosing-node-in point node '(list-p) :inclusive nil)))
      (when list
        (ast-origin list)))))

(defun point-down-list (point node)
  (awhen (point-next-list point node)
    (point-forward-char (point-prev-list it node))))

(macrolet ((def (name point-move message)
             `(define-editor-command ,name
                  (:cursor cursor :modes (:insert) :n n :record (:insert :record))
                (with-move-or-error (cursor (editor-error ,message) n)
                  (loop with move-point = nil and node = (buffer-ast (cursor-buffer cursor))
                        with point = (cursor-point cursor)
                        repeat n while point
                        do (awhen (setf point (,point-move point node))
                             (setf move-point it))
                        finally (when move-point
                                  (setf (cursor-point cursor) move-point
                                        (cursor-snapcol cursor) nil)))))))
  (def forward-sexpr point-next-sexpr "No next sexpr")
  (def backward-sexpr point-prev-sexpr "No previous sexpr")
  (def forward-list point-next-list "No next list")
  (def backward-list point-prev-list "No previous list")
  (def beginning-of-defun point-prev-defun "No previous defun")
  (def end-of-defun point-next-defun "No next defun")
  (def up-list point-up-list "No up list")
  (def down-list point-down-list "No down list"))

(define-editor-command delete-sexpr
    (:cursor cursor :modes (:insert) :n n :register r :record (:insert :record)
     :keys ((repeat (and (toplevel-p) (eql *previous-binding* 'delete-sexpr)))
            (true-r (or r (when repeat (editor-variable 'previous-edit-register)) #\"))))
  (when (toplevel-p)
    (setf (editor-variable 'previous-edit-register) true-r))
  (let ((p1 (cursor-point cursor))
        (p2 (save-excursion cursor
              (forward-sexpr :cursor cursor :n n)
              (cursor-point cursor))))
      (with-undo-group cursor
        (buffer-delete-between-copy-register (cursor-buffer cursor) p1 p2 true-r :point (when repeat :after))
        (setf (cursor-snapcol cursor) nil))))

(define-editor-command transpose-sexprs
    (:cursor cursor :modes (:insert) :n n :record (:insert :record))
  (let ((node (buffer-ast (cursor-buffer cursor))))
    (transpose-aux
     cursor
     n
     (lambda (point)
       (let* ((w1 (point-prev-sexpr point node))
              (w2 (and w1 (point-next-sexpr w1 node)))
              (t2 (and w2 (point-next-sexpr w2 node)))
              (t1 (and t2 (point-prev-sexpr t2 node))))
         (if (or (null w1) (null w2))
             (editor-error "No previous sexpr")
             (values w1 w2 (or t1 (copy-point w2)) (or t2 t1 (copy-point w2)))))))))

(define-editor-command indent-sexpr
    (:cursor cursor :modes (:insert) :n n :record (:insert :record))
  (multiple-value-bind (p1 p2) (save-excursion cursor
                                 (forward-sexpr :cursor cursor :n n)
                                 (let ((p2 (cursor-point cursor)))
                                   (backward-sexpr :cursor cursor :n n)
                                   (values (cursor-point cursor) p2)))
    (with-undo-group cursor
      (buffer-indent-between (cursor-buffer cursor) p1 p2))))

(defun format-paragraph-aux (string max-column)
  (with-output-to-string (stream)
    (labels ((space-p (ch)
               (member ch '(#\Space #\Newline)))
             (next-ws/word (idx)
               (let* ((ws-end (or (position-if-not #'space-p string :start idx) (length string)))
                      (word-end (or (position-if #'space-p string :start ws-end) (length string))))
                 (values ws-end word-end))))
      (loop with columns = 0 and first = t and i = 0 and j = 0 and k = 0
            do (setf i k
                     (values j k) (next-ws/word i))
               (tagbody
                begin
                  (let ((ws (subseq string i j))
                        (word (subseq string j k)))
                    (cond ((string= "" word)
                           (return))
                          (first
                           (setf first nil)
                           (write-string word stream)
                           (setf columns (length word)))
                          (t
                           (when (< 1 (count #\Newline ws))
                             (write-string (remove #\Space ws) stream)
                             (setf first t
                                   i j)
                             (go begin))
                           (when (eql #\. (aref word (1- (length word))))
                             (multiple-value-bind (j-next k-next) (next-ws/word k)
                               (when (and (= 1 (- j-next k)) (eql #\Space (aref string k)))
                                 ;; Treat j to k-next as if it is one word
                                 (setf k k-next)
                                 (go begin))))
                           (let ((n-spaces (cond ((not (find #\Newline ws))
                                                  (count #\Space ws))
                                                 ((and (< 0 i) (member (aref string (1- i)) '(#\. #\? #\!))) 2)
                                                 (t 1))))
                             (if (< max-column (+ (length word) n-spaces columns))
                                 (progn
                                   (format stream "~%~a" word)
                                   (setf columns (length word)))
                                 (progn
                                   (format stream "~a~a" (make-string n-spaces :initial-element #\Space) word)
                                   (incf columns (+ (length word) n-spaces)))))))))))))

(define-editor-command format-paragraph
    (:cursor cursor :modes (:insert) :record (:insert :record))
  (multiple-value-bind (p1 p2) (point-select-paragraph (cursor-point cursor))
    (with-undo-group cursor
      (let ((buffer (cursor-buffer cursor)))
        (buffer-replace buffer p1 p2 (lambda (text) (format-paragraph-aux text (buffer-variable buffer 'fill-column))))))))

(defun compute-formatting-between (node buffer p1 p2)
  (let ((ret nil))
    (labels ((aux (node)
               (unless (or (point<= p2 (ast-origin node))
                           (point<= (ast-offset node) p1))
                 (aif (eval-ast node 'formatter)
                      (push (list (make-mark-left-inserting (ast-origin node))
                                  (make-mark-left-inserting (ast-offset node))
                                  (funcall it node buffer))
                            ret)
                      (loop for child in (ast-children node)
                            do (aux child))))))
      (when node
        (aux node)
        ret))))

(define-operator (format-operator apply-format-operator) (p1 p2 type)
    (:cursor cursor :record t :venterpos venterpos)
  (with-marks ((m1 p1))
    (let ((formatting (compute-formatting-between (buffer-ast (cursor-buffer cursor)) (cursor-buffer cursor) p1 p2)))
      (unwind-protect
          (with-undo-group (cursor :pos venterpos)
            (loop while formatting
                  do (destructuring-bind (mstart mend string) (pop formatting)
                       (when (point< mstart m1)
                         (move-mark m1 mstart))
                       (unwind-protect
                           (buffer-replace (cursor-buffer cursor) mstart mend string)
                         (delete-mark mstart)
                         (delete-mark mend))))
            (setf (cursor-point cursor) (ecase type (:line (point-back-to-indentation (copy-point m1))) (:point m1))
                  (cursor-snapcol cursor) nil))
        (loop for (m1 m2) in formatting do (delete-mark m1) (delete-mark m2))))))

(defun first-number-node-between (node p1 p2)
  (let ((ret1 nil) (ret2 nil))
    (labels ((aux (node)
               (unless (or (point<= p2 (ast-origin node))
                           (point<= (ast-offset node) p1))
                 (aif (eval-ast node 'number-formatter)
                      (when (or (null ret1) (point< (ast-origin node) (ast-origin ret1)))
                        (setf ret1 node
                              ret2 it))
                      (loop for child in (ast-children node)
                            do (aux child))))))
      (if node
          (progn (aux node) (values ret1 ret2))
          (values nil nil)))))

(defun point-not-at-numeral (p)
  (aif (point-peek p) (not (char<= #\0 it #\9)) t))

(defun point-not-at-numeral-prev (p)
  (aif (point-peek-prev p) (not (char<= #\0 it #\9)) t))

(defun first-number-between (node p1 p2)
  (multiple-value-bind (node formatter) (first-number-node-between node p1 p2)
    (let ((p (copy-point p1))
          (p-end (if node (if (point<= p1 (ast-origin node)) (ast-origin node) (copy-point p1)) (copy-point p2))))
      (if (loop until (point= p p-end) always (point-not-at-numeral p) do (point-skip p))
          (if node
              (values (handler-case (eval-ast node) (error (e) (editor-error "~a" e))) (ast-origin node) (ast-offset node) formatter)
              (values nil nil nil nil))
          (let* ((p1 (point-backward-until-maybe (copy-point p) #'point-not-at-numeral-prev))
                 (p2 (point-forward-until-maybe (copy-point p) #'point-not-at-numeral))
                 (n (parse-integer (text-substring (point-text p1) (point-pos p1) (point-pos p2))
                                   :radix 10)))
            ;; NOTE: Uncomment to let non-syntax numbers be interpreted as negative
            ;; (when (eql #\- (point-peek-prev p1))
            ;;  (point-skip-prev p1)
            ;;  (setf n (- n)))
            (values n p1 p2 (lambda (n) (princ-to-string (max 0 n)))))))))

(defun modify-number-aux (cursor mode n increment-p alt-p)
  (ecase mode
    (:normal
     (multiple-value-bind (node-n p1 p2 formatter)
         (first-number-between (buffer-ast (cursor-buffer cursor))
                               (cursor-point cursor)
                               (point-end-of-line (cursor-point cursor)))
       (unless node-n
         (editor-error "No number after cursor"))
       (with-marks ((offset p2 :left-inserting))
         (with-undo-group cursor
           (buffer-replace (cursor-buffer cursor) p1 p2
                           (funcall formatter (if increment-p (+ node-n n) (- node-n n))))
           (setf (cursor-point-after cursor) offset
                 (cursor-snapcol cursor) nil)))))
    (:visual
     (multiple-value-bind (p1 p2) (cursor-visual-bounds cursor)
       (let* ((buffer (cursor-buffer cursor))
              (ast (buffer-ast buffer))
              (text (buffer-text buffer))
              (venterpos (cursor-venterpos cursor)))
         (normal-mode :cursor cursor)
         (with-marks ((m1 p1 :right-inserting)
                      (m2 p2 :left-inserting))
           (with-undo-group (cursor :pos venterpos)
             (let ((m1-line (point-line m1))
                   (m2-line (point-line m2))
                   (line-multiplier 1)
                   (modifier-fn (if increment-p #'+ #'-)))
               (loop for i below (1+ (- m2-line m1-line)) and line from m1-line
                     do (multiple-value-bind (node-n p1 p2 formatter)
                            (first-number-between ast
                                                  (if (= line m1-line) (copy-point m1) (text-vcoord-point text (coord 0 line)))
                                                  (if (= line (1- m2-line)) (copy-point m2) (text-vcoord-point text (coord -1 line))))
                          (when node-n
                            (when (and (= line m1-line) (point< p1 m1))
                              (move-mark m1 p1))
                            (buffer-replace buffer p1 p2
                                            (lambda (prev-text)
                                              ;; Check for newlines because they could mess up the line logic of this loop
                                              (assert (null (find #\Newline prev-text)))
                                              (let ((ret (funcall formatter (funcall modifier-fn node-n (* n (if alt-p line-multiplier 1))))))
                                                (assert (null (find #\Newline ret)))
                                                ret)))
                            (incf line-multiplier)))))
             (setf (cursor-point cursor) (copy-point m1)
                   (cursor-snapcol cursor) nil))))))))

(define-editor-command increment-number
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :record (:visual :commit-record))
  (modify-number-aux cursor mode n t nil))

(define-editor-command decrement-number
    (:cursor cursor :modes (:normal :visual) :mode mode :n n :record (:visual :commit-record))
  (modify-number-aux cursor mode n nil nil))

(define-editor-command increment-number-alt
    (:cursor cursor :modes (:visual) :mode mode :n n :record (:visual :commit-record))
  (modify-number-aux cursor mode n t t))

(define-editor-command decrement-number-alt
    (:cursor cursor :modes (:visual) :mode mode :n n :record (:visual :commit-record))
  (modify-number-aux cursor mode n nil t))
