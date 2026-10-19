(defpackage "FCL"
  (:use "CL")
  (:export
   ;; Types
   #:positive-integer
   #:non-negative-integer
   
   ;; Array functions
   #:make-dynamic-array
   
   ;; Boolean functions
   #:xor
   
   ;; Integer functions
   #:align-extend #:sign
   
   ;; List functions
   #:flatten #:listify #:prognify #:unprognify #:lastcar
   
   ;; Hash tables and alist functions
   #:gethash-strict #:copy-hash-table #:hash-table-alist #:alist-hash-table
   #:assoc-value #:assoc-value-strict #:hashash
   
   ;; Errors
   #:unimplemented #:unreachable #:required-argument
   
   ;; File and string functions
   #:compile-and-load #:read-file-string
   #:string-invcase #:unescape
   #:char-digit #:char-caret #:caret-char
   #:char-width #:true-char-width
   
   ;; Image introspection
   #:make-special-variable #:special-variable-p #:quasiquote #:unquote
   
   ;; Macros
   #:awhen #:aif #:it #:once-only #:with-gensyms #:incf* #:decf*
   #:abort-protect #:comptime #:bind #:bind* #:macro-bind #:tagbody*
   
   ;; Constants
   #:defconstant-eqx #:define-constant
   
   ;; Scoped blocks
   #:progs #:define #:macro
   
   ;; Switch macro
   #:switch-macro #:switch-macro-translate #:switch
   
   ;; Doubly linked lists
   #:dll-node #:make-dll-node #:dll-node-next #:dll-node-prev
   #:dll-node-data #:dll-node-list #:dll #:make-dll #:dll-start
   #:delete-dll-node #:dll-push #:dll-dolist #:dll-dolist-maybe
   #:dll-list #:dll-member
   
   ;; Ring buffer
   #:ring-buffer #:make-ring-buffer #:ring-buffer-p #:copy-ring-buffer
   #:ring-buffer-buffer #:ring-buffer-wrap #:ring-buffer-start #:ring-buffer-end
   #:ring-buffer-size #:ring-buffer-length #:ring-buffer-full-p #:ring-buffer-aref
   #:ring-buffer-push #:ring-buffer-pop
   
   ;; Testing
   #:test #:test-suite #:run-suite #:run-test #:delete-test #:delete-suite
   #:test-equal
   
   ;; Aliases
   #:is #:== #:=== #:==== #:<- #:->)
  (:import-from "SB-INT" #:quasiquote)
  (:import-from "SB-IMPL" #:unquote))

(defpackage "FTX"
  (:use "CL" "FCL")
  (:export
   #:point #:point-p #:copy-point #:point-text #:merge-point #:make-point #:fmake-point
   #:point= #:point< #:point> #:point<= #:point>=
   #:fpoint= #:fpoint< #:fpoint> #:fpoint<= #:fpoint>=
   #:rpoint= #:rpoint< #:rpoint> #:rpoint<= #:rpoint>=
   #:mark #:mark-p #:mark-type #:copy-mark #:delete-mark #:mark-text #:move-mark
   #:make-mark-left-inserting #:make-mark-right-inserting #:with-marks
   #:fmake-mark-left-inserting #:fmake-mark-right-inserting
   #:rmake-mark-left-inserting #:rmake-mark-right-inserting
   #:point-peek       #:point-skip       #:point-next
   #:point-peek-prev  #:point-skip-prev  #:point-prev
   #:fpoint-peek      #:fpoint-skip      #:fpoint-next
   #:fpoint-peek-prev #:fpoint-skip-prev #:fpoint-prev
   #:rpoint-peek      #:rpoint-skip      #:rpoint-next
   #:mark-peek        #:mark-skip        #:mark-next
   #:mark-peek-prev   #:mark-skip-prev   #:mark-prev
   #:mark= #:mark< #:mark> #:mark<= #:mark>=
   
   #:text #:text-p #:make-text #:text-grammar-id #:text-prefix #:text-tabsize
   #:text-pos-point #:text-coord-point #:text-vcoord-point #:text-clear-state
   #:point-coord #:point-vcoord #:point-line #:point-pos #:fpoint-pos
   #:text-substring #:text-string #:string-text #:text-length #:text-end-coord
   #:point-leaf #:point-offset #:point-insert #:point-replace #:point-delete
   #:point-delete-between #:point-invalidate #:point-invalidate-between
   #:point-reductions #:point-trace #:point-susps #:point-char
   #:fpoint-reductions #:fpoint-trace #:fpoint-susps #:fpoint-char
   #:rpoint-reductions #:rpoint-trace #:rpoint-susps #:rpoint-char
   
   #:rich-string #:rich-string-mapc
   
   #:region #:make-region #:fmake-region #:delete-region #:copy-region #:region-p
   #:region-data #:region-type #:region-left #:region-right
   #:region-advance-right #:region-retract-left #:region-change-right-type #:region-change-left-type
   #:do-regions-between #:do-regions-at #:do-marks-at #:do-leaves
   
   #:coord #:coord-x #:coord-y #:coord0 #:print-coord
   #:coord= #:coord+ #:coord- #:lcoord< #:lcoord> #:lcoord<= #:lcoord>=
   
   #:ast-p #:ast #:ast-symbol #:ast-rule #:ast-length #:ast-string #:ast-children #:ast-ambiguous-p
   #:ast-origin #:ast-offset #:ast-variable
   
   #:grammar #:grammar-p #:grammar-id
   #:grammar-evaluators #:coerce-grammar
   #:eval-ast #:load-ast #:read-ast #:package-ast
   #:evm-run #:evm-run-strict #:evm-parse #:evm-parse-error #:evm-eval #:evm-load
   #:*debug-evm*
   #:define-syntax #:define-syntax-group #:define-chars #:define-rule-evaluator #:define-group-evaluator
   #:rule-groups
   #:rmakunbound #:gmakunbound #:group-attributes
   #:encoded-char-map-ips
   
   #:char-class #:char-class-trace #:char-class-trace-list
   #:eof #:eof-trace #:any #:any-trace
   #:asm #:alt #:many #:more #:maybe
   #:peek #:capt #:capture #:intersperse
   #:match #:match-trace #:match-trace-list
   #:advance #:stop #:offset #:set-offset
   #:reject #:trace-return #:trace-reduce
   #:trace-range #:trace-range-list #:trace-push #:trace-push-list
   
   #:fractl #:fractl-load #:lisp))

(defpackage "FTX-RUN/STRING"
  (:use "CL" "FCL" "FTX"))

(defpackage "FTX-RUN/TEXT"
  (:use "CL" "FCL" "FTX"))

(defpackage "FED"
  (:use "CL" "FCL" "FTX")
  (:export
   #:frame #:frame-p #:frame-editor #:frame-stream
   #:frame-windows #:frame-focused-window #:frame-selected-window
   #:frame-dims #:frame-width #:frame-height
   #:frame-global-render-state #:frame-render-state #:frame-prev-render-state
   
   #:full-frame #:full-frame-p #:make-full-frame
   #:full-frame-main-window #:full-frame-minibuffer-window
   #:full-frame-selected-window #:full-frame-focused-window
   #:full-frame-minibuffer
   
   #:repl-frame #:repl-frame-p #:make-repl-frame
   #:repl-frame-window #:repl-frame-max-height
   
   #:frame-display-output-hook #:frame-expand-repl-hook #:frame-recentre-focused-window-hook
   #:frame-render #:frame-draw #:frame-handle-winch
   #:frame-modeline #:frame-default-minibuffer
   #:switch-to-frame #:frame-focus-window
   #:clear-frame #:exit-frames
   #:switch-frame #:refresh-screen #:handle-sigwinch
   
   #:define-default-grammar #:define-editor-command
   
   #:buffers
   #:buffer-name #:buffer-text #:buffer-file #:buffer-keymap #:buffer-grammar
   #:buffer-reset #:buffer-revert #:buffer-prefix-annotate
   #:cursor-buffer #:cursor-frame #:cursor-point #:cursor-pos #:cursor-vcoord
   #:cursor-mode
   #:frame-stream #:current-frame
   
   #:editor-condition #:editor-error #:editor-cancel #:editor-exit
   #:bob-error #:eob-error #:bol-error #:eol-error
   
   #:exit-frame #:enter-frame #:enter-repl
   #:find-file #:normal-mode #:insert-mode
   
   #:load-digraphs-file #:save-digraphs-file))

(require :sb-posix)
(require :sb-introspect)

(defpackage "FSH"
  (:use "CL" "FCL")
  (:export
   #:set-exit-code #:shell-boundp #:shell-variable #:shell-fboundp #:shell-error
   #:init-shell #:define-fsh-command #:abort-fsh-command))
