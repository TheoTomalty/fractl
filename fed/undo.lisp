(in-package "FED")


(defstruct (undo-node (:constructor nil))
  "An object that composes the undo tree of a buffer.  Each node has exactly one
undo-node PARENT (which may be NIL iff the node is an UNDO-START-NODE), and zero
or more CHILDREN undo-nodes."
  (parent   nil :type (or null undo-node))
  (children nil :type list))

(defstruct (undo-start-node (:include undo-node))
  "Represents the start of the undo tree of a buffer.  PARENT must be NIL, but can
have any number of CHILDREN.")

(defstruct (undo-edit-node (:include undo-node))
  (pos    nil :type (integer 0))
  (before nil :type string)
  (after  nil :type string))

(defstruct (undo-group-node (:include undo-node))
  "Starts an edit group, lasting until the next undo-group-node at the same level."
  (level nil :type (member 1 2))
  (enter-pos nil :type non-negative-integer)
  (exit-pos  nil :type (or null non-negative-integer)))

(defstruct (undo-branch-node (:include undo-node))
  "Used to distinguish an alternate path off the main tree.")

(defstruct undo-group-record
  (cursor nil :type cursor)
  (node nil :type undo-group-node)
  (debug-level nil))

(defstruct (undo-tree (:constructor %make-undo-tree))
  (node (required-argument 'node) :type undo-node)
  (start-node (required-argument 'start-node) :type undo-node)
  (saved-node (required-argument 'saved-node) :type undo-node)
  (record nil :type (or null undo-group-record)))

#| An undo group starts at the corresponding UNDO-GROUP-NODE, and continues
until then next UNDO-GROUP-NODE of the same level.  There are two situations
where a group is started: (1) when an edit command wraps edits inside the
WITH-UNDO-GROUP macro, and (2) when insert mode is entered.  Situation-2 enters
a level-1 group, and situation-1 enters a level-2 group when situation-2 is
active, and a level-1 group otherwise.

Things are complicated by the following factors:
1) Sometimes a command will want to MERGE with a previous command, in which case
   its undo group is combined with the previous one.
2) Some buffers are set to DEFAULT-INSERT, which means that non-"change"-style
   commands that enter insert mode do not activate situation-2.
3) Commands may be called recursively, and only the toplevel commands should
   activate an undo group when WITH-UNDO-GROUP is used.
4) If an abnormal error occurs at any point within a command, the debugger is
   entered before the WITH-UNDO-GROUP has fully terminated.  Within the
   debugger, the buffer may be re-opened and new toplevel commands may be
   issued.  The debugger may call a restart which resumes the original command,
   in which case further edits should fail if the original undo group has since
   been closed by intervening toplevel commands.
5) A buffer may have multiple cursors trying to edit it.

The solution is to dynamically bind *UNDO-GROUP-RECORD* in each WITH-UNDO-GROUP
to a structure type containing (1) the initiating cursor, (2) the corresponding
UNDO-GROUP-NODE, and (3) the debug level at which the group was created.  The
dynamic extent of when *UNDO-GROUP-RECORD* is non-nil is when situation-1 is
active.  Similarly, situation situation-2 is active when an undo group record is
stored in the UNDO-TREE.  The extents of situations must be nested.
1) If WITH-UNDO-GROUP is called while *UNDO-GROUP-RECORD* is already active,
   then one of the following occurs:
   1) If the requested cursor is different from the cursor in the record, the
      operation fails.
   2) If the current debug level is different from that of the record, close the
      undo group associated with the record and start a new one at the new debug
      level.
   3) Otherwise, do not open a new group or bind *UNDO-GROUP-RECORD*.
2) If no *UNDO-GROUP-RECORD* is active, then bind it with a new record using the
   relevant data.  The group node will be level-2 if there is an
   UNDO-GROUP-RECORD in the UNDO-TREE, and level-1 otherwise.
3) The *UNDO-GROUP-RECORD* is closed early when it is re-bound as part of a new
   debug-level.
4) When MERGE is used, search the UNDO-TREE backwards for the undo group node of
   the given level, and use that instead of creating a new one. |#

(defmethod print-object ((node undo-start-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":n-children ~d"
            (length (undo-node-children node)))))

(defmethod print-object ((node undo-edit-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":pos ~s :before ~s :after ~s :n-children ~d"
            (undo-edit-node-pos node)
            (undo-edit-node-before node)
            (undo-edit-node-after node)
            (length (undo-node-children node)))))

(defmethod print-object ((node undo-group-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":level ~s :enter-pos ~s :exit-pos ~s :n-children ~d"
            (undo-group-node-level node)
            (undo-group-node-enter-pos node)
            (undo-group-node-exit-pos node)
            (length (undo-node-children node)))))

(defun make-undo-tree ()
  (let ((node (make-undo-start-node)))
    (%make-undo-tree :node node :start-node node :saved-node node)))

(defun undo-node-closure (node)
  "Returns a list of all nodes reachable from NODE without undoing an edit."
  (labels ((aux (node)
             (list* node
                    (loop for child in (undo-node-children node)
                          unless (undo-edit-node-p child)
                          nconc (aux child)))))
    (aux (loop for n = node then (undo-node-parent n)
               do (when (or (undo-start-node-p n) (undo-edit-node-p n))
                    (return n))))))

(defun undo-tree-changed-p (tree)
  (not (member (undo-tree-saved-node tree) (undo-node-closure (undo-tree-node tree)))))

(defun undo-tree-save (tree)
  (setf (undo-tree-saved-node tree) (undo-tree-node tree)))

(defun undo-node-first-child (node)
  (first (undo-node-children node)))

(defun undo-node-last-child (node)
  (lastcar (undo-node-children node)))

(defun undo-tree-push-undo (tree node)
  (let ((prev-node (undo-tree-node tree)))
    (setf (undo-node-parent node) prev-node)
    (push node (undo-node-children prev-node))
    (setf (undo-tree-node tree) node)
    node))

(defun ensure-undo-group-record-debug-level ()
  (unless (awhen *undo-group-record* (eql *debug-level* (undo-group-record-debug-level it)))
    (setf *undo-group-record* nil)))

(defun buffer-push-undo-edit (buffer pos before after)
  (unless *no-undo*
    (ensure-undo-group-record-debug-level)
    (if *undo-group-record*
        (undo-tree-push-undo
         (buffer-undo-tree buffer)
         (make-undo-edit-node :pos pos :before before :after after))
        (error "Buffer edits should be wrapped in a WITH-UNDO-GROUP"))))

(defun undo-group-enter-pos (cursor pos)
  (or pos (cursor-venterpos cursor) (cursor-pos cursor)))

(defun open-undo-group (cursor &key pos merge)
  (unless *no-undo*
    (ensure-undo-group-record-debug-level)
    (if *undo-group-record*
        (if (eql cursor (undo-group-record-cursor *undo-group-record*))
            nil
            (editor-error "Another cursor is already editing this buffer"))
        (make-undo-group-record
         :cursor cursor
         :debug-level *debug-level*
         :node (let* ((tree (buffer-undo-tree (cursor-buffer cursor)))
                      (level (if (undo-tree-record tree) 2 1)))
                 (if merge
                     (or (undo-node-backward-group (undo-tree-node tree) level)
                         (error "No previous undo group at level ~d" level))
                     (undo-tree-push-undo
                      tree
                      (make-undo-group-node
                       :level level
                       :enter-pos (undo-group-enter-pos cursor pos)))))))))

(defun close-undo-group (cursor &optional (record *undo-group-record*))
  (unless (or *no-undo* (null record))
    (let ((tree (buffer-undo-tree (cursor-buffer cursor))))
      (setf (undo-group-node-exit-pos (undo-group-record-node record)) (cursor-pos cursor))
      (when (undo-node-children (undo-tree-node tree))
        (undo-tree-push-undo tree (make-undo-branch-node)))
      nil)))

(defun open-undo-tree-group (cursor &key pos)
  (ensure-undo-group-record-debug-level)
  (when *undo-group-record*
    (error "Can not open an undo-group record while within WITH-UNDO-GROUP"))
  (let ((tree (buffer-undo-tree (cursor-buffer cursor))))
    (unless (null (undo-tree-record tree))
      (error "An undo-group record is already open in buffer"))
    (setf (undo-tree-record tree)
          (make-undo-group-record
           :cursor cursor
           :debug-level *debug-level*
           :node (undo-tree-push-undo tree (make-undo-group-node :level 1 :enter-pos (undo-group-enter-pos cursor pos)))))))

(defun close-undo-tree-group (cursor)
  (ensure-undo-group-record-debug-level)
  (when *undo-group-record*
    (error "Can not close an undo-group record while within WITH-UNDO-GROUP"))
  (close-undo-group cursor (shiftf (undo-tree-record (buffer-undo-tree (cursor-buffer cursor))) nil)))

(defun undo-node-forward-group (node level)
  (if (and (undo-group-node-p node) (= level (undo-group-node-level node)))
      (loop for n = node then next
            for next = (undo-node-first-child n)
            until (or (null next) (and (undo-group-node-p next) (<= (undo-group-node-level next) level)))
            finally (return (values node n)))
      (values nil nil)))

(defun undo-node-backward-group (node level)
  (loop for n = node then (undo-node-parent n)
        until (or (null n) (and (undo-group-node-p n) (< (undo-group-node-level n) level)))
        do (when (and (undo-group-node-p n) (= (undo-group-node-level n) level))
             (return (values n node)))
        finally (return (values nil nil))))

(defun buffer-undo-node (buffer node)
  (let ((tree (buffer-undo-tree buffer)))
    (assert (eql node (undo-tree-node tree)))
    (when (undo-edit-node-p node)
      (let* ((p1 (text-pos-point (buffer-text buffer) (undo-edit-node-pos node)))
             (p2 (point-forward-char (copy-point p1) (length (undo-edit-node-after node))))
             (*no-undo* t))
        (buffer-replace buffer p1 p2
                        (lambda (after) (assert (string= after (undo-edit-node-after node))) (undo-edit-node-before node)))))
    (unless (undo-start-node-p node)
      (setf (undo-tree-node tree) (undo-node-parent node)))))

(defun buffer-redo-node (buffer node)
  (let ((tree (buffer-undo-tree buffer)))
    (assert (and (member node (undo-node-children (undo-tree-node tree))) t))
    (when (undo-edit-node-p node)
      (let* ((p1 (text-pos-point (buffer-text buffer) (undo-edit-node-pos node)))
             (p2 (point-forward-char (copy-point p1) (length (undo-edit-node-before node))))
             (*no-undo* t))
        (buffer-replace buffer p1 p2
                        (lambda (before) (assert (string= before (undo-edit-node-before node))) (undo-edit-node-after node)))))
    (setf (undo-tree-node tree) node)))

(define-editor-command undo
    (:cursor cursor :modes (:normal :insert) :mode mode :n n :record (:insert :record))
  (let* ((buffer (cursor-buffer cursor))
         (tree (buffer-undo-tree buffer))
         (level (if (undo-tree-record tree) 2 1))
         (tree-node (undo-tree-node tree))
         (start (undo-node-backward-group tree-node level)))
    (when (and (not start) (= n 1))
      (editor-error "Already at oldest change"))
    (loop repeat (1- n)
          do (aif (undo-node-backward-group (undo-node-parent start) level)
                  (setf start it)
                  (return)))
    (when start
      (loop for n = tree-node then (undo-node-parent n)
          do (buffer-undo-node buffer n)
          until (eql n start))
      (setf (cursor-pos cursor) (undo-group-node-enter-pos start)
            (cursor-snapcol cursor) nil))))

(define-editor-command redo
    (:cursor cursor :modes (:normal :insert) :mode mode :n n :record (:insert :record))
  (let* ((buffer (cursor-buffer cursor))
         (tree (buffer-undo-tree buffer))
         (level (if (undo-tree-record tree) 2 1))
         (tree-node (undo-tree-node tree)))
    (multiple-value-bind (start end) (awhen (undo-node-first-child tree-node) (undo-node-forward-group it level))
      (when (and (not end) (= n 1))
        (editor-error "Already at newest change"))
      (loop repeat (1- n)
            do (multiple-value-bind (start-next end-next)
                   (undo-node-forward-group (undo-node-first-child end) level)
                 (if end-next
                    (setf start start-next end end-next)
                    (return))))
      (when end
        (loop for n = (undo-node-first-child tree-node) then (undo-node-first-child n)
              do (buffer-redo-node buffer n)
              until (eql n end))
        (setf (cursor-pos cursor) (or (undo-group-node-exit-pos start) (undo-group-node-enter-pos start))
              (cursor-snapcol cursor) nil)))))

(defun print-undo-tree (tree)
  (loop for n = (undo-tree-start-node tree) then (undo-node-first-child n)
        while n
        do (print n)))

;; (defun redo-forward (cursor node last-p)
;;   (loop for n = node then (if last-p (undo-node-last-child n) (undo-node-first-child n))
;;         while n
;;         do (cursor-redo cursor n)))

;; (define-editor-command undo-branch
;;     (:cursor cursor :modes (:normal) :mode mode :n n)
;;   ;; TODO: lock branches so that undo/redo goes along the branch that is selected
;;   ;; TODO: implement for insert mode as well (error to go beyond open delimiter boundary)
;;   (ecase mode
;;     (:normal
;;      (loop named outer repeat n
;;            do (let ((node (buffer-undo-tree (cursor-buffer cursor))))
;;                 (when (undo-start-node-p node)
;;                   (editor-error "No previous undo branches"))
;;                 (loop for n = node then (undo-node-parent n)
;;                       with next = nil
;;                       do (cursor-undo cursor n)
;;                          (cond ((undo-start-node-p n)
;;                                 (return-from outer))
;;                                ((setf next (cdr (member n (undo-node-children (undo-node-parent n)))))
;;                                 (redo-forward cursor (car next) nil)
;;                                 (return))))))))
;;   (setf (cursor-snapcol cursor) nil))

;; (define-editor-command redo-branch
;;     (:cursor cursor :modes (:normal) :mode mode :n n)
;;   (ecase mode
;;     (:normal
;;      (loop named outer repeat n
;;            do (let ((node (buffer-undo-tree (cursor-buffer cursor)))
;;                     (path nil))
;;                 (if (undo-node-children node)
;;                     (redo-forward cursor (undo-node-last-child node) t)
;;                     (loop for n = node then (undo-node-parent n)
;;                           with i = nil
;;                           do (push n path)
;;                              (cond ((undo-start-node-p n)
;;                                     (editor-error "No next undo branches"))
;;                                    ((< 0 (setf i (position n (undo-node-children (undo-node-parent n)))))
;;                                     (loop for n in (nreverse path)
;;                                           do (cursor-undo cursor n))
;;                                     (redo-forward cursor (nth (1- i) (undo-node-children (undo-node-parent n))) t)
;;                                     (return)))))))))
;;   (setf (cursor-snapcol cursor) nil))
