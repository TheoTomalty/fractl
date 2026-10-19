(in-package "FTX")


(declaim (optimize (debug 3)))

(deftype leaf () 'vector)
(deftype text-node () '(or leaf branch))

(defconstant max-string-size 13)
(defconstant leaf-header-size 7)
(defconstant leaf-size (+ leaf-header-size (* 4 max-string-size) 3))
(defconstant leaf-offset-bits 4)
(defconstant leaf-entry-size 4)

(defstruct (leaf (:type vector) (:constructor nil) (:copier nil))
  (text    nil)
  (parent  nil)
  (marks   nil :type list)
  (regions nil :type list)
  (prev    nil :type (or null leaf))
  (next    nil :type (or null leaf))
  (length  0   :type (integer 0 #.max-string-size)))

(defun make-leaf (&key text parent marks regions prev next (length 0))
  (let ((ret (make-array leaf-size)))
    (setf (leaf-text ret) text)
    (setf (leaf-parent ret) parent)
    (setf (leaf-marks ret) marks)
    (setf (leaf-regions ret) regions)
    (setf (leaf-prev ret) prev)
    (setf (leaf-next ret) next)
    (setf (leaf-length ret) length)
    ret))

(defstruct branch
  (text     nil)
  (parent   nil)
  (length   0 :type fixnum)
  (newlines 0 :type fixnum)
  (col      0 :type fixnum)
  (vcol     0 :type fixnum)
  (tjoin    0 :type (or fixnum null))
  (height   0 :type fixnum)
  (regions nil :type list)
  (left   nil :type text-node)
  (right  nil :type text-node))

(defstruct (region (:constructor %%make-region)
                   (:copier %copy-region))
  (node nil :type text-node)
  (left-leaf nil :type leaf)
  (%left-idx nil :type fixnum)
  (right-leaf nil :type leaf)
  (%right-idx nil :type fixnum)
  (data nil))

(deftype rich-string () `(or string cons vector))

(defstruct (text (:constructor %make-text)
                 (:copier nil))
  (grammar-id nil :type (or null fixnum))
  (root nil :type text-node)
  (%prefix "" :type rich-string)
  (%tabsize 4 :type (integer 0)))

(defun text-tabsize (text)
  (text-%tabsize text))

(defun text-prefix (text)
  (text-%prefix text))

(defun text-first-leaf (text)
  (let ((node (text-root text)))
    (loop while (branch-p node)
          do (setf node (branch-left node)))
    node))

(defun text-last-leaf (text)
  (let ((node (text-root text)))
    (loop while (branch-p node)
          do (setf node (branch-right node)))
    node))

(defun (setf text-prefix) (new text)
  (rich-string-mapc (lambda (ch) (assert (not (eql #\Newline ch)))) new)
  (setf (text-%prefix text) new)
  (text-node-update-recurse (text-first-leaf text))
  new)

(declaim (inline leaf-entry-offset))
(defun leaf-entry-offset (i)
  (+ leaf-header-size (* i leaf-entry-size)))

(macrolet ((def (name offset)
             `(progn
                (declaim (inline ,name (setf ,name)))
                (defun ,name (leaf i)
                  (svref leaf (+ (leaf-entry-offset i) ,offset)))
                (defun (setf ,name) (new leaf i)
                  (setf (svref leaf (+ (leaf-entry-offset i) ,offset)) new)))))
  (def rpoint-reductions 0)
  (def rpoint-trace 1)
  (def rpoint-susps 2)
  (def rpoint-char 3))

(defun rich-string-mapc (function s)
  (declare (type rich-string s))
  (etypecase s
    (string (loop for ch across s do (funcall function ch)))
    (vector (loop for s across s do (rich-string-mapc function s)))
    (cons (rich-string-mapc function (car s))))
  s)

(defun %do-leaf-chars (leaf function)
  (when (null (leaf-prev leaf))
    (rich-string-mapc (lambda (ch) (funcall function ch -1)) (text-prefix (leaf-text leaf))))
  (loop for i below (leaf-length leaf)
        do (funcall function (rpoint-char leaf i) i)))
  
(defmacro do-leaf-chars ((var-and-idx leaf &optional result) &body body)
  (destructuring-bind (var &optional (idx (gensym "I") idx-p)) (listify var-and-idx)
    `(block nil
       (%do-leaf-chars ,leaf (lambda (,var ,idx)
                               (declare (ignore ,@(when (not idx-p) (list idx))))
                               ,@body))
       ,result)))

(macrolet ((def (name &body (branch-accessor leaf-part))
             `(progn
                (defun ,name (node)
                  (etypecase node
                    (branch (,branch-accessor node))
                    (leaf ,(if (symbolp leaf-part) `(,leaf-part node) leaf-part))))
                ,@(when (symbolp leaf-part)
                    `((defun (setf ,name) (new node)
                        (etypecase node
                          (branch (setf (,branch-accessor node) new))
                          (leaf (setf (,leaf-part node) new)))))))))

  (def text-node-text branch-text leaf-text)
  (def text-node-parent branch-parent leaf-parent)
  (def text-node-length branch-length leaf-length)
  (def text-node-regions branch-regions leaf-regions)

  (def text-node-newlines branch-newlines
    (loop for i from 0 below (leaf-length node)
          count (eql #\Newline (rpoint-char node i))))

  (def text-node-col branch-col
    (loop for i from (1- (leaf-length node)) downto 0
          do (when (eql #\Newline (rpoint-char node i))
               (return (- (leaf-length node) (1+ i))))
          finally (return (leaf-length node))))

  (def text-node-vcol branch-vcol
    (let ((sum 0) (tabsize (text-tabsize (leaf-text node))))
      (do-leaf-chars (ch node sum)
        (if (char= #\Newline ch)
            (setf sum 0)
            (incf sum (true-char-width ch sum tabsize))))))

  (def text-node-tjoin branch-tjoin
    (let ((sum 0) (summing t))
      (do-leaf-chars (ch node (unless summing sum))
        (case ch
          (#\Newline (setf sum 0 summing t))
          (#\Tab (setf summing nil))
          (t (when summing (incf sum (char-width ch)))))))))

(defmethod print-object ((branch branch) stream)
  (print-unreadable-object (branch stream :type t :identity t)
    (format stream ":length ~s" (branch-length branch))))

(defmethod print-object ((text text) stream)
  (print-unreadable-object (text stream :type t :identity t)
    (let* ((max-length 20)
           (string (text-substring text 0 max-length)))
      (when (< max-length (text-length text))
        (setf string (concatenate 'string string "...")))
      (format stream ":string ~s" string))))

(defun init-states (leaf i)
  (setf (rpoint-reductions leaf i) (make-dll)
        (rpoint-trace leaf i) nil
        (rpoint-susps leaf i) nil))

(defun make-text ()
  (let* ((leaf (make-leaf))
         (text (%make-text :root leaf)))
    (init-states leaf 0)
    (setf (leaf-text leaf) text)))

(defun text-clear-state (text)
  (point-invalidate-between (text-pos-point text 0) (text-pos-point text -1))
  (setf (text-grammar-id text) nil))

(defun set-branch-left (new branch)
  (setf (branch-left branch) new
        (text-node-parent new) branch))

(defun set-branch-right (new branch)
  (setf (branch-right branch) new
        (text-node-parent new) branch))

(defun summary= (s1 s2)
  (and (eql (text-node-length s1) (text-node-length s2))
       (eql (text-node-newlines s1) (text-node-newlines s2))
       (eql (text-node-col s1) (text-node-col s2))
       (eql (text-node-vcol s1) (text-node-vcol s2))
       (eql (text-node-tjoin s1) (text-node-tjoin s2))))

(defun tadd (width1 tjoin width2 tabsize)
  (if tjoin
      (+ (align-extend (+ width1 tjoin 1) tabsize)
         (- width2 (align-extend (1+ tjoin) tabsize)))
      (+ width1 width2)))

(defun vcoord-add (c1 tj1 c2 tj2 tabsize)
  (values
   (coord (if (/= 0 (coord-y c2))
              (coord-x c2)
              (tadd (coord-x c1) tj2 (coord-x c2) tabsize))
          (+ (coord-y c1) (coord-y c2)))
   (if (/= 0 (coord-y c2))
       tj2
       (or tj1 (when tj2 (+ (coord-x c1) tj2))))))

(defun merge-summaries (s-ret s1 s2)
  (setf (branch-length s-ret)
        (+ (text-node-length s1)
           (text-node-length s2)))
  (setf (branch-newlines s-ret)
        (+ (text-node-newlines s1)
           (text-node-newlines s2)))
  (setf (branch-col s-ret)
        (if (/= 0 (text-node-newlines s2))
            (text-node-col s2)
            (+ (text-node-col s1)
               (text-node-col s2))))
  (multiple-value-bind (vcoord tjoin)
      (vcoord-add (coord (text-node-vcol s1) (text-node-newlines s1))
                  (text-node-tjoin s1)
                  (coord (text-node-vcol s2) (text-node-newlines s2))
                  (text-node-tjoin s2)
                  (text-tabsize (branch-text s-ret)))
    (setf (branch-vcol s-ret) (coord-x vcoord) (branch-tjoin s-ret) tjoin))
  s-ret)

(defun text-node-height (text-node)
  (etypecase text-node
    (branch (branch-height text-node))
    (leaf 0)))

(defun branch-bf (branch)
  (- (text-node-height (branch-right branch))
     (text-node-height (branch-left  branch))))

(defun text-node-bf (text-node)
  (etypecase text-node
    (branch (branch-bf text-node))
    (leaf 0)))

(defun branch-update-height (branch)
  (setf (branch-height branch)
        (1+ (max (text-node-height (branch-left branch))
                 (text-node-height (branch-right branch))))))

(defun branch-update (branch)
  (branch-update-height branch)
  (merge-summaries branch (branch-left branch) (branch-right branch))
  branch)

(defun switch-in-parent (new old parent)
  (if (null parent)
      (setf (text-root (text-node-text new)) new
            (text-node-parent new) parent)
      (cond ((eq old (branch-left  parent))
             (set-branch-left  new parent))
            ((eq old (branch-right parent))
             (set-branch-right new parent))
            (t (error "unreachable")))))

(defun branch-idx-position (branch)
  (+ leaf-offset-bits (1- (branch-height branch))))

(defun text-node-idx-size (node)
  (etypecase node
    (branch (1+ (branch-idx-position node)))
    (leaf leaf-offset-bits)))

(defun branch-tag-idx (branch child idx)
  (dpb (cond ((eq child (branch-left branch)) 0)
             ((eq child (branch-right branch)) 1)
             (t (unreachable)))
       (byte 1 (branch-idx-position branch))
       idx))

(defun split-regions (branch right-p)
  (let ((sub-branch (if right-p (branch-right branch) (branch-left branch)))
        (outer-left nil)
        (outer-right nil))
    (loop for cons = (shiftf (branch-regions branch) nil) then next
          while cons
          for next = (cdr cons)
          and region = (car cons)
          do (if (= 1 (ldb (byte 1 (branch-idx-position sub-branch))
                           (if right-p (region-right-idx region) (region-left-idx region))))
                 (setf outer-right (rplacd cons outer-right))
                 (setf outer-left (rplacd cons outer-left)))
             (setf (ldb (byte 1 (branch-idx-position branch)) (region-left-idx region)) 0
                   (ldb (byte 1 (branch-idx-position branch)) (region-right-idx region)) 0)
             (if right-p
                 (setf (ldb (byte 1 (branch-idx-position sub-branch)) (region-right-idx region)) 0)
                 (setf (ldb (byte 1 (branch-idx-position sub-branch)) (region-left-idx region)) 0)))
    (loop for region in (branch-regions sub-branch)
          do (setf (ldb (byte 1 (branch-idx-position sub-branch)) (region-left-idx region)) 0
                   (ldb (byte 1 (branch-idx-position sub-branch)) (region-right-idx region)) 0))
    (values outer-left outer-right (shiftf (branch-regions sub-branch) nil))))

;; NOTE: some bit-zeroing is unnecessary here, but do it anyway for better clarity

(defun merge-regions-inner (branch regions)
  (loop for region in regions
        do (setf (region-node region) branch
                 (ldb (byte 1 (branch-idx-position branch)) (region-left-idx region)) 0
                 (ldb (byte 1 (branch-idx-position branch)) (region-right-idx region)) 1))
  regions)

(defun merge-regions-outer (branch regions right-p bit)
  (loop with sub-branch = (if right-p (branch-right branch) (branch-left branch))
        for region in regions
        do (if right-p
               (setf (ldb (byte 1 (branch-idx-position sub-branch)) (region-right-idx region)) bit)
               (setf (ldb (byte 1 (branch-idx-position sub-branch)) (region-left-idx region)) bit)))
  (merge-regions-inner branch regions))

(defun branch-rotate-left (n1)
  "Rotates branches left:
  1              3
 / \            / \
2   3    ->    1   5
   / \        / \
  4   5      2   4
"
  (let* ((parent (branch-parent n1))
         (n3 (branch-right n1))
         (n4 (branch-left n3)))
    (multiple-value-bind (r24 r25 r45) (split-regions n1 t)
      (set-branch-right n4 n1)
      (set-branch-left n1 n3)
      (branch-update n1)
      (branch-update n3)
      (switch-in-parent n3 n1 parent)
      (setf (branch-regions n1) (merge-regions-inner n1 r24)
            (branch-regions n3) (nconc (merge-regions-outer n3 r25 nil 0)
                                       (merge-regions-outer n3 r45 nil 1))))
    (text-node-structural-update-recurse n3)
    n3))

(defun branch-rotate-right (n1)
  "Rotates branches right:
    1          2
   / \        / \
  2   3  ->  4   1
 / \            / \
4   5          5   3
"
  (let* ((parent (branch-parent n1))
         (n2 (branch-left n1))
         (n5 (branch-right n2)))
    (multiple-value-bind (r43 r53 r45) (split-regions n1 nil)
      (set-branch-left n5 n1)
      (set-branch-right n1 n2)
      (branch-update n1)
      (branch-update n2)
      (switch-in-parent n2 n1 parent)
      (setf (branch-regions n1) (merge-regions-inner n1 r53)
            (branch-regions n2) (nconc (merge-regions-outer n2 r43 t 1)
                                       (merge-regions-outer n2 r45 t 0))))
    (text-node-structural-update-recurse n2)
    n2))

(defun branch-rebalance (branch)
  (case (branch-bf branch)
    (-2
     (when (= +1 (branch-bf (branch-left branch)))
       (branch-rotate-left (branch-left branch)))
     (branch-rotate-right branch))
    (+2
     (when (= -1 (branch-bf (branch-right branch)))
       (branch-rotate-right (branch-right branch)))
     (branch-rotate-left branch))
    (t branch)))

(defun text-node-update-recurse (node)
  "Updates the branch summaries, recursively on parents of NODE.  Use on
a NODE whenever it changes without affecting tree structure."
  (loop for n = (text-node-parent node) then (branch-parent n)
        while n
        do (let ((height (branch-height n)))
             (branch-update n)
             (assert (= height (branch-height n))))))

(defun text-node-structural-update-recurse (node)
  "Updates the branch heights, recursively on parents of NODE.  Use on a
NODE whose internal structure has changed.  Any region idx that pass
through this node in parents are re-computed."
  (loop with i = 0 and idx-start = nil
        for n = node then p
        for p = (text-node-parent node) then (branch-parent p)
        while p
        do (if idx-start
               (setf i (branch-tag-idx p n i))
               (setf i (branch-tag-idx p n 0)
                     idx-start (branch-idx-position p)))
           (let ((idx-end (1+ (branch-idx-position p)))
                 (height-changed (/= 0 (- (branch-height p) (branch-update-height p)))))
             (loop for region in (text-node-regions p)
                   do (when (or height-changed (= i (mask-field (byte (- idx-end idx-start) idx-start) (region-left-idx region))))
                        (region-update-left-idx region))
                      (when (or height-changed (= i (mask-field (byte (- idx-end idx-start) idx-start) (region-right-idx region))))
                        (region-update-right-idx region)))
             (when height-changed
               (setf idx-start nil)))))

(defun branch-rebalance-recurse (branch)
  (when branch
    (loop for n = branch then (branch-parent n)
          while n
          do (setq n (branch-rebalance n)))))

(defstruct (point (:constructor make-point (leaf offset))
                  (:copier nil))
  (leaf   nil :type leaf)
  (offset nil :type (integer 0)))

(defun copy-point (point)
  (let ((ret (make-point (point-leaf point) (point-offset point))))
    ret))

(defstruct (mark (:include point)
                 (:copier %copy-mark)
                 (:constructor make-mark (%type leaf offset)))
  (%type nil :type (member :left-inserting :right-inserting)))

(defun mark-type (mark)
  (mark-%type mark))

(defun (setf mark-type) (new mark)
  (setf (mark-%type mark) new)
  (mark-adjust mark)
  new)

(defun move-mark (mark point)
  (let ((old-leaf (mark-leaf mark)))
    (setf (mark-leaf mark) (point-leaf point)
          (mark-offset mark) (point-offset point))
    (mark-adjust mark old-leaf)
    mark))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t :identity t)
    (format stream ":pos ~s" (point-pos point))))

(defmethod print-object ((mark mark) stream)
  (print-unreadable-object (mark stream :type t :identity t)
    (format stream ":type ~s :pos ~s" (mark-type mark) (point-pos mark))))

(defun merge-point (point-ret point)
  (assert (not (mark-p point-ret)))
  (setf (point-leaf point-ret) (point-leaf point))
  (setf (point-offset point-ret) (point-offset point))
  point-ret)

(declaim (inline fpoint-bias-right))
(defun fpoint-bias-right (leaf offset)
  (let ((next (leaf-next leaf)))
    (if (and next (= (leaf-length leaf) offset))
        (values next 0)
        (values leaf offset))))

(defun point-bias-right (point)
  (loop for leaf = (point-leaf point)
        for next = (leaf-next leaf)
        while (and next (= (point-offset point) (leaf-length leaf)))
        do (setf (point-leaf point) next
                 (point-offset point) 0))
  point)

(declaim (inline fpoint-bias-left))
(defun fpoint-bias-left (leaf offset)
  (let ((prev (leaf-prev leaf)))
    (if (and prev (= 0 offset))
        (values prev (leaf-length prev))
        (values leaf offset))))

(defun point-bias-left (point)
  (loop for leaf = (point-leaf point)
        for prev = (leaf-prev leaf)
        while (and prev (= 0 (point-offset point)))
        do (setf (point-leaf point) prev
                 (point-offset point) (leaf-length prev)))
  point)

(defun rpoint-p (leaf offset)
  (multiple-value-bind (l2 o2) (fpoint-bias-right leaf offset)
    (and (eql leaf l2) (eql offset o2))))

(defun lpoint-p (leaf offset)
  (multiple-value-bind (l2 o2) (fpoint-bias-left leaf offset)
    (and (eql leaf l2) (eql offset o2))))

(defun leaf< (l1 l2)
  (unless (eq l1 l2)
    (loop with n1 = l1 and n2 = l2
          with p1 = (leaf-parent n1) and p2 = (leaf-parent n2)
          until (eq p1 p2)
          do (if (<= (text-node-height p1) (text-node-height p2))
                 (setf n1 p1 p1 (branch-parent n1))
                 (setf n2 p2 p2 (branch-parent n2)))
          finally (return (eq n1 (branch-left p1))))))

(defun leaf> (l1 l2)
  (leaf< l2 l1))

(defmacro with-point-biased-right ((leaf offset) point &body body)
  (let ((point-sym (gensym "POINT")))
    `(let* ((,point-sym ,point)
            (,leaf (point-leaf ,point-sym))
            (,offset (point-offset ,point-sym)))
       (setf (values ,leaf ,offset) (fpoint-bias-right ,leaf ,offset))
       ,@body)))

(defmacro with-point-biased-left ((leaf offset) point &body body)
  (let ((point-sym (gensym "POINT")))
    `(let* ((,point-sym ,point)
            (,leaf (point-leaf ,point-sym))
            (,offset (point-offset ,point-sym)))
       (setf (values ,leaf ,offset) (fpoint-bias-left ,leaf ,offset))
       ,@body)))

(declaim (inline rpoint=))
(macrolet ((def (name fname rname offset-cmp leaf-cmp)
             `(progn
                (declaim (inline ,rname))
                (defun ,rname (l1 o1 l2 o2)
                  (if (eq l1 l2) (,offset-cmp o1 o2) (,leaf-cmp l1 l2)))
                (defun ,fname (l1 o1 l2 o2)
                  (setf (values l1 o1) (fpoint-bias-right l1 o1))
                  (setf (values l2 o2) (fpoint-bias-right l2 o2))
                  (,rname l1 o1 l2 o2))
                (defun ,name (p1 p2)
                  (,fname (point-leaf p1) (point-offset p1) (point-leaf p2) (point-offset p2))))))
  (defun rpoint= (l1 o1 l2 o2)
    (and (eql l1 l2) (eql o1 o2)))
  (defun fpoint= (l1 o1 l2 o2)
    (setf (values l1 o1) (fpoint-bias-right l1 o1))
    (setf (values l2 o2) (fpoint-bias-right l2 o2))
    (rpoint= l1 o1 l2 o2))
  (defun point= (p1 p2)
    (fpoint= (point-leaf p1) (point-offset p1) (point-leaf p2) (point-offset p2)))
  (def point< fpoint< rpoint< < leaf<)
  (def point> fpoint> rpoint> > leaf>)
  (def point<= fpoint<= rpoint<= <= leaf<)
  (def point>= fpoint>= rpoint>= >= leaf>))

(macrolet ((def (name fname rname char-p modify-p)
             `(progn
                (declaim (inline ,rname))
                (defun ,rname (leaf offset ,@(when char-p `(&aux char)))
                  (when (< offset (leaf-length leaf))
                    ,@(when char-p `((setf char (rpoint-char leaf offset))))
                    ,@(when modify-p
                        `((incf offset)
                          (setf (values leaf offset) (fpoint-bias-right leaf offset)))))
                  (values ,@(when modify-p `(leaf offset)) ,@(when char-p `(char))))
                (defun ,fname (leaf offset ,@(when char-p `(&aux char)))
                  (setf (values leaf offset) (fpoint-bias-right leaf offset))
                  (when (< offset (leaf-length leaf))
                    ,@(when char-p `((setf char (rpoint-char leaf offset))))
                    ,@(when modify-p `((incf offset))))
                  (values ,@(when modify-p `(leaf offset)) ,@(when char-p `(char))))
                (defun ,name (point ,@(when char-p `(&aux char)))
                  (setf (values ,@(when modify-p `((point-leaf point) (point-offset point)))
                                ,@(when char-p `(char)))
                        (,fname (point-leaf point) (point-offset point)))
                  ,(if char-p 'char 'point)))))
  (def point-peek fpoint-peek rpoint-peek t nil)
  (def point-skip fpoint-skip rpoint-skip nil t)
  (def point-next fpoint-next rpoint-next t t))

(macrolet ((def (name fname char-p modify-p)
             `(progn
                (defun ,fname (leaf offset ,@(when char-p `(&aux char)))
                  (setf (values leaf offset) (fpoint-bias-left leaf offset))
                  (when (< 0 offset)
                    ,@(when char-p `((setf char (rpoint-char leaf (1- offset)))))
                    ,@(when modify-p `((decf offset))))
                  (values ,@(when modify-p `(leaf offset)) ,@(when char-p `(char))))
                (defun ,name (point ,@(when char-p `(&aux char)))
                  (setf (values ,@(when modify-p `((point-leaf point) (point-offset point)))
                                ,@(when char-p `(char)))
                        (,fname (point-leaf point) (point-offset point)))
                  ,(if char-p 'char 'point)))))
  (def point-peek-prev fpoint-peek-prev t nil)
  (def point-skip-prev fpoint-skip-prev nil t)
  (def point-prev fpoint-prev t t))

(macrolet ((def (name fname accessor)
             `(progn
                (defun ,fname (leaf offset)
                  (setf (values leaf offset) (fpoint-bias-right leaf offset))
                  (,accessor leaf offset))
                (defun (setf ,fname) (new leaf offset)
                  (setf (values leaf offset) (fpoint-bias-right leaf offset))
                  (setf (,accessor leaf offset) new))
                (defun ,name (point)
                  (with-point-biased-right (leaf offset) point
                    (,accessor leaf offset)))
                (defun (setf ,name) (new point)
                  (with-point-biased-right (leaf offset) point
                    (setf (,accessor leaf offset) new))))))
  (def point-reductions fpoint-reductions rpoint-reductions)
  (def point-trace fpoint-trace rpoint-trace)
  (def point-susps fpoint-susps rpoint-susps)
  (def point-char fpoint-char rpoint-char))

(declaim (inline rmake-mark-left-inserting))
(defun rmake-mark-left-inserting (leaf offset &optional (cons (cons 0 0)))
  (let ((mark (make-mark :left-inserting leaf offset)))
    (setf (car cons) mark
          (cdr cons) (leaf-marks leaf)
          (leaf-marks leaf) cons)
    mark))

(declaim (inline rmake-mark-right-inserting))
(defun rmake-mark-right-inserting (leaf offset &optional (cons (cons 0 0)))
  (setf (values leaf offset) (fpoint-bias-left leaf offset))
  (let ((mark (make-mark :right-inserting leaf offset)))
    (setf (car cons) mark
          (cdr cons) (leaf-marks leaf)
          (leaf-marks leaf) cons)
    mark))

(declaim (inline fpoint-bias-mark-type))
(defun fpoint-bias-mark-type (type leaf offset)
  (ecase type
    (:left-inserting
     (fpoint-bias-right leaf offset))
    (:right-inserting
     (fpoint-bias-left leaf offset))))

(defun fpoint-bias-right* (leaf offset)
  "Same as FPOINT-BIAS-RIGHT except that it also skips over empty
leaves (which is usually not allowed)."
  (loop (let ((prev-leaf leaf))
          (setf (values leaf offset) (fpoint-bias-right leaf offset))
          (when (eq prev-leaf leaf)
            (return (values leaf offset))))))

(defun fpoint=* (l1 o1 l2 o2)
  (setf (values l1 o1) (fpoint-bias-right* l1 o1)
        (values l2 o2) (fpoint-bias-right* l2 o2))
  (and (eq l1 l2)
       (= o1 o2)))

(defun fpoint-bias-mark-type* (type leaf offset)
  "Same as FPOINT-BIAS-MARK-TYPE except that it also handles the case
where LEAF is empty (which is usually not allowed)."
  (if (= 0 (leaf-length leaf))
      (let ((next (leaf-next leaf))
            (prev (leaf-prev leaf)))
        (cond ((and (or (eql :left-inserting type) (null prev)) (not (null next)))
               (values next 0))
              ((and (or (eql :right-inserting type) (null next)) (not (null prev)))
               (values prev (leaf-length prev)))
              (t (values leaf 0))))
      (fpoint-bias-mark-type type leaf offset)))

(defun fmake-mark-left-inserting (leaf offset)
  (let ((type :left-inserting))
    (setf (values leaf offset) (fpoint-bias-mark-type type leaf offset))
    (let ((mark (make-mark type leaf offset)))
      (push mark (leaf-marks (mark-leaf mark)))
      mark)))

(defun fmake-mark-right-inserting (leaf offset)
  (let ((type :right-inserting))
    (setf (values leaf offset) (fpoint-bias-mark-type type leaf offset))
    (let ((mark (make-mark type leaf offset)))
      (push mark (leaf-marks (mark-leaf mark)))
      mark)))

(defun make-mark-left-inserting (point)
  (fmake-mark-left-inserting (point-leaf point) (point-offset point)))

(defun make-mark-right-inserting (point)
  (fmake-mark-right-inserting (point-leaf point) (point-offset point)))

(defmacro with-marks (bindings &body body)
  `(let ,(loop for binding in bindings
               collect (destructuring-bind (name point &optional (type :left-inserting)) binding
                         `(,name (,(case type
                                     (:left-inserting 'make-mark-left-inserting)
                                     (:right-inserting 'make-mark-right-inserting)
                                     (t `(lambda (point)
                                           (ecase ,type
                                             (:left-inserting (make-mark-left-inserting point))
                                             (:right-inserting (make-mark-right-inserting point))))))
                                  ,point))))
     (unwind-protect (progn ,@body)
       ,@(loop for (name) in bindings
               collect `(delete-mark ,name)))))

(defun copy-mark (mark)
  (let ((mark (%copy-mark mark)))
    (push mark (leaf-marks (mark-leaf mark)))
    mark))

(defun delete-mark-from (mark leaf)
  (let ((marks (leaf-marks leaf)))
    (if (eq mark (car marks))
        (progn (setf (leaf-marks leaf) (cdr marks)) marks)
        (loop for cons on marks
              do (when (eq mark (cadr cons))
                   (return (prog1 (cdr cons)
                             (setf (cdr cons) (cddr cons)))))))))

(defun delete-mark (mark)
  (delete-mark-from mark (mark-leaf mark))
  nil)

(defun mark-adjust (mark &optional (old-leaf (mark-leaf mark)) cons)
  (setf (values (mark-leaf mark) (mark-offset mark))
        (fpoint-bias-mark-type* (mark-type mark) (mark-leaf mark) (mark-offset mark)))
  (let ((new-leaf (mark-leaf mark)))
    (when (or cons (not (eql old-leaf new-leaf)))
      (setf (leaf-marks new-leaf)
            (rplacd (or cons (delete-mark-from mark old-leaf)) (leaf-marks new-leaf))))))

(defun mark-peek (mark)
  (point-peek mark))
(defun mark-skip (mark)
  (let ((leaf (mark-leaf mark)))
    (point-skip mark)
    (mark-adjust mark leaf)))
(defun mark-next (mark)
  (let ((leaf (mark-leaf mark)))
    (prog1 (point-next mark)
      (mark-adjust mark leaf))))
(defun mark-peek-prev (mark)
  (point-peek-prev mark))
(defun mark-skip-prev (mark)
  (let ((leaf (mark-leaf mark)))
    (point-skip-prev mark)
    (mark-adjust mark leaf)))
(defun mark-prev (mark)
  (let ((leaf (mark-leaf mark)))
    (prog1 (point-prev mark)
      (mark-adjust mark leaf))))

(defun mark= (m1 m2)
  (and (eq (mark-type m1) (mark-type m2))
       (eq (mark-leaf m1) (mark-leaf m2))
       (eq (mark-offset m1) (mark-offset m2))))
(defun mark< (m1 m2)
  (if (eq (mark-leaf m1) (mark-leaf m2))
      (or (< (mark-offset m1) (mark-offset m2))
          (and (= (mark-offset m1) (mark-offset m2))
               (eql :right-inserting (mark-type m1))
               (eql :left-inserting (mark-type m2))))
      (leaf< (mark-leaf m1) (mark-leaf m2))))
(defun mark> (m1 m2) (mark< m2 m1))
(defun mark<= (m1 m2)
  (or (mark< m1 m2) (mark= m1 m2)))
(defun mark>= (m1 m2)
  (or (mark< m2 m1) (mark= m2 m1)))

(defmethod print-object ((region region) stream)
  (print-unreadable-object (region stream :type t :identity t)
    (format stream ":left ~s :right ~s :data ~s" (point-pos (region-left region)) (point-pos (region-right region)) (region-data region))
    ;; (format stream ":left ~b :right ~b" (region-%left-idx region) (region-%right-idx region))
    ))

(defun int-to-mark-type (i)
  (ecase i
    (0 :left-inserting)
    (1 :right-inserting)))

(defun mark-type-to-int (type)
  (ecase type
    (:left-inserting 0)
    (:right-inserting 1)))

(defun fpoint-idx (leaf offset node)
  (loop with i = offset
        for n = leaf then p
        for p = (leaf-parent leaf) then (branch-parent n)
        while p
        do (setf (ldb (byte 1 (branch-idx-position p)) i)
                 (cond ((eq n (branch-left p)) 0)
                       ((eq n (branch-right p)) 1)
                       (t (unreachable))))
        until (eq p node)
        finally (return i)))

(defun region-left-type (region)
  (int-to-mark-type (ldb (byte 1 0) (region-%left-idx region))))

(defun (setf region-left-type) (new region)
  (setf (ldb (byte 1 0) (region-%left-idx region)) (mark-type-to-int new)))

(defun region-left-idx (region)
  (ash (region-%left-idx region) -1))

(defun (setf region-left-idx) (new region)
  (setf (region-%left-idx region)
        (logior (ash new 1)
                (ldb (byte 1 0) (region-%left-idx region))))
  new)

(defun region-left-offset (region)
  (ldb (byte leaf-offset-bits 0) (region-left-idx region)))

(defun (setf region-left-offset) (new region)
  (setf (ldb (byte leaf-offset-bits 0) (region-left-idx region)) new))

(defun region-update-left-idx (region)
  (setf (region-left-idx region) (fpoint-idx (region-left-leaf region) (region-left-offset region) (region-node region))))

(defun region-fleft (region)
  (values (region-left-leaf region) (region-left-offset region)))

(defun region-left (region)
  (make-point (region-left-leaf region) (region-left-offset region)))

(defun region-right-type (region)
  (int-to-mark-type (ldb (byte 1 0) (region-%right-idx region))))

(defun (setf region-right-type) (new region)
  (setf (ldb (byte 1 0) (region-%right-idx region)) (mark-type-to-int new)))

(defun region-right-idx (region)
  (ash (region-%right-idx region) -1))

(defun (setf region-right-idx) (new region)
  (setf (region-%right-idx region)
        (logior (ash new 1)
                (ldb (byte 1 0) (region-%right-idx region))))
  new)

(defun region-right-offset (region)
  (ldb (byte leaf-offset-bits 0) (region-right-idx region)))

(defun (setf region-right-offset) (new region)
  (setf (ldb (byte leaf-offset-bits 0) (region-right-idx region)) new))

(defun region-update-right-idx (region)
  (setf (region-right-idx region) (fpoint-idx (region-right-leaf region) (region-right-offset region) (region-node region))))

(defun region-fright (region)
  (values (region-right-leaf region) (region-right-offset region)))

(defun region-right (region)
  (make-point (region-right-leaf region) (region-right-offset region)))

(defun fpoint-shared-ancestor (l1 o1 l2 o2)
  (if (eq l1 l2)
      (values l1 o1 o2)
      (loop with n1 = l1 and n2 = l2
            with p1 = (leaf-parent n1) and p2 = (leaf-parent n2)
            for i1 = (branch-tag-idx p1 n1 o1) then (branch-tag-idx p1 n1 i1)
            and i2 = (branch-tag-idx p2 n2 o2) then (branch-tag-idx p2 n2 i2)
            until (eq p1 p2)
            do (if (<= (text-node-height p1) (text-node-height p2))
                   (setf n1 p1
                         p1 (branch-parent n1))
                   (setf n2 p2
                         p2 (branch-parent n2)))
            finally (return (values p1 i1 i2)))))

(defun %make-region (&key node data left-type left-leaf left-idx right-type right-leaf right-idx)
  (%%make-region :node node :data data
                 :left-leaf left-leaf :%left-idx (logior (ash left-idx 1) (mark-type-to-int left-type))
                 :right-leaf right-leaf :%right-idx (logior (ash right-idx 1) (mark-type-to-int right-type))))

(defun fmake-region (left-type l1 o1 right-type l2 o2 &optional data)
  (assert (eql (leaf-text l1) (leaf-text l2)))
  (if (fpoint= l1 o1 l2 o2)
      (assert (not (and (eql :left-inserting left-type) (eql :right-inserting right-type))))
      (assert (fpoint< l1 o1 l2 o2)))
  (setf (values l1 o1) (fpoint-bias-mark-type left-type l1 o1)
        (values l2 o2) (fpoint-bias-mark-type right-type l2 o2))
  (multiple-value-bind (node i1 i2) (fpoint-shared-ancestor l1 o1 l2 o2)
    (let ((ret (%make-region :node node :data data
                             :left-type left-type :left-leaf l1 :left-idx i1
                             :right-type right-type :right-leaf l2 :right-idx i2)))
      (push ret (text-node-regions node))
      ret)))

(defun make-region (left-type left right-type right &optional data)
  (fmake-region left-type (point-leaf left) (point-offset left) right-type (point-leaf right) (point-offset right) data))

(defun region-adjust (region &optional (old-node (region-node region)) cons)
  (multiple-value-bind (l1 o1) (region-fleft region)
    (multiple-value-bind (l2 o2) (region-fright region)
      (when (and (fpoint=* l1 o1 l2 o2)
                 (eql :left-inserting (region-left-type region))
                 (eql :right-inserting (region-right-type region)))
        (setf (region-right-type region) :left-inserting))
      (setf (values l1 o1) (fpoint-bias-mark-type* (region-left-type region) l1 o1)
            (values l2 o2) (fpoint-bias-mark-type* (region-right-type region) l2 o2))
      (multiple-value-bind (node i1 i2) (fpoint-shared-ancestor l1 o1 l2 o2)
        (setf (region-left-leaf region) l1
              (region-left-idx region) i1
              (region-right-leaf region) l2
              (region-right-idx region) i2)
        (when (or cons (not (eq old-node node)))
          (setf (region-node region) node)
          (setf (text-node-regions node)
                (rplacd (or cons (delete-region-from region old-node))
                        (text-node-regions node))))))))

(defun delete-region-from (region node)
  (let ((regions (text-node-regions node)))
    (if (eq region (car regions))
        (progn (setf (text-node-regions node) (cdr regions)) regions)
        (loop for cons on regions
              do (when (eq region (cadr cons))
                   (return (prog1 (cdr cons)
                             (setf (cdr cons) (cddr cons)))))))))

(defun delete-region (region)
  (delete-region-from region (region-node region))
  nil)

(defun copy-region (region)
  (let ((ret (%copy-region region)))
    (push ret (text-node-regions (region-node ret)))
    ret))

(defun %do-regions-at (point callback)
  (assert (= (point-offset point) (ldb (byte leaf-offset-bits 0) (point-offset point))))
  (loop with idx = (point-offset point)
        for n = (point-leaf point) then (text-node-parent n)
        while n
        do (dolist (region (text-node-regions n))
             (when (or (< (region-left-idx region) idx (region-right-idx region))
                       (and (= (region-left-idx region) idx)
                            (eql :right-inserting (region-left-type region)))
                       (and (= (region-right-idx region) idx)
                            (eql :left-inserting (region-right-type region))))
               (funcall callback region)))
           (when (text-node-parent n)
             (setf idx (branch-tag-idx (text-node-parent n) n idx)))))

(defmacro do-regions-at ((var point) &body body)
  `(%do-regions-at ,point (lambda (,var) ,@body)))

(defun typed-idx< (type i1 i2)
  (or (< i1 i2)
      (and (= i1 i2)
           (eql :right-inserting type))))

(defun typed-idx> (type i1 i2)
  (or (> i1 i2)
      (and (= i1 i2)
           (eql :left-inserting type))))

(defun %do-regions-between (left right callback)
  (when (point= left right) (return-from %do-regions-between))
  (with-point-biased-right (l1 o1) left
    (with-point-biased-left (l2 o2) right
      (multiple-value-bind (node i1 i2) (fpoint-shared-ancestor l1 o1 l2 o2)
        (labels ((aux-wedge-internal (node)
                   (loop for region in (text-node-regions node)
                         do (funcall callback region))
                   (when (branch-p node)
                     (aux-wedge-internal (branch-left node))
                     (aux-wedge-internal (branch-right node))))
                 (aux-wedge (node partial-idx)
                   (let* ((idx-size (text-node-idx-size node))
                          (pi1 (dpb 0 (byte idx-size 0) i1))
                          (pi2 (dpb 0 (byte idx-size 0) i2)))
                     (cond ((or (< partial-idx pi1)
                                (< pi2 partial-idx))
                            nil)
                           ((< pi1 partial-idx pi2)
                            (aux-wedge-internal node))
                           ((or (= partial-idx pi1)
                                (= partial-idx pi2))
                            (loop for region in (text-node-regions node)
                                  do (unless (or (typed-idx<
                                                  (region-right-type region)
                                                  (dpb (region-right-idx region) (byte idx-size 0) partial-idx) i1)
                                                 (typed-idx>
                                                  (region-left-type region)
                                                  (dpb (region-left-idx region) (byte idx-size 0) partial-idx) i2))
                                       (funcall callback region)))
                            (when (branch-p node)
                              (aux-wedge (branch-left node) (branch-tag-idx node (branch-left node) partial-idx))
                              (aux-wedge (branch-right node) (branch-tag-idx node (branch-right node) partial-idx))))))))
          (aux-wedge node 0)
          (loop with i1 = i1 and i2 = i2
                for n = node then p
                for p = (text-node-parent node) then (text-node-parent p)
                while p
                do (setf i1 (branch-tag-idx p n i1)
                         i2 (branch-tag-idx p n i2))
                   (loop for region in (branch-regions p)
                         do (unless (or (typed-idx< (region-right-type region) (region-right-idx region) i1)
                                        (typed-idx> (region-left-type region) (region-left-idx region) i2))
                              (funcall callback region)))))))))

(defmacro do-regions-between ((var left right) &body body)
  `(%do-regions-between ,left ,right (lambda (,var) ,@body)))

(defun %do-marks-at (point callback)
  (setf point (point-bias-right (copy-point point)))
  (let ((leaf (point-leaf point))
        (offset (point-offset point))) 
    (loop do (loop for mark in (leaf-marks leaf)
                   when (eql offset (mark-offset mark))
                     do (funcall callback mark))
          while (when (eql 0 offset)
                  (setf leaf (leaf-prev leaf)
                        offset (when leaf (leaf-length leaf)))
                  leaf))))

(defmacro do-marks-at ((var point) &body body)
  `(%do-marks-at ,point (lambda (,var) ,@body)))

(defmacro do-leaves ((var text) &body body)
  `(loop for ,var = (point-leaf (text-pos-point ,text 0)) then (leaf-next ,var)
         while ,var
         do (progn ,@body)))

(defun text-node-pos-point (text-node pos)
  (etypecase text-node
    (branch
     (let ((left-length (text-node-length (branch-left text-node))))
       (if (and (not (eql -1 pos)) (<= pos left-length))
           (text-node-pos-point (branch-left  text-node) pos)
           (text-node-pos-point (branch-right text-node) (if (eql -1 pos) -1 (- pos left-length))))))
    (leaf
     (if (eql -1 pos)
         (make-point text-node (leaf-length text-node))
         (progn
           (assert (<= pos (leaf-length text-node)) () "out of range")
           (make-point text-node pos))))))

(defun text-pos-point (text pos)
  (text-node-pos-point (text-root text) pos))

(defun sign-extend (int size)
  (sb-disassem:sign-extend int size))

(deftype coord () '(unsigned-byte 32))

(defun coord-x (coord)
  (sign-extend (logand coord #xffff) 16))

(defun coord-y (coord)
  (sign-extend (ash coord -16) 16))

(defun print-coord (coord)
  (format t "~%(~d, ~d) " (coord-x coord) (coord-y coord))
  coord)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun coord (x y)
    (assert (<= (- (expt 2 16)) x (1- (expt 2 16))) () "x coordinate too large")
    (assert (<= (- (expt 2 16)) y (1- (expt 2 16))) () "y coordinate too large")
    (the coord (logior (ash (logand y #xffff) 16) (logand x #xffff)))))

(defun coord= (c1 c2) (= c1 c2))
(defun coord+ (c1 c2) (coord (+ (coord-x c1) (coord-x c2))
                             (+ (coord-y c1) (coord-y c2))))
(defun coord- (c1 c2) (coord (- (coord-x c1) (coord-x c2))
                             (- (coord-y c1) (coord-y c2))))

(defconstant coord0 (coord 0 0))

(macrolet ((def (name cmp gt eq)
             `(progn
                (declaim (inline ,name))
                (defun ,name (x y)
                  (if (< y 0)
                      (if (< x 0)
                          ,eq
                          ,(not gt))
                      (if (< x 0)
                          ,gt
                          (,cmp x y)))))))
  (def neginf<  <  nil nil)
  (def neginf>  >  t   nil)
  (def neginf<= <= nil t)
  (def neginf>= >= t   t))

(declaim (inline neginf= neginf+ neginf-))
(defun neginf= (x y)
  (or (and (< x 0) (< y 0))
      (= x y)))

(defun neginf+ (x y)
  (if (or (< x 0) (< y 0))
      -1
      (+ x y)))

(defun neginf- (x y)
  (assert (neginf>= x y) () "invalid NEGINF-")
  (if (< x 0)
      (if (< y 0) 0 (- x y))
      (- x y)))

(macrolet ((def (name cmp)
             `(defun ,name (c1 c2)
                (if (neginf= (coord-y c1) (coord-y c2))
                    (,cmp (coord-x c1) (coord-x c2))
                    (,cmp (coord-y c1) (coord-y c2))))))
  (def lcoord<  neginf<)
  (def lcoord>  neginf>)
  (def lcoord<= neginf<=)
  (def lcoord>= neginf>=))

(defun lcoord- (c1 c2)
  (assert (lcoord>= c1 c2) () "invalid LCOORD-")
  (if (neginf= (coord-y c1) (coord-y c2))
      (coord (neginf- (coord-x c1) (coord-x c2)) 0)
      (coord (coord-x c1) (neginf- (coord-y c1) (coord-y c2)))))

(defun lcoord+ (c1 c2)
  (if (= 0 (coord-y c2))
      (coord (neginf+ (coord-x c1) (coord-x c2)) (coord-y c1))
      (coord (coord-x c2) (neginf+ (coord-y c1) (coord-y c2)))))

(defun text-node-coord-point (text-node coord)
  (etypecase text-node
    (branch
     (let ((left-coord (coord (text-node-col (branch-left text-node))
                              (text-node-newlines (branch-left text-node)))))
       (if (lcoord< coord left-coord)
           (text-node-coord-point (branch-left  text-node) coord)
           (text-node-coord-point (branch-right text-node) (lcoord- coord left-coord)))))
    (leaf
     (let ((ch-coord coord0)
           (i 0))
       (make-point text-node
                   (do-leaf-chars ((ch j) text-node i)
                     (when (<= 0 j)
                       (let ((next-coord
                               (lcoord+ ch-coord
                                        (if (char= #\Newline ch)
                                            (coord 0 1)
                                            (coord 1 0)))))
                         (when (lcoord< coord next-coord)
                           (return i))
                         (setf ch-coord next-coord)
                         (incf i)))))))))

(defun point-coord (point)
  (let ((leaf (point-leaf point))
        (offset (point-offset point))
        (ret coord0))
    (loop for parent = (leaf-parent leaf) then (branch-parent parent)
          and text-node = leaf then parent
          until (null parent)
          unless (eq text-node (branch-left parent))
            do (setf ret (lcoord+
                          (coord (text-node-col (branch-left parent))
                                 (text-node-newlines (branch-left parent)))
                          ret)))
    (let ((i 0))
      (do-leaf-chars ((ch j) leaf)
        (when (<= 0 j)
          (unless (< i offset) (return))
          (setf ret (lcoord+ ret (if (char= #\Newline ch)
                                     (coord 0 1)
                                     (coord 1 0))))
          (incf i))))
    ret))

(defun text-node-vcoord-point (text-node coord &optional (offset 0)
                               &aux (tabsize (text-tabsize (text-node-text text-node))))
  (when (neginf< (coord-x coord) offset)
    (setf coord (coord offset (coord-y coord))))
  (etypecase text-node
    (branch
     (let* ((left-branch (branch-left text-node))
            (left-coord (coord (tadd offset (text-node-tjoin left-branch) (text-node-vcol left-branch) tabsize)
                               (text-node-newlines left-branch))))
       (if (lcoord< coord left-coord)
           (text-node-vcoord-point (branch-left  text-node) coord offset)
           (text-node-vcoord-point (branch-right text-node)
                                   (coord (if (neginf= (coord-y coord) (coord-y left-coord))
                                              (prog1 (coord-x coord)
                                                (setf offset (coord-x left-coord)))
                                              (coord-x coord))
                                          (neginf- (coord-y coord) (coord-y left-coord)))
                                   offset))))
    (leaf
     (let ((ch-coord (coord offset 0))
           (i 0))
       (make-point text-node
                   (do-leaf-chars ((ch j) text-node i)
                     (let ((next-coord
                             (lcoord+ ch-coord
                                      (if (char= #\Newline ch)
                                          (coord offset 1)
                                          (coord (true-char-width ch (coord-x ch-coord) tabsize) 0)))))
                       (when (lcoord< coord next-coord)
                         (return i))
                       (setf ch-coord next-coord)
                       (when (<= 0 j) (setf i (1+ j))))))))))

(defun point-vcoord (point)
  (let* ((leaf (point-leaf point))
         (leaf-offset (point-offset point))
         (tabsize (text-tabsize (leaf-text leaf)))
         (ret coord0)
         (tjoin nil))
    (loop for parent = (leaf-parent leaf) then (branch-parent parent)
          and text-node = leaf then parent
          until (null parent)
          unless (eq text-node (branch-left parent))
            do (setf (values ret tjoin)
                     (vcoord-add (coord (text-node-vcol (branch-left parent))
                                        (text-node-newlines (branch-left parent)))
                                 (text-node-tjoin (branch-left parent))
                                 ret
                                 tjoin
                                 tabsize)))
    (let ((tabsize (text-tabsize (leaf-text leaf))))
      (do-leaf-chars ((ch i) leaf)
        (unless (< i leaf-offset) (return))
        (setf ret (lcoord+ ret (if (char= #\Newline ch)
                                   (coord 0 1)
                                   (coord (true-char-width ch (coord-x ret) tabsize) 0))))))
    ret))

(defun text-coord-point (text coord)
  (text-node-coord-point (text-root text) coord))

(defun text-vcoord-point (text coord)
  (text-node-vcoord-point (text-root text) coord))

(defun point-line (point)
  (coord-y (point-coord point)))

(defun fpoint-insert-no-split (leaf offset string)
  (assert (<= (+ (leaf-length leaf) (length string)) max-string-size))
  (loop for i from (1- (+ (leaf-entry-offset (leaf-length leaf))
                          (if (leaf-next leaf) 0 (1- leaf-entry-size))))
        downto (leaf-entry-offset offset) 
        do (shiftf (svref leaf (+ i (* (length string) leaf-entry-size))) (svref leaf i) 0))
  (incf (leaf-length leaf) (length string))
  (loop for ch across string and i from offset
        do (setf (rpoint-char leaf i) ch)
           (init-states leaf i))
  (loop for mark in (leaf-marks leaf)
        do (when (typed-idx> (mark-type mark) (mark-offset mark) offset)
             (incf (mark-offset mark) (length string))))
  (loop for n = leaf then (text-node-parent n)
        while n
        do (loop for region in (text-node-regions n)
                 do (when (and (eq leaf (region-left-leaf region))
                               (typed-idx> (region-left-type region) (region-left-offset region) offset))
                      (incf (region-left-offset region) (length string)))
                    (when (and (eq leaf (region-right-leaf region))
                               (typed-idx> (region-right-type region) (region-right-offset region) offset))
                      (incf (region-right-offset region) (length string)))))
  (text-node-update-recurse leaf))

(defun update-regions (regions)
  (loop for cons = regions then next
        for next = (cdr cons)
        while cons
        for region = (car cons)
        do (region-adjust region (region-node region) cons)))

(defun fpoint-split-leaf (leaf offset new-length)
  "Splits the given leaf into a branch and leaf-leaf pair.  The tree is
structurally updated, but the caller is expected to (1) insert the new
text into the returned leaf, (2) remove the text after OFFSET in
LEAF, (3) update the length of LEAF, and (4) update summaries of all
parents of the returned leaf."
  (assert (<= (- (leaf-length leaf) offset) new-length))
  (assert (< 0 new-length))
  (let* ((next-leaf (make-leaf :text (leaf-text leaf)
                               :prev leaf
                               :next (leaf-next leaf)
                               :length new-length))
         (branch (make-branch :text (leaf-text leaf)
                              :height 1
                              :left leaf
                              :right next-leaf))
         (length-diff (- new-length (leaf-length leaf))))
    (when (leaf-next leaf)
      (setf (leaf-prev (leaf-next leaf)) next-leaf))
    (switch-in-parent branch leaf (leaf-parent leaf))
    (setf (leaf-parent next-leaf) branch
          (leaf-parent leaf) branch
          (leaf-next leaf) next-leaf)
    (loop with left-marks = nil and right-marks = nil
          for cons = (leaf-marks leaf) then next for next = (cdr cons)
          while cons
          for mark = (car cons)
          do (if (typed-idx< (mark-type mark) (mark-offset mark) offset)
                 (setf left-marks (rplacd cons left-marks))
                 (setf right-marks (rplacd cons right-marks)))
          finally (setf (leaf-marks leaf) left-marks
                        (leaf-marks next-leaf) right-marks)
                  (loop for mark in right-marks
                        do (setf (mark-leaf mark) next-leaf)
                           (incf (mark-offset mark) length-diff)))
    (loop with update-regions = nil
          for node = leaf then (text-node-parent node)
          while node
          do (loop with no-update-regions = nil
                   for cons = (shiftf (text-node-regions node) nil) then next
                   for next = (cdr cons)
                   while cons
                   for region = (car cons)
                   do (let ((update nil))
                        (when (eq leaf (region-left-leaf region))
                          (unless (typed-idx< (region-left-type region) (region-left-offset region) offset)
                            (setf (region-left-leaf region) next-leaf)
                            (incf (region-left-offset region) length-diff))
                          (setf update t))
                        (when (eq leaf (region-right-leaf region))
                          (unless (typed-idx< (region-right-type region) (region-right-offset region) offset)
                            (setf (region-right-leaf region) next-leaf)
                            (incf (region-right-offset region) length-diff))
                          (setf update t))
                        (if update
                            (setf update-regions (rplacd cons update-regions))
                            (setf no-update-regions (rplacd cons no-update-regions))))
                   finally (setf (text-node-regions node) no-update-regions))
          finally (text-node-structural-update-recurse branch)
                  (update-regions update-regions))
    next-leaf))

(defun point-insert (point x)
  (point-invalidate point)
  (with-point-biased-left (leaf offset) point
    (let ((string (etypecase x
                    (character (string x))
                    (string x))))
      (when (string= "" string)
        (return-from point-insert nil))
      (if (<= (+ (leaf-length leaf) (length string)) max-string-size)
          (fpoint-insert-no-split leaf offset string)
          (let* ((string-first-chunk-size (min (- max-string-size offset) (length string)))
                 ;; ^^ Part of the string which goes in LEAF
                 (string-end-chunk-size (if (= 0 (- (length string) string-first-chunk-size))
                                            0
                                            (1+ (mod (1- (- (length string) string-first-chunk-size))
                                                     max-string-size))))
                 ;; ^^ Part of the string which goes in NEXT-LEAF
                 (next-leaf-length (let ((it (+ string-end-chunk-size (- (leaf-length leaf) offset))))
                                     (if (< max-string-size it)
                                         (progn
                                           (setf string-end-chunk-size 0)
                                           (- (leaf-length leaf) offset))
                                         it)))
                 (next-leaf-offset (+ offset (- next-leaf-length (leaf-length leaf))))
                 (next-leaf (fpoint-split-leaf leaf offset next-leaf-length)))
            ;; Split at offset
            ;; New leaf has offset to leaf-length (plus end-chunk if there is room)
            (loop for i from (leaf-entry-offset offset) below (+ (leaf-entry-offset (leaf-length leaf))
                                                                 (if (leaf-next next-leaf) 0 (1- leaf-entry-size)))
                  for j from (leaf-entry-offset next-leaf-offset)
                  do (shiftf (svref next-leaf j) (svref leaf i) 0))
            (loop for i below string-end-chunk-size
                  for j from (- (length string) string-end-chunk-size)
                  do (setf (rpoint-char next-leaf i) (aref string j))
                     (init-states next-leaf i))
            (loop for j below string-first-chunk-size
                  for i from offset
                  do (setf (rpoint-char leaf i) (aref string j))
                     (init-states leaf i))
            (setf (leaf-length leaf) (+ offset string-first-chunk-size))
            (text-node-update-recurse leaf)
            (branch-rebalance-recurse (leaf-parent leaf))
            (loop for start = string-first-chunk-size then end
                  for end = (min (+ start max-string-size) (- (length string) string-end-chunk-size))
                  while (< start end)
                  do (let ((next-leaf (fpoint-split-leaf leaf (leaf-length leaf) (- end start))))
                       (loop for i below (- end start)
                             do (setf (rpoint-char next-leaf i) (aref string (+ start i)))
                                (init-states next-leaf i))
                       (text-node-update-recurse next-leaf)
                       (branch-rebalance-recurse (leaf-parent next-leaf))
                       (setf leaf next-leaf))))))))

(defun check-empty-states (leaf i)
  (assert (eql nil (dll-list (rpoint-reductions leaf i))))
  (assert (or (eql nil (rpoint-trace leaf i))
              (equal (list nil) (rpoint-trace leaf i))))
  (assert (eql nil (rpoint-susps leaf i))))

(defun delete-inside-leaf (leaf offset &optional (n 1))
  (assert (<= (+ n offset) (leaf-length leaf)))
  (loop for i from offset repeat n
        do (check-empty-states leaf offset))
  (loop for i from (leaf-entry-offset offset) below (leaf-entry-offset (+ offset n))
        do (setf (svref leaf i) 0))
  (loop for i from (leaf-entry-offset offset)
        for j from (leaf-entry-offset (+ offset n))
          below (+ (leaf-entry-offset (leaf-length leaf))
                   (if (leaf-next leaf) 0 (1- leaf-entry-size)))
        do (shiftf (svref leaf i) (svref leaf j) 0))
  (decf (leaf-length leaf) n)
  (loop for cons = (shiftf (leaf-marks leaf) nil) then next for next = (cdr cons)
        while cons
        for mark = (car cons)
        do (when (< offset (mark-offset mark))
             (setf (mark-offset mark) (max offset (- (mark-offset mark) n))))
           (mark-adjust mark (mark-leaf mark) cons))
  (loop with update-regions = nil
        for node = leaf then (text-node-parent node)
        while node
        do (loop for region in (text-node-regions node)
                 do (assert (eq node (region-node region))))
           (loop with no-update-regions = nil
                 for cons = (shiftf (text-node-regions node) nil) then next for next = (cdr cons)
                 while cons
                 for region = (car cons)
                 do (let ((update nil))
                      (when (eq leaf (region-left-leaf region))
                        (unless (typed-idx< (region-left-type region) (region-left-offset region) offset)
                          (setf (region-left-offset region) (max offset (- (region-left-offset region) n))))
                        (setf update t))
                      (when (eq leaf (region-right-leaf region))
                        (unless (typed-idx< (region-right-type region) (region-right-offset region) offset)
                          (setf (region-right-offset region) (max offset (- (region-right-offset region) n))))
                        (setf update t))
                      (if update
                          (setf update-regions (rplacd cons update-regions))
                          (setf no-update-regions (rplacd cons no-update-regions))))
                 finally (setf (text-node-regions node) no-update-regions))
        finally (update-regions update-regions))
  (text-node-update-recurse leaf))

;; TODO: add coalescing

(defun delete-leaf (leaf)
  (assert (leaf-parent leaf))
  (loop for i below (leaf-length leaf)
        do (check-empty-states leaf i))
  (let* ((prev (leaf-prev leaf))
         (next (leaf-next leaf))
         (parent (leaf-parent leaf))
         (grandparent (branch-parent parent))
         (sibling (if (eq (branch-left parent) leaf)
                      (branch-right parent)
                      (branch-left  parent))))
    (delete-inside-leaf leaf 0 (leaf-length leaf))
    (when (null (leaf-next leaf))
      ;; prev is not null due to (leaf-parent leaf) requirement
      (loop for i from (leaf-entry-offset (leaf-length leaf)) repeat (1- leaf-entry-size)
            for j from (leaf-entry-offset (leaf-length prev))
            do (setf (svref prev j) (svref leaf i))))
    (when prev (setf (leaf-next prev) next))
    (when next (setf (leaf-prev next) prev))
    (assert (null (branch-regions parent)))
    (switch-in-parent sibling parent grandparent)
    (text-node-structural-update-recurse sibling)
    (text-node-update-recurse sibling)
    (branch-rebalance-recurse grandparent)))

(defun %point-delete (point &optional (n 1))
  (assert (<= 0 n))
  (with-point-biased-right (leaf offset) point
    (when (< 0 offset (leaf-length leaf))
      (let ((m (min n (- (leaf-length leaf) offset))))
        (decf n m)
        (delete-inside-leaf leaf offset m)
        (setf leaf (leaf-next leaf))))
    (loop while (and leaf (< 0 n))
          do (if (and (<= (leaf-length leaf) n) (leaf-parent leaf))
                 (let ((next-leaf (leaf-next leaf)))
                   (decf n (leaf-length leaf))
                   (delete-leaf leaf)
                   (setf leaf next-leaf))
                 (progn
                   (delete-inside-leaf leaf 0 n)
                   (return))))))

(defun point-delete (point &optional (n 1))
  (point-invalidate-between point (text-pos-point (point-text point) (+ (point-pos point) n)))
  (%point-delete point n))

(defun point-delete-between (p1 p2)
  (point-invalidate-between p1 p2)
  (%point-delete p1 (- (point-pos p2) (point-pos p1))))

(defun point-replace (point string)
  (setf point (copy-point point))
  (let ((leaves (make-hash-table)))
    (loop for ch across string and i from 0 while (point-peek point)
          do (assert (not (eql 0 (point-char point))))
             (point-bias-right point)
             (setf (gethash (point-leaf point) leaves) t)
             (setf (point-char point) ch)
             (point-skip point))
    (loop for leaf being the hash-keys of leaves
          do (text-node-update-recurse leaf)))
  point)

(defun text-substring (text start &optional (end nil end-p))
  (when end-p
    (decf end start))
  (let ((point (text-pos-point text start))
        (ret (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for i from 0
          and ch = (point-next point)
          while ch
          while (or (not end-p) (< i end))
          do (vector-push-extend ch ret))
    ret))

(defun text-string (text)
  (text-substring text 0))

(defun text-length (text)
  (text-node-length (text-root text)))

(defun text-end-coord (text)
  (coord (text-node-col (text-root text))
         (text-node-newlines (text-root text))))

(defun string-text (string)
  (let ((ret (make-text)))
    (point-insert (text-pos-point ret 0) string)
    ret))

(defun fpoint-pos (leaf offset)
  (+ (loop for parent = (leaf-parent leaf) then (branch-parent parent)
           and text-node = leaf then parent
           until (null parent)
           unless (eq text-node (branch-left parent))
             sum (text-node-length (branch-left parent)))
     offset))

(defun point-pos (point)
  (fpoint-pos (point-leaf point) (point-offset point)))

(defun point-text (point)
  (leaf-text (point-leaf point)))

(defun mark-text (mark)
  (leaf-text (mark-leaf mark)))

(defvar *region-map* nil)

(defun check-typed-point (type leaf offset)
  (assert (and (<= (if (and (leaf-prev leaf) (eql type :right-inserting)) 1 0) offset)
               (<= offset (- (leaf-length leaf)
                             (if (and (leaf-next leaf) (eql type :left-inserting)) 1 0))))
          () "~s mark is at offset ~s" type offset))

(defun check-mark (mark node)
  (assert (eq node (mark-leaf mark)))
  (check-typed-point (mark-type mark) (mark-leaf mark) (mark-offset mark)))

(defun check-region (region node)
  (assert (eq node (region-node region)))
  (multiple-value-bind (l1 o1) (region-fleft region)
    (multiple-value-bind (l2 o2) (region-fright region)
      (assert (or (fpoint< l1 o1 l2 o2)
                  (and (fpoint= l1 o1 l2 o2)
                       (not (and (eql :left-inserting (region-left-type region))
                                 (eql :right-inserting (region-right-type region)))))))
      (when *region-map*
        (destructuring-bind (m1 m2) (gethash region *region-map*)
          (assert (fpoint= l1 o1 (mark-leaf m1) (mark-offset m1)))
          (assert (fpoint= l2 o2 (mark-leaf m2) (mark-offset m2)))))
      (multiple-value-bind (n i1 i2) (fpoint-shared-ancestor l1 o1 l2 o2)
        (assert (eq n (region-node region)))
        (assert (eql i1 (region-left-idx region)))
        (assert (eql i2 (region-right-idx region))))
      (check-typed-point (region-left-type region) l1 o1)
      (check-typed-point (region-right-type region) l2 o2))))

(defun check-text-node (node parent text)
  (assert (eq parent (text-node-parent node)))
  (labels ()
    (etypecase node
      (branch
       (let ((copy (copy-branch node)))
         (merge-summaries copy (branch-left node) (branch-right node))
         (assert (summary= copy node)))
       (loop for region in (branch-regions node)
             do (check-region region node))
       (multiple-value-bind (nr1 nm1) (check-text-node (branch-left node) node text)
         (multiple-value-bind (nr2 nm2) (check-text-node (branch-right node) node text)
           (values (+ nr1 nr2 (length (branch-regions node))) (+ nm1 nm2)))))
      (leaf
       (when (leaf-prev node)
         (assert (eq node (leaf-next (leaf-prev node)))))
       (when (leaf-next node)
         (assert (eq node (leaf-prev (leaf-next node)))))
       (assert (or (null (leaf-parent node)) (< 0 (leaf-length node))))
       (loop for mark in (leaf-marks node)
             do (check-mark mark node))
       (loop for region in (leaf-regions node)
             do (check-region region node))
       (loop with partition = (+ (leaf-entry-offset (leaf-length node))
                                 (if (leaf-next node) 0 (1- leaf-entry-size)))
             for i from (leaf-entry-offset 0)
               below (length node)
             do (if (< i partition)
                    (assert (not (eql (svref node i) 0)))
                    (assert (eql (svref node i) 0))))
       (values (length (leaf-regions node)) (length (leaf-marks node)))))))

(defun check-text (text n-regions n-marks)
  (multiple-value-bind (nr nm) (check-text-node (text-root text) nil text)
    (assert (= n-regions nr))
    (assert (= n-marks nm))))

(defvar *insert-replay* nil)
(defvar *delete-replay* nil)

(defparameter *test-chunk-size* 50)

(defun test-text-insert (&key (size 10) replay)
  (setf *insert-replay*
        (if replay
            *insert-replay*
            (loop for i below size
                  collect (nconc
                           (sort (list (random (1+ i))
                                       (random (1+ i)))
                                 #'<)
                           (list (random (1+ i))
                                 (code-char (+ #.(char-code #\a) (random 26))))))))
  (let ((text (make-text))
        (*region-map* (or *region-map* (make-hash-table))))
    (loop for (left-pos right-pos pos char) in *insert-replay*
          for i from 0
          do (setf left-pos (* left-pos *test-chunk-size*)
                   right-pos (* right-pos *test-chunk-size*)
                   pos (* pos *test-chunk-size*))
             (check-text text i (* i 2))
             ;; (format t "~%insert[~d]: ~c@~d" i char pos)
             (let ((left (make-mark-left-inserting (text-pos-point text left-pos)))
                   (right (if (= left-pos right-pos)
                              (make-mark-left-inserting (text-pos-point text right-pos))
                              (make-mark-right-inserting (text-pos-point text right-pos)))))
               (setf (gethash (make-region :left-inserting left
                                           (if (= left-pos right-pos) :left-inserting :right-inserting) right)
                              *region-map*)
                     (list left right))
               (point-insert (text-pos-point text pos) (make-string *test-chunk-size* :initial-element char)))
          finally (check-text text (1+ i) (* (1+ i) 2))
                  (return (values text (1+ i))))))

(defun test-text (&key (size 10) replay)
  (setf *delete-replay* (if replay
                            *delete-replay*
                            (loop for i from size downto 1
                                  collect (random i))))
  (let ((*region-map* (make-hash-table)))
    (multiple-value-bind (text n-regions) (test-text-insert :size size :replay replay)
      (loop for rand in *delete-replay* for i from 0
            do (setf rand (* rand *test-chunk-size*))
               ;; (format t "~%delete[~d]: ~d" i rand)
               (point-delete (text-pos-point text rand) *test-chunk-size*)
               (check-text text n-regions (* n-regions 2)))
      (text-string text))))

