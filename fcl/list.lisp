(defun %rplaca (cons x)
  (rplaca cons x)
  x)

(defun %rplacd (cons x)
  (rplacd cons x)
  x)

(defsetf car %rplaca)
(defsetf cdr %rplacd)

(macrolet ((define-getter (name first rest)
             `(define-setf-expander ,name (list)
                (let ((list-sym (make-symbol "LIST"))
                      (new (make-symbol "NEW")))
                  (values (list list-sym) (list (list ',rest list)) (list new)
                          (list ',(ecase first (car '%rplaca) (cdr '%rplacd)) list-sym new)
                          (list ',first list-sym))))))
  (define-getter caar car car)
  (define-getter cadr car cdr)
  (define-getter cdar cdr car)
  (define-getter cddr cdr cdr)
  (define-getter caaar car caar)
  (define-getter caadr car cadr)
  (define-getter cadar car cdar)
  (define-getter caddr car cddr)
  (define-getter cdaar cdr caar)
  (define-getter cdadr cdr cadr)
  (define-getter cddar cdr cdar)
  (define-getter cdddr cdr cddr)
  (define-getter caaaar car caaar)
  (define-getter caaadr car caadr)
  (define-getter caadar car cadar)
  (define-getter caaddr car caddr)
  (define-getter cadaar car cdaar)
  (define-getter cadadr car cdadr)
  (define-getter caddar car cddar)
  (define-getter cadddr car cdddr)
  (define-getter cdaaar cdr caaar)
  (define-getter cdaadr cdr caadr)
  (define-getter cdadar cdr cadar)
  (define-getter cdaddr cdr caddr)
  (define-getter cddaar cdr cdaar)
  (define-getter cddadr cdr cdadr)
  (define-getter cdddar cdr cddar)
  (define-getter cddddr cdr cdddr))

(defun first (list)
  (car list))
(defun second (list)
  (cadr list))
(defun third (list)
  (caddr list))
(defun fourth (list)
  (cadddr list))
(defun fifth (list)
  (car (cddddr list)))
(defun sixth (list)
  (cadr (cddddr list)))
(defun seventh (list)
  (caddr (cddddr list)))
(defun eighth (list)
  (cadddr (cddddr list)))
(defun ninth (list)
  (car (cddddr (cddddr list))))
(defun tenth (list)
  (cadr (cddddr (cddddr list))))

(macrolet ((define-getter (name form)
             `(define-setf-expander ,name (list)
                (let ((list-sym (make-symbol "LIST"))
                      (new (make-symbol "NEW")))
                  (values (list list-sym) (list ,form) (list new)
                          (list '%rplaca list-sym new)
                          (list ',name list-sym))))))
  (define-getter first list)
  (define-getter second (cdr list))
  (define-getter third (cddr list))
  (define-getter fourth (cdddr list))
  (define-getter fifth (cddddr list))
  (define-getter sixth (cdr (cddddr list)))
  (define-getter seventh (cddr (cddddr list)))
  (define-getter eighth (cdddr (cddddr list)))
  (define-getter ninth (cddddr (cddddr list)))
  (define-getter tenth (cdr (cddddr (cddddr list)))))

(defun rest (list)
  (cdr list))
(defsetf rest %rplacd)

(defun copy-tree (tree)
  (if (consp tree)
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
      tree))

(defmacro define-substitution (name args &key let (cond (error "condition required")) destructive predicate)
  
  `(defun ,name (,@args tree &key key ,@(unless predicate `((test #'eql test-p) (test-not nil test-not-p))))
     ,@(unless predicate
         `((when (and test-p test-not-p)
             (error "both TEST and TEST-NOT specified"))))
     (let ((key (and key (coerce-to-function key)))
           ,@(if predicate
                 `((predicate (coerce-to-function predicate)))
                 `((test (if test-p (coerce-to-function test) test))
                   (test-not (and test-not-p (coerce-to-function test-not))))))
       (labels ((aux (subtree)
                  (let* ((key (if key (funcall key subtree) subtree))
                         ,@let)
                    (cond
                      ,cond
                      ((atom subtree) subtree)
                      (t (let* ((car (car subtree))
                                (cdr (cdr subtree))
                                (aux-car (aux car))
                                (aux-cdr (aux cdr)))
                           ,(if destructive
                                `(progn
                                   (unless (eq car aux-car)
                                     (setf (car subtree) aux-car))
                                   (unless (eq cdr aux-cdr)
                                     (setf (cdr subtree) aux-cdr))
                                   subtree)
                                `(if (and (eq car aux-car)
                                          (eq cdr aux-cdr))
                                     subtree
                                     (cons aux-car aux-cdr)))))))))
         (aux tree)))))

(define-substitution sublis (alist)
  :let ((assoc (if test-not-p
                   (assoc key alist :test-not test-not)
                   (assoc key alist :test test))))
  :cond (assoc (cdr assoc)))

(define-substitution nsublis (alist)
  :let ((assoc (if test-not-p
                   (assoc key alist :test-not test-not)
                   (assoc key alist :test test))))
  :cond (assoc (cdr assoc))
  :destructive t)

(define-substitution subst (new old)
  :cond ((if test-not-p
            (funcall test-not key old)
            (funcall test key old))
         new))

(define-substitution nsubst (new old)
  :cond ((if test-not-p
            (funcall test-not key old)
            (funcall test key old))
         new)
  :destructive t)

(define-substitution subst-if (new predicate)
  :cond ((funcall predicate key) new)
  :predicate t)

(define-substitution subst-if-not (new predicate)
  :cond ((not (funcall predicate key)) new)
  :predicate t)

(define-substitution nsubst-if (new predicate)
  :cond ((funcall predicate key) new)
  :predicate t
  :destructive t)

(define-substitution nsubst-if-not (new predicate)
  :cond ((not (funcall predicate key)) new)
  :predicate t
  :destructive t)

(defun tree-equal (tree-1 tree-2 &key (test #'eql test-p) (test-not nil test-not-p))
  (when (and test-p test-not-p)
    (error "both TEST and TEST-NOT specified"))
  (let ((test (if test-p (coerce-to-function test) test))
        (test-not (and test-not (coerce-to-function test-not))))
    (labels ((aux (subtree-1 subtree-2)
               (cond
                 ((and (consp subtree-1) (consp subtree-2))
                  (and (aux (car subtree-1) (car subtree-2))
                       (aux (cdr subtree-1) (cdr subtree-2))))
                 ((and (atom subtree-1) (atom subtree-2))
                  (if test-not-p
                      (not (funcall test-not subtree-1 subtree-2))
                      (funcall test subtree-1 subtree-2)))
                 (t nil))))
      (aux tree-1 tree-2))))

(defun copy-list (list)
  (with-ext-lists ((new-list new-list-end))
    (loop for sublist on list
          do (push-tail (car sublist) new-list-end)
          finally (setf (cdr new-list-end) sublist))
    new-list))

(defun list-length (list)
  (loop for tortoise = list then (cdr tortoise)
        for hare = list then (cddr hare)
        for n from 0 by 2
        unless hare
          do (return n)
        unless (cdr hare)
          do (return (+ n 1))
        when (and (eq tortoise hare) (> n 0))
          do (return nil)))

(defun make-list (size &key initial-element)
  (loop for n below size
        collect initial-element))

(defun nth (n list)
  (car (nthcdr n list)))

(define-setf-expander nth (n list)
  (let ((list-sym (make-symbol "LIST"))
        (new (make-symbol "NEW")))
    (values (list list-sym) (list `(nthcdr ,n ,list)) (list new)
            `(%rplaca ,list-sym ,new)
            `(car ,list-sym))))

(defun nconc (&rest lists)
  (loop while (cdr lists)
        with start = (list nil)
        with last = start
        do (setf last (last (rplacd last (copy-list (pop lists)))))
        finally (rplacd last (car lists))
                (return (cdr start))))

;; ;; Don't override built-in append until after other functions are defined
;; (defun append (&rest lists)
;;   (loop while (cdr lists)
;;         with start = (list nil)
;;         with last = start
;;         do (setf last (last (rplacd last (copy-list (pop lists)))))
;;         finally (rplacd last (car lists))
;;                 (return (cdr start))))

(defun revappend (list tail)
  (loop for item in (cdr list)
        with ret = tail
        do (push item ret)
        finally (return ret)))

(defun nreconc (list tail)
  (loop with ret = tail
        while list
        do (psetf (cdr list) ret
                  ret list
                  list (cdr list))
        finally (return ret)))

(defun list-nreverse (list)
  (nreconc list nil))

(defun list-reverse (list)
  (revappend list nil))

(defun butlast (list &optional n)
  (if (= 0 n)
      (copy-list list)
      (let ((tail list))
        (loop for i below n
              while (consp tail)
              do (pop tail))
        (loop until (atom tail)
              collect (car list)
              do (pop tail)
                 (pop list)))))

(defun nbutlast (list &optional n)
  (if (= 0 n)
      (progn
        (unless (or (consp list) (null list))
          (error "LIST is not a valid list: ~s" list))
        list)
      (let ((head list)
            (tail list))
        (loop for i below n
              while (consp tail)
              do (pop tail))
        (if (atom tail)
            nil
            (loop do (pop tail)
                  when (atom tail)
                    do (rplacd list nil)
                       (return head)
                  do (pop list))))))

(defun last (list &optional (n 1))
  (do ((l list (cdr l))
       (r list)
       (i 0 (+ i 1)))
      ((atom l) r)
    (if (>= i n) (pop r))))

(defun tailp (object list)
  (do ((l list (cdr l)))
      ((atom l) (eql l object))
    (when (eql l object)
      (return t))))

(defun ldiff (list object)
  (with-ext-lists ((new new-tail))
    (do ((l list (cdr l)))
        ((atom l) (rplacd new-tail l) new)
      (when (eql l object)
        (return new))
      (push-tail (car l) new-tail))))

(defun nthcdr (n list)
  (loop for i below n
        do (pop list))
  list)

(defmacro check-final (tail list)
  `(unless (null ,tail)
     (error "LIST is not a proper list: ~s" ,list)))

(macrolet ((key-test-not-header ()
             `(progn
                (when (and test-p test-not-p)
                  (error "both TEST and TEST-NOT specified"))
                (setq key (and key (coerce-to-function key))
                      test (if test-p (coerce-to-function test) test)
                      test-not (and test-not (coerce-to-function test-not)))))
           (key-predicate-header ()
             `(setq key (and key (coerce-to-function key))
                    predicate (coerce-to-function predicate)))
           (maybe-funcall (name obj)
             `(if ,name (funcall ,name ,obj) ,obj))
           (test (item obj)
             `(if test-not-p
                  (not (funcall test-not ,item ,obj))
                  (funcall test ,item ,obj)))
           (member-test (elem list)
             (let ((obj-1 (make-symbol "OBJ-1"))
                   (obj-2 (make-symbol "OBJ-2")))
               `(let ((,obj-1 (maybe-funcall key ,elem)))
                  (loop for ,obj-2 in ,list
                        do (let ((,obj-2 (maybe-funcall key ,obj-2)))
                             (when (test ,obj-1 ,obj-2)
                               (return t))))))))
  
  (defun member (item list &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for tail on list
          do (let ((obj (maybe-funcall key (car tail))))
               (when (test item obj)
                 (return tail)))
          finally (check-final tail list)))

  (defun member-if (predicate list &key key)
    (key-predicate-header)
    (loop for tail on list
          do (let ((obj (maybe-funcall key (car tail))))
               (when (funcall predicate key)
                 (return tail)))
          finally (check-final tail list)))

  (defun member-if-not (predicate list &key key)
    (key-predicate-header)
    (loop for tail on list
          do (let ((obj (maybe-funcall key (car tail))))
               (when (not (funcall predicate key))
                 (return tail)))
          finally (check-final tail list)))

  (defun assoc (item alist &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for cons in alist
          when cons
            do (let ((obj (maybe-funcall key (car cons))))
                 (when (test item obj)
                   (return cons)))))

  (defun assoc-if (predicate alist &key key)
    (key-predicate-header)
    (loop for cons in alist
          when cons
            do (let ((obj (maybe-funcall key (car cons))))
                 (when (funcall predicate obj)
                   (return cons)))))

  (defun assoc-if-not (predicate alist &key key)
    (key-predicate-header)
    (loop for cons in alist
          when cons
            do (let ((obj (maybe-funcall key (car cons))))
                 (when (funcall predicate obj)
                   (return cons)))))

  (defun rassoc (item alist &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for cons in alist
          when cons
            do (let ((obj (maybe-funcall key (cdr cons))))
                 (when (test item obj)
                   (return cons)))))

  (defun rassoc-if (predicate alist &key key)
    (key-predicate-header)
    (loop for cons in alist
          when cons
            do (let ((obj (maybe-funcall key (cdr cons))))
                 (when (funcall predicate obj)
                   (return cons)))))

  (defun rassoc-if-not (predicate alist &key key)
    (key-predicate-header)
    (loop for cons in alist
          when cons
            do (let ((obj (maybe-funcall key (cdr cons))))
                 (when (funcall predicate obj)
                   (return cons)))))

  (defun intersection (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for elem-1 in list-1
          when (member-test elem-1 list-2)
            collect elem-1))

  (defun nintersection (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop with prev
          with tail = list-1
          while tail
          for elem-1 = (car tail)
          do (if (member-test elem-1 list-2)
                 (setf prev tail)
                 (if prev
                     (setf (cdr prev) (cdr tail))
                     (setf list-1 (cdr tail))))
             (pop tail)
          finally (check-final tail list-1)
                  (return tail-1)))

  (defun adjoin (item list &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for elem in list
          do (let ((obj (maybe-funcall key elem)))
               (when (test item obj)
                 (return list)))
          finally (return (cons item list))))

  (defun set-difference (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for elem-1 in list-1
          unless (member-test elem-1 list-2)
            collect elem-1))

  (defun nset-difference (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop with prev
          with tail = list-1
          while tail
          for elem-1 = (car tail)
          do (if (member-test elem-1 list-2)
                 (if prev
                     (setf (cdr prev) (cdr tail))
                     (setf list-1 (cdr tail)))
                 (setf prev tail))
             (pop tail)
          finally (check-final tail list-1)
                  (return tail-1)))

  (defun set-exclusive-or (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (with-ext-lists ((ret ret-end))
      (loop for elem-1 in list-1
            unless (member-test elem-1 list-2)
              do (push-tail elem-1 ret-end))
      (loop for elem-2 in list-2
            unless (member-test elem-2 list-1)
              do (push-tail elem-2 ret-end))
      ret))

  (defun nset-exclusive-or (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (with-ext-lists ((ret ret-end))
      (loop with tail = list-1
            while tail
            do (if (member-test (car tail) list-2)
                   (pop tail)
                   (let ((cons tail))
                     (pop tail)
                     (setf ret-end (%rplacd ret-end cons)))))
      (loop with tail = list-2
            while tail
            do (if (member-test (car tail) list-1)
                   (pop tail)
                   (let ((cons tail))
                     (pop tail)
                     (setf ret-end (%rplacd ret-end cons)))))
      (rplacd ret-end nil)
      ret))

  (defun subsetp (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for elem-1 in list-1
          unless (member-test elem-1 list-2)
            do (return nil)
          finally (return t)))

  (defun union (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop for elem-1 in list-1
          with ret = list-2
          unless (member-test elem-1 list-2)
            do (push elem-1 ret)
          finally (return ret)))

  (defun nunion (list-1 list-2 &key key (test #'eql test-p) (test-not nil test-not-p))
    (key-test-not-header)
    (loop with ret = list-2
          with tail = list-1
          while tail
          for elem-1 = (car tail)
          do (if (member-test elem-1 list-2)
                 (pop tail)
                 (let ((cons tail))
                   (pop tail)
                   (setf ret (rplacd cons ret))))
          finally (return ret))))

(labels ((pop-args (lists)
           (loop for l on lists
                 collect (let ((elem (car l)))
                           (when (null elem)
                             (return nil))
                           (pop (car l)))))
         (pop-lists (lists)
           (loop for l on lists
                 collect (let ((elem (car l)))
                           (when (null elem)
                             (return nil))
                           (rplaca l (cdr elem))
                           elem))))
  (defun mapc (function &rest lists)
    (when (null lists)
      (error "not enough arguments to MAPC"))
    (let ((function (coerce-to-function function)))
      (loop for args = (pop-args lists)
            while args
            do (apply function args))
      (car lists)))

  (defun mapcar (function &rest lists)
    (when (null lists)
      (error "not enough arguments to MAPCAR"))
    (let ((function (coerce-to-function function)))
      (loop for args = (pop-args lists)
            while args
            collect (apply function args))))

  (defun mapcan (function &rest lists)
    (when (null lists)
      (error "not enough arguments to MAPCAN"))
    (let ((function (coerce-to-function function)))
      (loop for args = (pop-args lists)
            while args
            nconc (apply function args))))

  (defun mapl (function &rest lists)
    (when (null lists)
      (error "not enough arguments to MAPL"))
    (let ((function (coerce-to-function function)))
      (loop for args = (pop-lists lists)
            while args
            do (apply function args))
      (car lists)))

  (defun maplist (function &rest lists)
    (when (null lists)
      (error "not enough arguments to MAPLIST"))
    (let ((function (coerce-to-function function)))
      (loop for args = (pop-lists lists)
            while args
            collect (apply function args))))

  (defun mapcon (function &rest lists)
    (when (null lists)
      (error "not enough arguments to MAPCON"))
    (let ((function (coerce-to-function function)))
      (loop for args = (pop-lists lists)
            while args
            nconc (apply function args)))))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun copy-alist (alist)
  (loop for cons in alist
        collect (if (consp cons)
                    (cons (car cons) (cdr cons))
                    cons)))

(defun pairlis (keys data &optional alist)
  (loop for key in keys
        for data in data
        do (push (cons key data) alist)))

