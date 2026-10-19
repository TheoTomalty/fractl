(defmacro seq-case (sequence &body (list-case vector-case))
  `(let ((it ,sequence))
     (etypecase it
       (list ,list-case)
       (vector ,vector-case))))

(defun copy-seq (sequence)
  (seq-case sequence
    (copy-list sequence)
    (make-array (array-dimensions sequence)
                :initial-contents sequence
                :element-type (array-element-type sequence))))

(defun elt (sequence index)
  (seq-case sequence
    (nth index sequence)
    (aref sequence index)))

(defun (setf elt) (new sequence index)
  (seq-case sequence
    (setf (nth index sequence) new)
    (setf (aref sequence index) new)))

(defun fill (sequence item &key (start 0) (end (length sequence)))
  (seq-case sequence
    (loop for list on sequence and i from 0 below end
          when (<= start i)
            do (setf (car list) item))
    (loop for i from start below (if end-p end (length sequence))
          do (setf (aref sequence item) item))))

(defun extract-element-type (type)
  (let* ((expanded-type (expand-type type))
         (name (if (consp expanded-type) (car expanded-type) expanded-type))
         (args (if (consp expanded-type) (cdr expanded-type) nil)))
    (unless (typep name '(member array simple-array))
      (error "Type must be an array type."))
    (if args (if (eql '* (car args)) t (car args)) t)))

(defun valid-sequence-type-p (type)
  (or (subtypep type 'list) (subtypep type 'vector)))

(defmacro seq-subtype-case (type (element-type) &body (list-case vector-case))
  (let ((type-sym (make-symbol "TYPE")))
    `(let* ((,type-sym ,type)
            (ret (cond
                   ((subtypep ,type-sym 'list)
                    ,list-case)
                   ((subtypep ,type-sym 'vector)
                    (let ((,element-type (extract-element-type result-type)))
                      ,vector-case))
                   (t (error 'simple-type-error
                             :datum ,type-sym
                             :expected-type '(satisfies valid-sequence-type-p)
                             :format-control "The result-type ~s must be a subtype of LIST or VECTOR."
                             :format-arguments (list ,type-sym))))))
       (unless (typep ret ,type-sym)
         (simple-type-error ret ,type-sym))
       ret)))

(defun make-sequence (result-type size &rest args &key initial-element)
  (seq-subtype-case result-type (element-type)
    (apply #'make-list size args)
    (apply #'make-array (list size) :element-type element-type args)))

(defun subseq (sequence start &optional (end (length sequence)))
  (seq-case sequence
    (loop for elem in sequence and i from 0 below end
          when (<= start i)
            collect elem)
    (let ((ret (make-array (- end start) :element-type (array-element-type sequence))))
      (loop for i from start below end
            do (setf (aref ret (- i start)) (aref sequence i)))
      ret)))

(defsetf subseq (sequence start &optional end) (new)
  `(progn (replace ,sequence ,new :start1 ,start :end1 ,end) ,new))

(defun sequence-iter (i sequences)
  (loop for sequences on sequences
        collect (seq-case (car sequences)
                  (if it
                      (prog1 (car it)
                        (rplaca (car sequences) (cdr it)))
                      (return :end))
                  (if (< i (length it))
                      (aref it i)
                      (return :end)))))

(defun map (result-type function sequence &rest sequences)
  (push sequence sequences)
  (if (null result-type)
      (loop for i from 0
            for iter = (sequence-iter i sequences)
            until (eql :end iter)
            do (apply function iter))
      (seq-subtype-case result-type (element-type)
        (loop for i from 0
              for iter = (sequence-iter i sequences)
              until (eql :end iter)
              collect (apply function iter))
        (let ((ret (make-array (loop for seq in sequences
                                     minimize (length seq))
                               :element-type element-type)))
          (loop for i from 0
                for iter = (sequence-iter i sequences)
                until (eql :end iter)
                do (setf (aref ret i) (apply function iter)))
          ret))))

(defun map-into (result-sequence function &rest sequences)
  (seq-case result-sequence
    (loop for results on result-sequence
          and i from 0
          for iter = (sequence-iter i sequences)
          until (eql :end iter)
          do (setf (car results) (apply function iter))
          finally (check-final results result-sequence))
    (loop for i below (length result-sequence)
          for iter = (sequence-iter i sequences)
          until (eql :end iter)
          do (setf (aref result-sequence i) (apply function iter)))))

(defun reduce (function sequence &key key from-end (start 0) end (initial-value nil initial-value-p))
  (setf function (coerce-to-function function)
        key (when key (coerce-to-function key)))
  (seq-case sequence
    ;; TODO: need to handle start and end in list implementation
    (labels ((auxl (acc l)
               (if l
                   (auxl (funcall function acc (apply-key (car l))) (cdr l))
                   acc))
             (auxri (l)
               (if l
                   (funcall function (apply-key (car l)) (auxri (cdr l)))
                   initial-value))
             (auxrn (l)
               (if (cdr l)
                   (funcall function (apply-key (car l)) (auxri (cdr l)))
                   (apply-key (car l)))))
      (if from-end
          (if initial-value-p
              (auxri sequence)
              (if sequence
                  (auxrn sequence)
                  (funcall function)))
          (progn
            (unless initial-value-p
              (if sequence
                  (setf initial-value (apply-key (pop sequence)))
                  (return-from reduce (funcall function))))
            (auxl initial-value sequence))))
    (let ((n (max (- (or end (setf end (length sequence))) start) 0))
          (ret initial-value))
      (unless initial-value-p
        (if (< 0 n)
            (if from-end
                (setf ret (apply-key (aref sequence (decf end))))
                (setf ret (apply-key (aref sequence start))
                      start (1+ start)))
            (return-from reduce (funcall function))))
      (if from-end
          (loop for i downfrom (1- end) to start
                do (setf ret (funcall function (apply-key (aref sequence i)) ret)))
          (loop for i from start below end
                do (setf ret (funcall function ret (apply-key (aref sequence i))))))
      ret)))

(macrolet ((count-body (name)
             (flet ((expand-condition (expr)
                      (ecase name
                        (count `(funcall test item ,expr))
                        (count-if `(funcall predicate ,expr))
                        (count-if-not `(not (funcall predicate ,expr))))))
               `(progn
                  (setf ,@(ecase name
                            (count '(test (coerce-to-function test)))
                            ((count-if count-if-not)
                             '(predicate (coerce-to-function predicate))))
                        key (when key (coerce-to-function key))
                        end (or end (length sequence)))
                  (seq-case sequence
                    (if from-end
                        (labels ((aux (l i)
                                   (if (and l (< i end))
                                       (let ((acc (aux (cdr l) (1+ i))))
                                         (if (<= i start)
                                             (+ acc
                                                (if ,(expand-condition '(apply-key (car l)))
                                                    1 0))
                                             acc))
                                       0)))
                          (aux sequence 0))
                        (loop for elem in sequence and i from 0 below end
                              when (<= start i)
                                count ,(expand-condition '(apply-key elem))))
                    (if from-end
                        (loop for i from start below end
                              count ,(expand-condition '(apply-key (aref sequence i))))
                        (loop for i downfrom (1- end) to start
                              count ,(expand-condition '(apply-key (aref sequence i))))))))))
  
  (defun count (item sequence &key key from-end (start 0) end (test #'eql))
    (count-body count))

  (defun count-if (predicate sequence &key key from-end (start 0) end)
    (count-body count-if))

  (defun count-if-not (predicate sequence &key key from-end (start 0) end)
    (count-body count-if-not)))

(defun length (sequence)
  (seq-case sequence
    (list-length sequence)
    (array-length sequence)))

(defun nreverse (sequence)
  (seq-case sequence
    (list-nreverse sequence)
    (loop for j = (1- (length sequence)) then (1- end)
          and i from 0
          while (< i j)
          do (rotatef (aref sequence i) (aref sequence j)))))

(defun reverse (sequence)
  (seq-case sequence
    (list-reverse sequence)
    (nreverse (copy-seq sequence))))

(defun sort (sequence predicate &key key)
  (setf predicate (coerce-to-function predicate)
        key (when key (coerce-to-function key)))
  (seq-case sequence
    (list-sort sequence predicate key)
    (labels ((aux (start end &aux (length (- end start)))
               (unless (= 1 length)
                 (let ((half (+ start (ceiling length 2))))
                   (aux start half)
                   (aux half end)
                   (loop with i = start and j = half and k = start
                         while (and (< i half) (< j end))
                         do (setf (aref sequence (incf* k))
                                  (if (funcall predicate
                                               (apply-key (aref sequence i))
                                               (apply-key (aref sequence j)))
                                      (aref sequence (incf* i))
                                      (aref sequence (incf* j))))
                         finally (loop for q from i below half
                                       do (setf (aref sequence (incf* k)) (aref sequence q)))
                                 (loop for q from j below end
                                       do (setf (aref sequence (incf* k)) (aref sequence q))))))))
      (let ((length (length sequence)))
        (unless (<= length 1)
          (aux 0 length)))
      sequence)))

(defun stable-sort (sequence predicate &key key)
  ;; Fix sort to be stable, and this can just be an alias
  (error "unimplemented"))

(defmacro normalize-list (sequence start end)
  `(when (listp ,sequence)
     (loop for i below ,start
           do (pop ,sequence)
              (decf ,end))))

(macrolet ((find/position-body (name)
             (flet ((expand-condition (expr)
                      (ecase name
                        ((find position) `(funcall test item ,expr))
                        ((find-if position-if) `(funcall predicate ,expr))
                        ((find-if-not position-if-not) `(not (funcall predicate ,expr)))))
                    (elem-or-i (&key add-start)
                      (ecase name
                        ((find find-if find-if-not) 'elem)
                        ((position position-if position-if-not) (if add-start '(+ i start) 'i)))))
               `(progn
                  (setf ,@(ecase name
                            ((find position) '(test (coerce-to-function test)))
                            ((find-if find-if-not position-if position-if-not)
                             '(predicate (coerce-to-function predicate))))
                        key (when key (coerce-to-function key))
                        end (or end (length sequence)))
                  (normalize-list sequence start end)
                  (seq-case sequence
                    (if from-end
                        (labels ((aux (l i)
                                   (when (and l (< i end))
                                     (aux (cdr l) (1+ i))
                                     (let ((elem (car l)))
                                       (when ,(expand-condition '(apply-key elem))
                                         (return-from ,name ,(elem-or-i :add-start t)))))))
                          (aux sequence 0))
                        (loop for elem in sequence and i below end
                              when ,(expand-condition '(apply-key elem))
                                return ,(elem-or-i :add-start t)))
                    (if from-end
                        (loop for i from start below end
                              for elem = (aref sequence i)
                              when ,(expand-condition '(apply-key elem))
                                return ,(elem-or-i))
                        (loop for i downfrom (1- end) to start
                              for elem = (aref sequence i)
                              when ,(expand-condition '(apply-key elem))
                                return ,(elem-or-i))))))))
  

  (defun find (item sequence &key key from-end (start 0) end (test #'eql))
    (find/position-body find))

  (defun find-if (predicate sequence &key key from-end (start 0) end)
    (find/position-body find-if))

  (defun find-if-not (predicate sequence &key key from-end (start 0) end)
    (find/position-body find-if-not))

  (defun position (item sequence &key key from-end (start 0) end (test #'eql))
    (find/position-body position))

  (defun position-if (predicate sequence &key key from-end (start 0) end)
    (find/position-body position-if))

  (defun position-if-not (predicate sequence &key key from-end (start 0) end)
    (find/position-body position-if-not)))


(defun search (sequence-1 sequence-2 &key from-end (test #'eql) key (start1 0) (start2 0) end1 end2)
  (setf key (when key (coerce-to-function key))
        test (coerce-to-function test)
        end1 (or end1 (length sequence-1))
        end2 (or end2 (length sequence-2)))
  ;; TODO: use mismatch function instead of matches-list and matches-vector
  (normalize-list sequence-1 start1 end1)
  (flet ((matches-list (list index)
           (seq-case sequence-1
             (loop for elem-1 in sequence-1 and i below end1
                   never (or (endp list) (not (funcall test (apply-key (pop list)) (apply-key elem-1)))))
             (loop for i from start1 below end1
                   never (or (endp list) (not (funcall test (apply-key (pop list)) (apply-key (aref sequence-1 i)))))))
           index)
         (matches-vector (start)
           (seq-case sequence-1
             (loop for elem-1 in sequence-1 and i below end1 for j = (+ i start)
                   never (or (<= end2 j) (not (funcall test (apply-key (aref sequence-2 j)) (apply-key elem-1)))))
             (loop for i from start1 below end1 for j = (+ i start)
                   never (or (<= end2 j) (not (funcall test (apply-key (aref sequence-2 j)) (apply-key (aref sequence-1 i)))))))
           start))
    (seq-case sequence-2
      (if from-end
          (labels ((aux (list i)
                     (when (and list (< i end2))
                       (aux (cdr list) (1+ i))
                       (awhen (and (<= i start2) (matches-list list i))
                         (return-from search it)))))
            (aux sequence-2 0))
          (loop for list on sequence-2 and i from 0 below end2
                  thereis (and (<= i start2) (matches-list list i))))
      (if from-end
          (loop for i downfrom (1- end2) to start2
                  thereis (matches-vector i))
          (loop for i from start2 below end2
                  thereis (matches-vector i))))))

(defun mismatch (sequence-1 sequence-2 &key from-end (test #'eql) key (start1 0) (start2 0) end1 end2 &aux (start start1) end)
  (setf key (when key (coerce-to-function key))
        test (coerce-to-function test)
        end1 (or end1 (length sequence-1))
        end2 (or end2 (length sequence-2))
        end end1)
  (normalize-list sequence-1 start1 end1)
  (normalize-list sequence-2 start2 end2)
  ;; FIXME: this sucks
  (when from-end
    (nreverse sequence-1)
    (nreverse sequence-2))
  (let ((i 0))
    (flet ((iter-1 ()
             (seq-case sequence-1
               (values (pop sequence-1) sequence-1)
               (aref sequence-1 i)))
           (iter-2 ()
             (seq-case sequence-2
               (pop sequence-2)
               (aref sequence-2 i))))
      (let ((ret (loop (multiple-value-bind (elem-1 endp-1) (iter-1)
                         (multiple-value-bind (elem-2 endp-2) (iter-2)
                           (if (and endp-1 endp-2)
                               (return)
                               (when (or endp-1 endp-2 (not (funcall test (apply-key elem-1) (apply-key elem-2))))
                                 (return (+ i start)))))))))
        (when from-end
          (nreverse sequence-1)
          (nreverse sequence-2)
          (when ret
            (setf ret (- end ret))))
        ret))))

(defun replace (sequence-1 sequence-2 &key (start1 0) end1 (start2 0) end2)
  (setf end1 (or end1 (length sequence-1))
        end2 (or end2 (length sequence-2)))
  (normalize-list sequence-1 start1 end1)
  (normalize-list sequence-2 start2 end2)
  (seq-case sequence-1
    (seq-case sequence-2
      (loop for i below (min end1 end2)
            while (and sequence-1 sequence-2)
            do (setf (car sequence-1) (car sequence-2)
                     sequence-1 (cdr sequence-1)
                     sequence-2 (cdr sequence-2)))
      (loop for i below end1 and j from start2 below end2
            while sequence-1
            do (setf (car sequence-1) (aref sequence-2 j)
                     sequence-1 (cdr sequence-1))))
    (seq-case sequence-2
      (loop for i from start1 below end2 and j below end2
            while sequence-2
            do (setf (aref sequence-1 i) (car sequence-2)
                     sequence-2 (cdr sequence-2)))
      (loop for i from start1 below end1 and j from start1 below end2
            do (setf (aref sequence-1 i) (aref sequence-2 j))))))

(macrolet ((nsubstitute-body (name)
             (flet ((expand-condition (expr)
                      (ecase name
                        (nsubstitute `(funcall test old ,expr))
                        (nsubstitute-if `(funcall predicate ,expr))
                        (nsubstitute-if-not `(not (funcall predicate ,expr)))))
                    (decf-count ()
                      `(when count
                         (decf count)
                         (when (= 0 count)
                           (return-from ,name sequence)))))
               `(progn
                  (setf ,@(ecase name
                            (nsubstitute '(test (coerce-to-function test)))
                            ((nsubstitute-if nsubstitute-if-not)
                             '(predicate (coerce-to-function predicate))))
                        key (when key (coerce-to-function key))
                        end (or end (length sequence)))
                  (normalize-list sequence start end)
                  (seq-case sequence
                    (if from-end
                        (labels ((aux (l i)
                                   (when (and l (< i end))
                                     (aux (cdr l) (1+ i))
                                     (when ,(expand-condition '(apply-key (car l)))
                                       ,(decf-count)))))
                          (aux sequence 0))
                        (loop for elem in sequence and i below end
                              when ,(expand-condition '(apply-key elem))
                                do ,(decf-count)))
                    (if from-end
                        (loop for i from start below end
                              for elem = (aref sequence i)
                              when ,(expand-condition '(apply-key elem))
                                do ,(decf-count))
                        (loop for i downfrom (1- end) to start
                              for elem = (aref sequence i)
                              when ,(expand-condition '(apply-key elem))
                                do ,(decf-count))))
                  sequence))))
  
  (defun nsubstitute (new old sequence &key key from-end (start 0) end (test #'eql) count)
    (nsubstitute-body nsubstitute))

  (defun nsubstitute-if (new predicate sequence &key key from-end (start 0) end count)
    (nsubstitute-body nsubstitute-if))

  (defun nsubstitute-if-not (new predicate sequence &key key from-end (start 0) end count)
    (nsubstitute-body nsubstitute-if-not))

  (defun substitute (new old sequence &rest args &key key from-end (start 0) end (test #'eql) count)
    (setf sequence (copy-seq sequence))
    (apply #'nsubstitute new old sequence args))

  (defun substitute-if (new predicate sequence &key key from-end (start 0) end count)
    (setf sequence (copy-seq sequence))
    (apply #'nsubstitute-if new predicate sequence args))

  (defun substitute-if-not (new predicate sequence &key key from-end (start 0) end count)
    (setf sequence (copy-seq sequence))
    (apply #'nsubstitute-if-not new predicate sequence args)))

(defun concatenate (result-type &rest sequences)
  (macrolet ((body (&body body)
               `(loop for sequence in sequences
                      do (seq-case sequence
                           (loop for elem in sequence
                                 do (progn ,@body))
                           (loop for elem across sequence
                                 do (progn ,@body))))))
    (seq-subtype-case result-type (element-type)
      (with-ext-lists ((ret ret-end))
        (body (push-tail elem ret-end))
        ret)
      (let ((ret (make-array 16 :fill-pointer 0 :adjustable t :element-type element-type)))
        (body (vector-push-extend elem ret))
        ret))))

(defun merge (result-type sequence-1 sequence-2 predicate &key key)
  (setf predicate (coerce-to-function predicate)
        key (when key (coerce-to-function key)))
  (macrolet ((body (type-r type-1 type-2)
               (flet ((push-elem (elem)
                        (ecase type-r
                          (list `(push-tail ,elem ret-end))
                          (vector `(vector-push-extend ,elem ret))))
                      (seq-type (sequence)
                        (ecase sequence
                          (sequence-1 type-1)
                          (sequence-2 type-2)))
                      (seq-index (sequence)
                        (ecase sequence
                          (sequence-1 'i)
                          (sequence-2 'j)))
                      (condition (sequence)
                        (ecase (seq-type sequence)
                          (list sequence)
                          ;; TODO: lift length computation
                          (vector `(< ,(seq-index sequence) (length sequence)))))
                      (seq-next (sequence)
                        (ecase (seq-type sequence)
                          (list `(car ,sequence))
                          (vector `(aref ,sequence ,(seq-index sequence)))))
                      (seq-pop (sequence)
                        (ecase (seq-type sequence)
                          (list `(setf ,sequence (cdr ,sequence)))
                          (vector `(incf ,(seq-index sequence))))))
                 `(loop while (and ,(condition 'sequence-1) ,(condition 'sequence-2))
                        do (let ((elem-1 ,(seq-next 'sequence-1))
                                 (elem-2 ,(seq-next 'sequence-2)))
                             (if (funcall predicate (apply-key elem-1) (apply-key elem-2))
                                 (progn ,(push-elem 'elem-1) ,(seq-pop 'sequence-1))
                                 (progn ,(push-elem 'elem-2) ,(seq-pop 'sequence-2))))
                        finally (loop while ,(condition 'sequence-1)
                                      do ,(push-elem (seq-next 'sequence-1)) ,(seq-pop 'sequence-1))
                                (loop while ,(condition 'sequence-2)
                                      do ,(push-elem (seq-next 'sequence-2)) ,(seq-pop 'sequence-2))))))
    (seq-subtype-case result-type (element-type)
      (with-ext-lists ((ret ret-end))
        (seq-case sequence-1
          (seq-case sequence-2
            (body list list list)
            (body list list vector))
          (seq-case sequence-2
            (body list vector list)
            (body list vector vector)))
        ret)
      (let ((ret (make-array 16 :fill-pointer 0 :adjustable t :element-type element-type)))
        (seq-case sequence-1
          (seq-case sequence-2
            (body vector list list)
            (body vector list vector))
          (seq-case sequence-2
            (body vector vector list)
            (body vector vector vector)))
        ret))))

(macrolet ((body (name)
             (flet ((condition (elem)
                      (ecase name
                        (remove `(funcall test (apply-key item) (apply-key ,elem)))
                        (remove-if `(funcall predicate ,elem))
                        (remove-if-not `(not (funcall predicate ,elem)))))
                    (decf-count (type)
                      `(when count
                         (decf count)
                         (when (= 0 count)
                           (return-from ,name ,(case type
                                                 (list '(cdr sequence))
                                                 (vector '(go end-vector))))))))
               `(progn
                  (setf ,@(ecase name
                            (remove `(test (coerce-to-function test)))
                            ((remove-if remove-if-not) `(predicate (coerce-to-function predicate))))
                        key (when key (coerce-to-function key))
                        end (or end (length sequence)))
                  (normalize-list sequence start end)
                  (seq-case sequence
                    (let ((sequence (cons nil sequence)))
                      (if from-end
                          (labels ((aux (l i)
                                     (when (and (cdr l) (< i end))
                                       (aux (cdr l) (1+ i))
                                       (when ,(condition '(cadr l))
                                         (setf (cdr l) (cddr l))
                                         ,(decf-count 'list)))))
                            (aux sequence 0))
                          (loop with l = sequence
                                while (cdr l)
                                for i below end
                                do (if ,(condition '(cadr l))
                                       (progn
                                         (setf (cdr l) (cddr l))
                                         ,(decf-count 'list))
                                       (setf l (cdr l)))))
                      (cdr sequence))
                    (if from-end
                        (prog ((target end))
                           (loop for i downfrom (1- end) to start
                                 with target = end
                                 do (let ((elem (aref sequence i)))
                                      (if ,(condition 'elem)
                                          ,(decf-count 'vector)
                                          (setf (aref sequence (decf target)) elem))))
                         end-vector
                           (loop for j from target below end
                                 and i from 0
                                 do (setf (aref sequence i) (aref sequence j)))
                           (shrink-vector sequence (- end target))
                           (return-from ,name sequence))
                        (prog ((target start))
                           (loop for i from start below end
                              do (let ((elem (aref sequence i)))
                                   (if ,(condition 'elem)
                                       ,(decf-count 'vector)
                                       (setf (aref sequence (incf* target)) elem))))
                         end-vector
                           (shrink-vector sequence target)
                           (return-from ,name sequence))))))))

  (defun remove (item sequence &key from-end (test #'eql) (start 0) end count key)
    (body remove))

  (defun remove-if (predicate sequence &key from-end (start 0) end count key)
    (body remove-if))

  (defun remove-if-not (predicate sequence &key from-end (start 0) end count key)
    (body remove-if-not))

  (defun delete (item sequence &rest args &key from-end (test #'eql) (start 0) end count key)
    (apply #'remove item (copy-seq sequence) args))

  (defun delete-if (predicate sequence &rest args &key from-end (start 0) end count key)
    (apply #'remove-if predicate (copy-seq sequence) args))

  (defun delete-if-not (predicate sequence &rest args &key from-end (start 0) end count key)
    (apply #'remove-if-not predicate (copy-seq sequence) args)))

(defun remove-duplicates (sequence &key from-end (test #'eql) (start 0) end key)
  (setf test (coerce-to-function test))
  (let (seen)
    (remove-if
     (lambda (elem)
       (if (member elem seen :test test)
           t
           (progn (push elem seen) nil)))
     sequence :from-end from-end :start start :end end :key key)))

(defun delete-duplicates (sequence &key from-end (test #'eql) (start 0) end key)
  (setf test (coerce-to-function test))
  (let (seen)
    (delete-if
     (lambda (elem)
       (if (member elem seen :test test)
           t
           (progn (push elem seen) nil)))
     sequence :from-end from-end :start start :end end :key key)))
