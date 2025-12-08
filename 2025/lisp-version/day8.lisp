(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day8.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day8_demo.input"))

(defun distance-cal (dx dy dz)
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))

(defun parse-input (input)
  (let ((all-points (loop for line in input
                          collect (str:match line
                                    ((x "," y "," z)
                                     (list (parse-integer x)
                                           (parse-integer y)
                                           (parse-integer z)))))))
    (let ((distance-nodes-pairs (make-hash-table :test 'equal))
          (all-distance nil))
      (do* ((rest-nodes all-points (cdr rest-nodes))
            (this (first rest-nodes) (first rest-nodes))
            (all-after (cdr rest-nodes) (cdr rest-nodes)))
           ((not all-after)
            (values
             (sort (remove-duplicates all-distance :test #'equal) #'<)
             distance-nodes-pairs
             (length input)))
        (loop for n in all-after
              for distance = (distance-cal
                              (- (first n) (first this))
                              (- (second n) (second this))
                              (- (third n) (third this)))
              do (setf (gethash distance distance-nodes-pairs)
                       (append (gethash distance distance-nodes-pairs)
                               (list (list this n))))
                 (push distance all-distance))))))

(defun gen-all-nodes-graph (distances distance-nodes-pairs)
  (let ((g (make-graph :graph-type 'undirected))
        (times 0)
        (limit 1000))
    (loop for d in distances
          if (= times limit)
            return nil
          do (loop for (a b) in (gethash d distance-nodes-pairs)
                   if (= times limit)
                     return nil
                   do (insert-graph-node g a b d)
                      (incf times)))
    g))

(defun rec-count (g start visited)
  (if (gethash start visited)
      (return-from rec-count 0)
      (setf (gethash start visited) t))
  (1+ (loop for next in (get-all-nodes-of-id-without-weight g start)
            sum (rec-count g next visited))))

(defun day8 (input)
  (multiple-value-bind (d m len)
      (parse-input input)
    (let ((g (gen-all-nodes-graph d m))
          (visited (make-hash-table :test #'equal))
          connect-count)
      (setf connect-count
            (loop for (k connects) in (get-all-nodes g)
                  unless (gethash k visited)
                    collect (rec-count g k visited)))
      (let ((s (sort connect-count #'>)))
        (* (first s) (second s) (third s))))))

(defun rec-visited (g start visited)
  (if (gethash start visited)
      (return-from rec-visited nil)
      (setf (gethash start visited) t))
  (loop for next in (get-all-nodes-of-id-without-weight g start)
        unless (gethash next visited)
          do (rec-visited g next visited)))

(defun day8-2 (input)
  (multiple-value-bind (d m len)
      (parse-input input)
    (let ((g (make-graph :graph-type 'undirected))
          start)
      (loop for dd in d
            do (loop for (a b) in (gethash dd m)
                     unless start
                       do (setf start a)
                     do (insert-graph-node g a b d)
                        (let ((visited (make-hash-table :test #'equal)))
                          (rec-visited g start visited)
                          (if (= len (hash-table-count visited))
                              (return-from day8-2 (* (first a) (first b))))))))))

;; (multiple-value-bind (d m len)
;;     (parse-input *input-demo*)
;;   (loop for dd in d
;;         do (format t "~a~%" (gethash dd m))))

;; (multiple-value-bind (d m len)
;;     (parse-input *input-demo*)
;;   (let ((g (gen-all-nodes-graph d m))
;;         (visited (make-hash-table :test #'equal)))
;;     (format t "~a" (get-all-nodes g))
;;     (rec-count g '(162 817 812) visited)
;;     (alexandria:hash-table-plist visited)))
