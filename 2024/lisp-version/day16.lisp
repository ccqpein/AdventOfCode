(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day16.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day16_demo.input"))

(defun parse-input (input)
  (gen-aoc-map (loop for line in input
                     collect (concatenate 'list line))
               :ele-coops t :coop-ele t))

;; (defun next-steps (coop dir)
;;   (let (offsets)
;;     (setf offsets
;;           (cond ((eq dir 'right)
;;                  '(((-1 0) up) ((1 0) down) ((0 1) right)))
;;                 ((eq dir 'left)
;;                  '(((-1 0) up) ((1 0) down) ((0 -1) left)))
;;                 ((eq dir 'up)
;;                  '(((0 -1) left) ((0 1) right) ((-1 0) up)))
;;                 ((eq dir 'down)
;;                  '(((0 -1) left) ((0 1) right) ((1 0) down)))))
;;     (loop for (o dir) in offsets
;;           collect (list (mapcar #'+ o coop) dir))))

;; (defun dijkstra (m start)
;;   (let ((distance-table (make-hash-table :test 'equal))
;;         (visited (make-hash-set)))
;;     (setf (gethash start distance-table) 0)
;;     (do* (;; (coop dir)
;;           (this start) 
;;           ;; (coop dir cost)
;;           (next-round (make-instance 'cl-heap:binary-heap
;;                                      :sort-fun #'<
;;                                      :key #'third))
;;           (this-to-start-value (gethash this distance-table)
;;                                (gethash this distance-table))
;;           )
;;          (
;;           ;;(equal (get-aoc-map-ele m (first this)) #\E)
;;           (not this)
;;           distance-table)
;;       ;;(format t "this is ~a, heap is ~a~%" this (slot-value next-round 'cl-heap::data))
;;       (if (set-get visited this)
;;           nil
;;           (loop for (next next-dir) in (next-steps (first this) (second this))
;;                 unless (equal (get-aoc-map-ele m next) #\#)
;;                   unless (set-get visited (list next next-dir))
;;                     do (let ((cost (if (eq next-dir (second this)) 1 1001)))
;;                          (if (or (not (gethash (list next next-dir) distance-table))
;;                                  (> (gethash (list next next-dir) distance-table)
;;                                     (+ this-to-start-value cost)))
;;                              (progn (setf (gethash (list next next-dir) distance-table)
;;                                           (+ this-to-start-value cost))
;;                                     (cl-heap:add-to-heap next-round (list next next-dir (+ this-to-start-value cost))))))
;;                 ))
;;       ;;(format t "after heap is:~a~%" (slot-value next-round 'cl-heap::data))
;;       (set-insert visited this)
;;       (setf this (let ((tt (cl-heap:pop-heap next-round)))
;;                    (if tt (subseq tt 0 2) nil)))
;;       )))

;; (defun day16 (input)
;;   (let ((m (parse-input input)))
;;     (let ((start-coop (car (get-aoc-map-coops-of-ele m #\S)))
;;           (end-coop (car (get-aoc-map-coops-of-ele m #\E))))
;;       (apply #'min
;;              (remove nil
;;                      (loop for dir in '(up down left right)
;;                            collect (gethash (list end-coop dir) (dijkstra m (list start-coop 'right)))))))))

;;;;;;;;;;;;;;;;;;;;; re-write with tools.lisp dijkstra below

(defun next-steps (m coop)
  (declare (ignore m))
  (let (offsets
        (dir (third coop)))
    (setf offsets
          (cond ((eq dir 'right)
                 '(((-1 0) up) ((1 0) down) ((0 1) right)))
                ((eq dir 'left)
                 '(((-1 0) up) ((1 0) down) ((0 -1) left)))
                ((eq dir 'up)
                 '(((0 -1) left) ((0 1) right) ((-1 0) up)))
                ((eq dir 'down)
                 '(((0 -1) left) ((0 1) right) ((1 0) down)))))
    (loop for (o new-dir) in offsets
          collect (list
                   (if (equal new-dir dir)
                       1
                       1001)
                   (append (mapcar #'+ o coop) (list new-dir))))))


(defun day16 (input)
  (let ((m (parse-input input)))
    (let ((start-coop (car (get-aoc-map-coops-of-ele m #\S)))
          (end-coop (car (get-aoc-map-coops-of-ele m #\E))))
      ;;(format t "end: ~a~%" end-coop)
      (apply #'min
             (remove nil
                     (loop for dir in '(up down left right)
                           for table = (dijkstra m (list start-coop 'right)
                                                 :end end-coop
                                                 :with-dir t
                                                 :next-steps-fun #'next-steps)
                           ;; do (format t "~a~%" (alexandria:hash-table-alist table))
                           ;;    (format t "~a~%" (gethash (append end-coop dir) table))
                           ;;    (format t "~a~%" (append end-coop dir))
                           collect (gethash (append end-coop (list dir))
                                            (dijkstra m (list start-coop 'right)
                                                      ;;:end end-coop
                                                      :with-dir t
                                                      :next-steps-fun #'next-steps))))))))

;;;;;;;;;;;;;;; dijkstra-2 is too special for tools

(defun dijkstra-2 (m start table)
  (do* (;; (coop dir cost all-path)
        (this start) 
        ;; (coop dir cost all-path)
        (next-round '())
        (all-paths))
       ((not this)
        all-paths)
    ;;(format t "this is ~a, heap is ~a~%" this (slot-value next-round 'cl-heap::data))
    (if  (equal (get-aoc-map-ele m (first this)) #\E)
         (progn
           (setf (gethash (first this) (fourth this)) t)
           (push this all-paths))
         (if (or (gethash (first this) (fourth this))
                 (> (third this) (gethash (subseq this 0 2) table)))
             nil
             (loop for (next next-dir) in (next-steps (first this) (second this))
                   unless (equal (get-aoc-map-ele m next) #\#)
                     unless (gethash (first this) (fourth this))
                       do (let ((cost (if (eq next-dir (second this)) 1 1001))
                                (new-table (alexandria:copy-hash-table (fourth this))))
                            (setf (gethash (first this) new-table) t)
                            (push (list next
                                        next-dir
                                        (+ (third this) cost)
                                        new-table)
                                  next-round))))
         ;;(format t "after heap is:~a~%" (slot-value next-round 'cl-heap::data))
         )
    (setf this (pop next-round))))

(defun day16-2 (input)
  (let ((m (parse-input input)))
    (let ((start-coop (car (get-aoc-map-coops-of-ele m #\S)))
          (table (make-hash-table :test 'equal)))
      (let* ((steps-record (dijkstra m (list start-coop 'right)))
             (result (dijkstra-2 m (list start-coop 'right 0 table) steps-record))
             (steps (day16 input)))
        (format t "steps: ~a~%" steps)
        (let ((all-points (remove-duplicates
                           (loop for r in result
                                 when (= steps (third r))
                                   append (loop for c being each hash-key of (fourth r)
                                                collect c))
                           :test 'equal)))
          ;;(format t "points: ~a~%" all-points)
          (length all-points))))))
