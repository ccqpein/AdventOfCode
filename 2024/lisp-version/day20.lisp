(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day20.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day20_demo.input"))

(defun parse-input (input)
  (gen-aoc-map (loop for l in input collect (concatenate 'list l))
               :ele-coops t :coop-ele t))

;; (defun next-steps (m coop)
;;   (aoc-map-around-coop m coop :dir 'hor-ver))

;; use the tools.lisp dijkstra instead
;; (defun dijkstra (m start end)
;;   (let ((distance-table (make-hash-table :test 'equal))
;;         (visited (make-hash-set)))
;;     (setf (gethash start distance-table) 0)
;;     (do* (;; (coop)
;;           (this start) 
;;           ;; (coop cost)
;;           (next-round (make-instance 'cl-heap:binary-heap
;;                                      :sort-fun #'<
;;                                      :key #'second))
;;           (this-to-start-value (gethash this distance-table)
;;                                (gethash this distance-table))
;;           )
;;          ((or (equal this end) (not this))
;;           distance-table)
;;       (if (set-get visited this)
;;           nil
;;           (loop for next in (next-steps m this)
;;                 unless (equal (get-aoc-map-ele m next) #\#)
;;                   unless (set-get visited next)
;;                     do (if (or (not (gethash next distance-table))
;;                                (> (gethash next distance-table)
;;                                   (+ this-to-start-value 1)))
;;                            (progn (setf (gethash next distance-table)
;;                                         (+ this-to-start-value 1))
;;                                   (cl-heap:add-to-heap next-round (list next (+ this-to-start-value 1)))))))
;;       (set-insert visited this)
;;       (setf this (let ((tt (cl-heap:pop-heap next-round)))
;;                    (if tt (first tt) nil))))))


;; (day20 *input* 100)

(defun next-steps (m coop)
  (loop for n in (aoc-map-around-coop m coop :dir 'hor-ver) collect (list 1 n)))

(defun day20 (input save-step)
  (let ((m (parse-input input)))
    (let ((start (first (get-aoc-map-coops-of-ele m #\S)))
          (end (first (get-aoc-map-coops-of-ele m #\E))))
      ;;(format t "start: ~a, end: ~a~%" start end)
    
      (let ((distance-table (dijkstra m start
                                      :end end
                                      :next-steps-fun #'next-steps)))
        ;;(format t "~a~%" (alexandria:hash-table-alist distance-table))
        (length
         (loop for w in (get-aoc-map-coops-of-ele m #\#)
               append (loop for (offset-a offset-b)  in '(((1 0) (-1 0)) ((0 1) (0 -1)))
                            for a = (gethash (mapcar #'+ w offset-a) distance-table)
                            and b = (gethash (mapcar #'+ w offset-b) distance-table)
                            when (and a b)
                              when (>= (- (abs (- a b)) 2) save-step)
                                collect (- (abs (- a b)) 2))))))))

(defun manhattan-distance (x y)
  (+ (abs (- (first x) (first y)))
     (abs (- (second x) (second y)))))

;; (day20-2 *input* 100)

(defun day20-2 (input save-step)
  (let* ((m (parse-input input))
         (start (first (get-aoc-map-coops-of-ele m #\S)))
         (end (first (get-aoc-map-coops-of-ele m #\E))))

    (let ((distance-table (dijkstra m start
                                      :end end
                                      :next-steps-fun #'next-steps))
          cheat-table)
      (setf cheat-table
            (do ((all-path (sort (alexandria:hash-table-alist distance-table) #'< :key #'cdr)
                           (cdr all-path))
                 (table (make-hash-table :test 'equal)))
                ((not all-path) table)
              (loop with this = (first all-path)
                    for n in (cdr all-path)
                    when (<= (manhattan-distance (car this) (car n)) 20)
                      do ;;(format t "this: ~a, other: ~a~%" this n)
                         (push (list (car this) (car n))
                               (gethash (- (abs (- (cdr this) (cdr n)))
                                           (manhattan-distance (car this) (car n)))
                                        table nil)))))
      (loop for n in (alexandria:hash-table-keys cheat-table)
            when (>= n save-step)
              do (format t "n: ~a, way: ~a~%" n (length (gethash n cheat-table)))
              and sum (length (gethash n cheat-table))))))
