(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day18.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day18_demo.input"))

(defun parse-input (input length)
  (let (m)
    (setf m (gen-aoc-map (loop for r from 0 to length collect (make-list (1+ length) :initial-element #\.))
                         :ele-coops t :coop-ele t))
    (loop for l in input
          do (str:match l ((c "," r) (set-aoc-map-ele m (list (parse-integer r) (parse-integer c)) #\#))))
    (print-raw-map m)
    m))

(defun next-steps (m coop)
  (aoc-map-around-coop m coop :dir 'hor-ver))

(defun dijkstra (m start end)
  (let ((distance-table (make-hash-table :test 'equal))
        (visited (make-hash-set)))
    (setf (gethash start distance-table) 0)
    (do* (;; (coop)
          (this start) 
          ;; (coop cost)
          (next-round (make-instance 'cl-heap:binary-heap
                                     :sort-fun #'<
                                     :key #'second))
          (this-to-start-value (gethash this distance-table)
                               (gethash this distance-table))
          )
         (
          (or (equal this (list end end)) (not this))
          (gethash (list end end) distance-table))
      ;;(format t "this is ~a, heap is ~a~%" this (slot-value next-round 'cl-heap::data))
      (if (set-get visited this)
          nil
          (loop for next in (next-steps m this)
                unless (equal (get-aoc-map-ele m next) #\#)
                  unless (set-get visited next)
                    do (if (or (not (gethash next distance-table))
                               (> (gethash next distance-table)
                                  (+ this-to-start-value 1)))
                           (progn (setf (gethash next distance-table)
                                        (+ this-to-start-value 1))
                                  (cl-heap:add-to-heap next-round (list next (+ this-to-start-value 1)))))
                ))
      ;;(format t "after heap is:~a~%" (slot-value next-round 'cl-heap::data))
      (set-insert visited this)
      (setf this (let ((tt (cl-heap:pop-heap next-round)))
                   (if tt (first tt) nil)))
      )))

(defun day18 ()
  (let ((m (parse-input (subseq *input* 0 1024) 70)))
    (dijkstra m '(0 0) 70)))

(defun day18-2 ()
  (let ((m (parse-input (subseq *input* 0 1024) 70)))
    (loop with re
          for l in (subseq *input* 1024)
          do (str:match l ((c "," r) (set-aoc-map-ele m (list (parse-integer r) (parse-integer c)) #\#)))
             (setf re (dijkstra m '(0 0) 70))
             ;;(format t "~a~%" l)
          unless re
            return l)))
