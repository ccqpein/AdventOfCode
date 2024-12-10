(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day10.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day10_demo.input"))

(defun parse-input (input)
  (gen-aoc-map (loop for line in input
                     collect (mapcar #'parse-integer (mapcar #'string (concatenate 'list line))))
               :ele-coops t
               :coop-ele t))

(defun around (m coop)
  (loop for offset in '((0 1) (0 -1)
                        (1 0) (-1 0))
        unless (aoc-map-beyond-the-range m (mapcar #'+ offset coop))
          collect (mapcar #'+ offset coop)))

(defun around-one-larger (m coop e)
  (loop for c in (around m coop)
        when (= (get-aoc-map-ele m c) (1+ e))
          collect c))

(defun one-step (map coop)
  (let ((e (get-aoc-map-ele map coop)))
    (if (= e 9) (return-from one-step (list (list coop))))
    (let ((nexts (around-one-larger map coop e)))
      (if (not nexts) (return-from one-step nil))
      (loop for next in nexts
            append (loop for trail in (one-step map next)
                         collect (cons coop trail))))))

(defun climbing (map)
  (loop for coop in (gethash 0 (amap-ele-coops map))
        collect (one-step map coop)))

(defun day10 (part2 &optional (input *input*))
  (let ((m (parse-input input))
        all-ends)
    (if (not part2)        
        (progn (setf all-ends
                     (loop for trail in (climbing m)
                           collect (remove-duplicates
                                    (loop for l in trail
                                          collect (car (last l)))
                                    :test 'equal)))
               (loop for l in all-ends sum (length l)))
        (loop for trail in (climbing m)
              sum (length trail)))))
