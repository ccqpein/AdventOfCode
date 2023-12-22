(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day21.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day21_demo.input"))

(defun parse-input (input)
  (gen-aoc-map (mapcar (lambda (l) (concatenate 'list l)) input)
               :ele-coops t
               :coop-ele t))

(defun neighbour (coop)
  (loop for offset in '((0 1) (-1 0) (1 0) (0 -1))
        collect (mapcar #'+ coop offset)))

(defun in-the-range (coop r-min r-max c-min c-max)
  (and (<= r-min (car coop) r-max)
       (<= c-min (cadr coop) c-max)))

(defun flood-fill (map start step-limit &optional part2)
  (let ((r-min 0)
        (c-min 0)
        (r-max (1- (get-aoc-map-rows-len map)))
        (c-max (1- (get-aoc-map-cols-len map)))
        (table (make-hash-table :test 'equal)))
    (loop with continue-flag = t
          and next-round = (list start)
          for step upfrom 0
          
          while (and continue-flag (< step step-limit))
          do (setf continue-flag nil)
             ;;do (format t "~a~%" next-round)
          do (loop for next in next-round
                   append (loop for n in (neighbour next)
                                if (and (if part2 t (in-the-range n r-min r-max c-min c-max))
                                        (char/= #\# (if part2
                                                        (get-aoc-map-ele-extend map n)
                                                        (get-aoc-map-ele map n)))
                                                     )
                                  do (setf (gethash n table) t
                                           continue-flag t)
                                  and collect n into aa
                                finally (return aa))
                     into nn
                   finally (setf next-round (remove-duplicates nn :test #'equal)))
          finally (return (length next-round))
          )))

(defun day21 (input)
  (let* ((map (parse-input input))
         (start (car (gethash #\S (amap-ele-coops map)))))
    (flood-fill map start 64)
    )
  )

(defun get-aoc-map-ele-extend (map coop)
  (let ((new-coop (list (mod (car coop) (get-aoc-map-rows-len map))
                        (mod (cadr coop) (get-aoc-map-cols-len map)))))
    (get-aoc-map-ele map new-coop)))

;; only for real input
(defun day21-2 ()
  (let* ((map (parse-input *input*))
         (start (car (gethash #\S (amap-ele-coops map)))))
    (let*  ((b0 (flood-fill map start 65 t))
            (b1 (flood-fill map start (+ 65 131) t))
            (b2 (flood-fill map start (+ 65 131 131) t))
            (a -2)
            (a0 (+ (- b0) (* 2 b1) (- b2)))
            (a1 (+ (* 3 b0) (* -4 b1) b2))
            (a2 (* -2 b0))
            (x0 (floor (/ a0 a)))
            (x1 (floor (/ a1 a)))
            (x2 (floor (/ a2 a))))
      (format t "~a, ~a, ~a, ~a, ~a, ~a, ~a~% " a a0 a1 a2 x0 x1 x2)
      (+ (* x0 202300 202300) (* x1 202300) x2)
      )))
