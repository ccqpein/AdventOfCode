(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day4.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day4_demo.input"))

(defun input-to-map (input)
  (let ((m (gen-aoc-map (loop for l in input
                              collect (concatenate 'list l))
                        :ele-coops t)))
    m))

(defun star-coop-offsets ()
  '(((0 -1) (0 -2) (0 -3))
    ((-1 -1) (-2 -2) (-3 -3))
    ((-1 0) (-2 0) (-3 0))
    ((-1 1) (-2 2) (-3 3))
    ((0 1) (0 2) (0 3))
    ((1 1) (2 2) (3 3))
    ((1 0) (2 0) (3 0))
    ((1 -1) (2 -2) (3 -3))))

(defun day4 (&optional (input *input*))
  (let ((map (input-to-map input)))
    (length
     (loop for coop in (gethash #\X (amap-ele-coops map))
           append (loop with result = nil
                        for coop-offset in (star-coop-offsets)
                        for (m-o a-o s-o) = coop-offset
                        do (let ((m (mapcar #'+ coop m-o))
                                 (a (mapcar #'+ coop a-o))
                                 (s (mapcar #'+ coop s-o)))
                             (if (and (equal (get-aoc-map-ele map m) #\M)
                                      (equal (get-aoc-map-ele map a) #\A)
                                      (equal (get-aoc-map-ele map s) #\S))
                                 (push (list m a s) result)))
                        finally (return result))))))

(defun x-coop-offset ()
  ;; M S M S
  '(((-1 -1) (1 1) (1 -1) (-1 1))
    ((1 1) (-1 -1) (1 -1) (-1 1))
    ((-1 -1) (1 1) (-1 1) (1 -1))
    ((1 1) (-1 -1) (-1 1) (1 -1))))

(defun day4-2 (&optional (input *input*))
  (let* ((map (input-to-map input)))
    (loop for coop in (gethash #\A (amap-ele-coops map))
          sum (loop for (a-o b-o c-o d-o) in (x-coop-offset)
                    when (let ((a (mapcar #'+ coop a-o))
                               (b (mapcar #'+ coop b-o))
                               (c (mapcar #'+ coop c-o))
                               (d (mapcar #'+ coop d-o)))
                           (and (equal (get-aoc-map-ele map a) #\M)
                                (equal (get-aoc-map-ele map b) #\S)
                                (equal (get-aoc-map-ele map c) #\M)
                                (equal (get-aoc-map-ele map d) #\S)))
                      return 1
                    finally (return 0)))))
