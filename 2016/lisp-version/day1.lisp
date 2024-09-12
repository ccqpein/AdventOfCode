(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day1.input"))
;;(defparameter *input-demo* (read-file-by-line "../inputs/day1_demo.input"))

(defun parse-input (line)
  (str:split ", " line))

(defun next-ori (ori c)
  (cond ((equal (list ori c)
                '("U" "R"))
         '(1 0))
        ((equal (list ori c)
                '("U" "L"))
         '(-1 0))
        ((equal (list ori c)
                '("D" "R"))
         '(-1 0))
        ((equal (list ori c)
                '("D" "L"))
         '(1 0))
        ((equal (list ori c)
                '("R" "R"))
         '(0 -1))
        ((equal (list ori c)
                '("R" "L"))
         '(0 1))
        ((equal (list ori c)
                '("L" "L"))
         '(0 -1))
        ((equal (list ori c)
                '("L" "R"))
         '(0 1))))

(defun next-step (ori coord c)
  (str:match c
    (("R" n) (list
              (next-o ori "R")
              (mapcar #'+
                      coord
                      (mapcar (lambda (x) (* x (parse-integer n))) (next-ori ori "R")))))
    (("L" n) (list
              (next-o ori "L")
              (mapcar #'+
                      coord
                      (mapcar (lambda (x) (* x (parse-integer n))) (next-ori ori "L")))))))

(defun next-o (ori c)
  (cond ((equal (list ori c)
                '("U" "R"))
         "R")
        ((equal (list ori c)
                '("U" "L"))
         "L")
        ((equal (list ori c)
                '("D" "R"))
         "L")
        ((equal (list ori c)
                '("D" "L"))
         "R")
        ((equal (list ori c)
                '("R" "R"))
         "D")
        ((equal (list ori c)
                '("R" "L"))
         "U")
        ((equal (list ori c)
                '("L" "L"))
         "D")
        ((equal (list ori c)
                '("L" "R"))
         "U")))

(defun day1 (&key (input *input*))
  (let ((input (parse-input (car input))))
    (loop with a = (str:match (car input)
                     (("R" n) (list "R" (list (parse-integer n) 0)))
                     (("L" n) (list "L" (list (- (parse-integer n)) 0))))
          for c in (cdr input)
          do (format t "~a~%" a)
          do (setf a (next-step (first a) (second a) c))
          finally (return (apply #'+ (mapcar #'abs (second a)))))))

;;;;;;;;;;;;;;; AI write below
(defun orientation (p q r)
  (let* ((val (- (* (- (cadr q) (cadr p))
                    (- (car r) (car q)))
                 (* (- (car q) (car p))
                    (- (cadr r) (cadr q))))))
    (cond ((= val 0) 0)                 ; collinear
          ((> val 0) 1)                 ; clockwise
          (t 2))))       ; counterclockwise

(defun on-segment (p q r)
  (and (<= (min (car p) (car q)) (car r) (max (car p) (car q)))
       (<= (min (cadr p) (cadr q)) (cadr r) (max (cadr p) (cadr q)))))

(defun intersection-point (p1 q1 p2 q2)
  (let* ((a1 (- (cadr q1) (cadr p1)))
         (b1 (- (car p1) (car q1)))
         (c1 (+ (* a1 (car p1)) (* b1 (cadr p1))))
         (a2 (- (cadr q2) (cadr p2)))
         (b2 (- (car p2) (car q2)))
         (c2 (+ (* a2 (car p2)) (* b2 (cadr p2))))
         (det (- (* a1 b2) (* a2 b1))))
    (if (not (= det 0))
        ;; lines are not parallel, compute intersection point
        (list (/ (- (* b2 c1) (* b1 c2)) det)
              (/ (- (* a1 c2) (* a2 c1)) det))
        nil))) ; lines are parallel

(defun do-intersect (p1 q1 p2 q2)
  (let* ((o1 (orientation p1 q1 p2))
         (o2 (orientation p1 q1 q2))
         (o3 (orientation p2 q2 p1))
         (o4 (orientation p2 q2 q1))
        (intersectp nil)
         (point nil))
    (if (or (and (not (= o1 o2)) (not (= o3 o4)))
            (and (= o1 0) (on-segment p1 q1 p2))
            (and (= o2 0) (on-segment p1 q1 q2))
            (and (= o3 0) (on-segment p2 q2 p1))
            (and (= o4 0) (on-segment p2 q2 q1)))
      (progn
        (setq intersectp t)
        (setq point (intersection-point p1 q1 p2 q2))))
    (values intersectp point)))

;; Example usage
;; (let ((p1 (list x1 y1))
;;       (q1 (list x2 y2))
;;       (p2 (list x3 y3))
;;       (q2 (list x4 y4)))
;;   (multiple-value-bind (intersectp point) (do-intersect p1 q1 p2 q2)
;;     (if intersectp
;;         (format t "The lines intersect at point: ~A~%" point)
;;         (format t "The lines do not intersect.~%")))
;;         )

;;;;;;;;;;;;;;; AI write done

(defun day1-2 (&key (input *input*))
  (let ((input (parse-input (car input))))
    (loop with a = (str:match (car input)
                     (("R" n) (list "R" (list (parse-integer n) 0)))
                     (("L" n) (list "L" (list (- (parse-integer n)) 0))))
          with bucket = (list (list '(0 0) (second a)))
          for c in (cdr input)
          ;;do (format t "~a~%" a)
          for aa = (next-step (first a) (second a) c)
          do (push (list (second a) (second aa)) bucket)
          do (setf a aa)
          do (loop with init = (car bucket)
                   for x in (cdr bucket)
                   do (multiple-value-bind (intersectp point)
                          (do-intersect (first init) (second init) (first x) (second x))
                        ;;(format t "first line: ~a, second line: ~a~%" init x)
                        (when (and intersectp (not (member point (append init x) :test 'equal)))
                          (format t "The lines intersect at point: ~A~%" point)
                          (return-from day1-2 (apply #'+ (mapcar #'abs point))))))
             ;;finally (return (apply #'+ (mapcar #'abs (second a))))
          )))
