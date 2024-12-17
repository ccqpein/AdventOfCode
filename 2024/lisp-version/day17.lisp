(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day17.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day17_demo.input"))

(defun parse-input (input)
  (let ((input (split-sequence:split-sequence-if (lambda (l) (string= l ""))
                                                 input))
        (registers (list 0 0 0)))
    (format t "input: ~a~%" input)
    (loop for l in (first input)
          do (str:match l
               (("Register A: " a) (setf (nth 0 registers) (parse-integer a)))
               (("Register B: " b) (setf (nth 1 registers) (parse-integer b)))
               (("Register C: " c) (setf (nth 2 registers) (parse-integer c)))))

    (values registers
            (str:match (car (second input))
              (("Program: " ps)
               (loop for p in (str:split "," ps)
                     collect (parse-integer p)))))))

(defun get-v (n a b c)
  (if (< n 4)
      n
      (cond ((= n 4) a)
            ((= n 5) b)
            ((= n 6) c))))

(defun opration (op pos a b c output)
  (if (= pos (length op))
      (values op pos a b c output)
      (let ((o (nth pos op))
            (n (nth (1+ pos) op)))
        (cond ((= o 0)
               (values op (+ 2 pos)
                       (floor (/ a (expt 2 (get-v n a b c)))) b c
                       output))
              ((= o 1)
               (values op (+ 2 pos)
                       a (logxor b n) c
                       output))
              ((= o 2)
               (values op (+ 2 pos)
                       a (mod (get-v n a b c) 8) c
                       output))
              ((= o 3)
               (if (= a 0)
                   (values op (+ 2 pos) a b c output)
                   (if (<= 0 n (1- (length op)))
                       (values op n
                               a b c
                               output)
                       (values op (+ 2 pos) a b c output))))
              ((= o 4)
               (values op (+ 2 pos)
                       a (logxor b c) c
                       output))
              ((= o 5)
               (values op (+ 2 pos) a b c (cons (mod (get-v n a b c) 8) output)))
              ((= o 6)
               (values op (+ 2 pos)
                       a (floor (/ a (expt 2 (get-v n a b c)))) c
                       output))
              ((= o 7)
               (values op (+ 2 pos)
                       a b (floor (/ a (expt 2 (get-v n a b c))))
                       output))
              ))))

(defun run (ops a b c)
  (let ((output '()))
    (loop with pos = 0
          while (/= pos (length ops))
          do (format t "ops: ~a, pos: ~a, comm: ~a, num: ~a, a: ~a, b: ~a, c: ~a~%output: ~a~%~%"
                     ops pos (nth pos ops) (nth (1+ pos) ops) a b c output)
             (multiple-value-setq (ops pos a b c output)
               (opration ops pos a b c output))             
          finally (return output))))

(defun day17 (input)
  (multiple-value-bind (abc ops)
      (parse-input input)
    (format t "~{~a~^,~}" (reverse (run ops (first abc) (second abc) (third abc))))))

;; (opration '(2 6) 0 0 0 9 '())
;; (opration '(5 0 5 1 5 4) 0 10 0 0 '())
;; (opration '(5 4) 0 10 0 0 '())

;; (run '(5 0 5 1 5 4) 10 0 0)
;; (run '(2 6) 0 0 9)
;; (run '(0 1 5 4 3 0) 2024 0 0)
;; (run '(1 7) 0 29 0)
;; (run '(4 0) 0 2024 43690)

;; (run '(0 1 5 4 3 0) 729 0 0)
;; (run '(2 6) 0 0 9)
;; (run '(5 0 5 1 5 4) 10 0 0)

(defun cal (a)
  (mod (logxor (logxor (mod a 8) 5) 6 (ash a (- (logxor (mod a 8) 5)))) 8))

(defun day17 (input)
  (multiple-value-bind (abc ops)
      (parse-input input)
    (declare (ignore ops))
    (format t "~{~a~^,~}" (loop with a = (first abc)
                                while (/= a 0)
                                collect (cal a) into result
                                do (setf a (floor (/ a 8)))
                                finally (return result)))))

(defun day17-2 (input)
  (let (want)
    (setf want (multiple-value-bind (abc ops)
                   (parse-input input)
                 (declare (ignore abc))
                 (reverse ops))) ;; reverse it
    (format t "want: ~a~%" want)
    (loop for last-a upfrom 1
          for (got result-a)
            = (loop with a = (list last-a)
                    for w in want
                    for good-a = (loop for aa in a
                                       when (= w (cal aa))
                                         collect aa)
                    if (null good-a)
                      return (list nil nil)
                    do (setf a (loop for aa in good-a
                                     append (loop for x from (* aa 8) below (* (1+ aa) 8) collect x)))
                    finally (progn (pprint good-a) (return (list t good-a))))                             
          when got
            return (apply #'min result-a))))
