(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day11.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day11_demo.input"))

(defun parse-input (input)
  (let ((input (loop for line in input collect (concatenate 'list line)))
        e-cols
        e-rows)
    (setf e-cols
          (loop for c from 0 below (length (car input))
                if (every (lambda (line)
                            (char= #\. (nth c line)))
                          input)
                  collect c))
    (setf e-rows
          (loop for r from 0 to (length input)
                if (every (lambda (c) (char= #\. c)) (nth r input))
                  collect r))
    (values input e-rows e-cols)
    ))

(defun expansion (all-pairs e-rows e-cols num)
  (loop for ((r1 c1) (r2 c2)) in all-pairs
        for new-r1 = (+ r1 (* (- num 1) (count-if (lambda (r) (> r1 r)) e-rows)))
        for new-r2 = (+ r2 (* (- num 1) (count-if (lambda (r) (> r2 r)) e-rows)))
        for new-c1 = (+ c1 (* (- num 1) (count-if (lambda (c) (> c1 c)) e-cols)))
        for new-c2 = (+ c2 (* (- num 1) (count-if (lambda (c) (> c2 c)) e-cols)))
        collect (list (list new-r1 new-c1) (list new-r2 new-c2))
        ))

(defun get-pairs (parsed-input)
  (let (all-coop)
    (setf all-coop
          (loop for r from 0 below (length parsed-input)
                append (loop for c from 0 below (length (car parsed-input))
                             if (char= (nth-nest parsed-input (list r c)) #\#)
                               collect (list r c))))
    (loop for xx on all-coop
          append (loop for y in (cdr xx)
                       collect (list (car xx) y)))))

(defun day11 (input num)
  (multiple-value-bind (parsed-input e-rows e-cols)
      (parse-input input)
    (let ((all-pairs (get-pairs parsed-input)))
      (setf all-pairs (expansion all-pairs e-rows e-cols num))
      (loop for ((r1 c1) (r2 c2)) in all-pairs
            sum (+ (abs (- r2 r1)) (abs (- c2 c1))))
      )))

;; par1
;; (day11 *input* 2)
;; part2 
;; (day11 *input* 1000000)
