(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day24.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day24_demo.input"))

(defun parse-input (input)
  (loop for h in input
        collect (mapcar (lambda (s) (mapcar #'parse-integer (str:split ", " s)))
                        (str:split " @ " h))))

(defun cal-a-and-x (h)
  (let ((va (car h))
        (args (cadr h)))
    (let ((a (/ (nth 1 args) (nth 0 args)))
          c)
      (setf c (- (nth 1 va) (* a (nth 0 va))))
      (list a c
            (car args) ;; original a for past check
            ))))

(defun cal-cross (h1ac h2ac)
  (let (x y)
    (if (= 0 (- (nth 0 h2ac) (nth 0 h1ac))) (return-from cal-cross nil))
    (setf x (/ (- (- (nth 1 h2ac) (nth 1 h1ac)))
               (- (nth 0 h2ac) (nth 0 h1ac))))
    (setf y (+ (* (car h1ac) x) (cadr h1ac)))
    (list x y)))

(defun pastp (point-check ac xy)
  (let ((delta-x (- (nth 0 point-check) (nth 0 xy))))
    (if (> (* delta-x (nth 2 ac)) 0)
        nil
        t)))

(defun day24 (input)
  (let ((input (parse-input input))
        (hails-table (make-hash-table :test 'equal)))

    (loop for ind from 0 below (length input)
          do (setf (gethash ind hails-table) (cal-a-and-x (nth ind input))))
    
    (loop for inda from 0 below (1- (length input))
          for a = (gethash inda hails-table)
          for point-a = (subseq (car (nth inda input)) 0 2)
          
          sum (loop for indb from (1+ inda) below (length input)
                    for b = (gethash indb hails-table)
                    for point-b = (subseq (car (nth indb input)) 0 2)
                    
                    for ccc = (cal-cross a b)
                    if ccc
                      when (and (<= 200000000000000 (nth 0 ccc) 400000000000000)
                                (<= 200000000000000 (nth 1 ccc) 400000000000000)
                                (not (pastp ccc a point-a))
                                (not (pastp ccc b point-b)))
                        sum 1
                    ))
    ))

;; only has part1
