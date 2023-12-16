(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day16.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day16_demo.input"))

(defun parse-input (input)
  (gen-aoc-map input
               :line-op (lambda (l) (concatenate 'list l))
               :ele-coops t
               :coop-ele t))

(defun up-and-down (coop map)
  (loop for co in (list (list #\U (1- (car coop)) (cadr coop))
                        (list #\D (1+ (car coop)) (cadr coop)))
        if (not (aoc-map-beyond-the-range map (cdr co)))
          collect co))

(defun left-and-right (coop map)
  (loop for co in (list (list #\L (car coop) (1- (cadr coop)))
                        (list #\R (car coop) (1+ (cadr coop))))
        if (not (aoc-map-beyond-the-range map (cdr co)))
          collect co))

(defun check-range (map coop)
  (if (not (aoc-map-beyond-the-range map coop)) coop nil))

(defun next-step (coop dir map)
  (let ((this (gethash coop (amap-coop-ele map))))
    (ccase this
      (#\|
       (cond ((or (char= dir #\L) (char= dir #\R))
              (up-and-down coop map))
             ((char= dir #\U) (list (cons dir (check-range map (list (1- (car coop)) (cadr coop))))))
             ((char= dir #\D) (list (cons dir (check-range map (list (1+ (car coop)) (cadr coop))))))))
      (#\/
       (cond 
         ((char= dir #\R) (list (cons #\U (check-range map (list (1- (car coop)) (cadr coop))))))
         ((char= dir #\L) (list (cons #\D (check-range map (list (1+ (car coop)) (cadr coop))))))
         ((char= dir #\U) (list (cons #\R (check-range map (list (car coop) (1+ (cadr coop)))))))
         ((char= dir #\D) (list (cons #\L (check-range map (list (car coop) (1- (cadr coop)))))))))
      (#\\
       (cond 
         ((char= dir #\R) (list (cons #\D (check-range map (list (1+ (car coop)) (cadr coop))))))
         ((char= dir #\L) (list (cons #\U (check-range map (list (1- (car coop)) (cadr coop))))))
         ((char= dir #\U) (list (cons #\L (check-range map (list (car coop) (1- (cadr coop)))))))
         ((char= dir #\D) (list (cons #\R (check-range map (list (car coop) (1+ (cadr coop)))))))))
      (#\-
       (cond ((or (char= dir #\U) (char= dir #\D))
              (left-and-right coop map))
             ((char= dir #\L) (list (cons dir (check-range map (list (car coop) (1- (cadr coop)))))))
             ((char= dir #\R) (list (cons dir (check-range map (list (car coop) (1+ (cadr coop)))))))))
      (#\.
       (cond 
         ((char= dir #\R) (list (cons dir (check-range map (list (car coop) (1+ (cadr coop)))))))
         ((char= dir #\L) (list (cons dir (check-range map (list (car coop) (1- (cadr coop)))))))
         ((char= dir #\U) (list (cons dir (check-range map (list (1- (car coop)) (cadr coop))))))
         ((char= dir #\D) (list (cons dir (check-range map (list (1+ (car coop)) (cadr coop)))))))))
    ))

(defun run (map coop dir result-map)
  (do ((next-coop coop))
      ((not next-coop) t)

    (if (member dir (gethash next-coop result-map))
        (return-from run t))
    
    (push dir (gethash next-coop result-map))
    
    (let ((a (next-step next-coop dir map)))
      (if (> (length a) 1)
          (run map (cdr (cadr a)) (car (cadr a)) result-map))
      (setf dir (car (car a))
            next-coop (cdr (car a))))))

(defun all-starts (map)
  (append (loop for x in (get-aoc-map-row map 0)
                for n upfrom 0
                if (char= x #\.)
                  collect (list #\D 0 n))
          (loop for x in (get-aoc-map-col map 0)
                for n upfrom 0
                if (char= x #\.)
                  collect (list #\R n 0))
          (loop for x in (get-aoc-map-row map (1- (get-aoc-map-rows-len map)))
                for n upfrom 0
                if (char= x #\.)
                  collect (list #\U (1- (get-aoc-map-rows-len map)) n))
          (loop for x in (get-aoc-map-col map (1- (get-aoc-map-cols-len map)))
                for n upfrom 0
                if (char= x #\.)
                  collect (list #\L n (1- (get-aoc-map-rows-len map))))))

(defun day16 (input &optional part2)
  (let* ((map (parse-input input))
         (ss (if part2 (all-starts map) '((#\R 0 0)))))
    (loop
      for result-map = (make-hash-table :test 'equal)
      for s in ss
      do (run map (cdr s) (car s) result-map)
      maximize (length (alexandria:hash-table-keys result-map)))))

;;(= (day16 *input*) 6994)
;;(= (day16 *input* t) 7488)
