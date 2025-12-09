(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day9.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day9_demo.input"))

(defun parse-input (input)
  (let (m rr cc)
    (loop for line in input
          for (c r) = (str:match line
                        ((c "," r) (list (parse-integer c) (parse-integer r))))
          maximize r into max-r
          maximize c into max-c
          finally (setf rr max-r
                        cc max-c))
    
    (setf m
          (gen-aoc-map (loop for r from 0 to rr
                             collect (loop for c from 0 to cc
                                           collect "."))
                       :ele-frequency t
                       :ele-coops t
                       :coop-ele t))
    (print-raw-map m)
    (loop for line in input
          do (str:match line
               ((c "," r)
                (set-aoc-map-ele m (list (parse-integer r) (parse-integer c)) "#"))))
    m))


(defun day9 (input)
  (let ((all-coops
          (loop for line in input
                collect (str:match line
                          ((c "," r) (list (parse-integer c) (parse-integer r)))))))
    (loop for coops on all-coops by #'cdr
          maximize (loop for x in coops
                         maximize (loop for y in (cdr coops)
                                        while y
                                        maximize (* (1+ (abs (- (first x) (first y))))
                                                    (1+ (abs (- (second x) (second y))))))))))
