(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day8.input"))
;;(defparameter *input-demo* (read-file-by-line "../inputs/day8_demo.input"))

(defun handler-line (map line)
  (str:match line
    (("rect " col "x" row)
     (loop for c from 0 below (parse-integer col)
           do (loop for r from 0 below (parse-integer row)
                    do (set-aoc-map-ele map (list r c) "#"))))
    (("rotate column " "x=" col " by " col-offset)
     (let ((new-col (rotate-list (get-aoc-map-col map (parse-integer col))
                                 (parse-integer col-offset))))
       (set-aoc-map-col map (parse-integer col) new-col)))
    (("rotate row " "y=" row " by " row-offset)
     (let ((new-row (rotate-list (get-aoc-map-row map (parse-integer row))
                                 (parse-integer row-offset))))
       (set-aoc-map-row map (parse-integer row) new-row)))
    (t (error "no match"))))

(defun part1 (&optional (input *input*))
  (let ((map (gen-aoc-map
              (make-list 6
                         :initial-element
                         (make-list 50 :initial-element "."))
              :ele-frequency t)))
    ;;(print-raw-map map)
    (loop for line in input
          do (handler-line map line)
          finally (return (gethash "#" (amap-ele-frequency map))))
    ))

(defun part2 (&optional (input *input*))
  (let ((map (gen-aoc-map
              (make-list 6
                         :initial-element
                         (make-list 50 :initial-element "."))
              :ele-frequency t)))
    (loop for line in input
          do (handler-line map line)
          finally (print-raw-map map))))

;; (let ((map (gen-aoc-map
;;             (make-list 3 :initial-element
;;                        (make-list 7 :initial-element ".")))))
;;   (handler-line map "rect 3x2")
;;   (handler-line map "rotate column x=1 by 1")
;;   (handler-line map "rotate row y=0 by 4")
;;   (handler-line map "rotate column x=1 by 1")
;;   ;;(set-aoc-map-row map 2 (make-list 50 :initial-element "#"))
;;   ;;(set-aoc-map-col map 2 (make-list 6 :initial-element "#"))
;;   (print-raw-map map))
