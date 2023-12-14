(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day14.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day14_demo.input"))

(defun parse-input (input)
  (gen-aoc-map input :line-op (lambda (l) (concatenate 'list l))))

(defun split-sql (l e)
  (loop with cache = '()
        and result = '()
        for c in l
        if (char/= c e)
          do (push c cache)
        else
          do (if cache (push (reverse cache) result))
          and do (push (list e) result)
          and do (setf cache nil)
        finally (progn
                  (if cache (push (reverse cache) result))
                  (return (reverse result)))))

(defun move-one-line (line)
  (loop for g in (split-sql line #\#)
        if (/= 1 (length g))
          append (sort g (lambda (a b) (> (char-code a) (char-code b)))) into new-l
        else
          append g into new-l
        finally (return new-l)))

(defun move-north (map)
  (gen-aoc-map
   (loop for col in (get-aoc-map-cols map (loop for c from 0 below (get-aoc-map-cols-len map) collect c))
         collect (move-one-line col))
   :is-cols t))

(defun move-south (map)
  (gen-aoc-map
   (loop for col in (get-aoc-map-cols map (loop for c from 0 below (get-aoc-map-cols-len map) collect c))
         collect (reverse (move-one-line (reverse col))))
   :is-cols t))

(defun move-east (map)
  (gen-aoc-map
   (loop for row in (get-aoc-map-rows map (loop for r from 0 below (get-aoc-map-rows-len map) collect r))
         collect (reverse (move-one-line (reverse row))))))

(defun move-west (map)
  (gen-aoc-map
   (loop for row in (get-aoc-map-rows map (loop for r from 0 below (get-aoc-map-rows-len map) collect r))
         collect (move-one-line row))))

(defun cal-col-2 (map)
  (loop for col in (get-aoc-map-cols map (loop for c from 0 below (get-aoc-map-cols-len map) collect c))
        sum (loop for r-num downfrom (get-aoc-map-rows-len map) to 1
                  for c in col
                  if (char= #\O c)
                    sum r-num)))

(defun day14 (input)
  (let ((input (parse-input input)))
    (cal-col-2 (move-north input))))

(defun day14-2 (input)
  (let ((input (parse-input input))
        (a-circle (lambda (m) (move-east (move-south (move-west (move-north m))))))
        (cache (make-hash-table :test 'equal)))
    (loop for i from 1 to 200
          do (setf input (funcall a-circle input))
          if (gethash (amap-raw-map input) cache)
            do (format t "~a: ~a~%" i (gethash (amap-raw-map input) cache))
          else
            do (setf (gethash (amap-raw-map input) cache) (list i (cal-col-2 input))))))

;;(mod (- 1000000000 136) 22)
