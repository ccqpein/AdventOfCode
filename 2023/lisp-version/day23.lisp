(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day23.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day23_demo.input"))

(defun parse-input (input)
  (gen-aoc-map input :coop-ele t :line-op (lambda (l) (concatenate 'list l))))

(defun neighbor (map coop &optional part2)
  (remove-if
   (lambda (c) (or (aoc-map-beyond-the-range map c)
                   (if part2 nil (char= #\# (get-aoc-map-ele map c)))))
   (loop for offset in '((0 1) (0 -1) (1 0) (-1 0)) collect (mapcar #'+ coop offset))))

(defun downhill-check (map next-coop)
  (let ((nt (get-aoc-map-ele map next-coop)))
    (cond ((char= nt #\^) (mapcar #'+ '(-1 0) next-coop))
          ((char= nt #\>) (mapcar #'+ '(0 1) next-coop))
          ((char= nt #\<) (mapcar #'+ '(0 -1) next-coop))
          ((char= nt #\v) (mapcar #'+ '(1 0) next-coop))
          (t next-coop))))

(defun next-step (map this next &optional part2)
  (let (next-tail)
    (setf next-tail
          (if part2
              (list next)
              (if (equal (downhill-check map next) next)
                  (list next)
                  (list next (downhill-check map next))
                  )))
    ;;(format t "~a~%" (nth 2 this))
    (if (intersection next-tail (nth 2 this) :test 'equal)
        nil
        (list (+ (length next-tail) (nth 0 this))
              (car (last next-tail))
              (append (nth 2 this) next-tail)))))

(defun run (map start ends &optional part2)
  (let ((queue (list (list 0 start ())))
        result)
    (loop for x = (pop queue)
          while x
          do (loop for n in (neighbor map (nth 1 x))
                   for nn = (next-step map x n part2)
                   when (and nn (not (equal n start)))
                     do (if (member n ends :test 'equal)
                            (push nn result)
                            (setf queue (append queue (list nn)))))
          do (setf queue (sort queue #'> :key #'car))
          )
    result))

(defun day23 (input)
  (let ((map (parse-input input)))
    (car (sort (run map
                    '(0 1)
                    (list (list (1- (get-aoc-map-rows-len map))
                                (- (get-aoc-map-cols-len map) 2))))
               #'> :key #'car))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun if-special (map coop)
  (and (neighbor map coop t)
       (char= #\. (get-aoc-map-ele map coop))
       (every (lambda (c) (let ((e (get-aoc-map-ele map c)))
                            (and (char/= #\# e)
                                 (char/= #\. e)
                                 )))
              (neighbor map coop t))))

(defun find-all-special-point (map)
  (loop for coop being the hash-keys of (amap-coop-ele map)
          using (hash-value e)
        if (if-special map coop)
          collect coop))

(defun day23-2 (input)
  (let* ((map (parse-input input))
         (end (list (1- (get-aoc-map-rows-len map))
                    (- (get-aoc-map-cols-len map) 2)))
         (all-special-points (append '((0 1))
                                     (find-all-special-point map)
                                     (list end)
                                     ))
         (special-point-table (make-hash-table :test 'equal)))

    (loop for p in all-special-points
          for v = (run map p all-special-points t)
          do (setf (gethash p special-point-table)
                   (loop for vv in v collect (list (nth 0 vv) (nth 1 vv) (list-to-set (nth 2 vv))))))

    ;;(format t "~a~%" (alexandria:hash-table-alist special-point-table))
    ;;(format t "done table~%")
    (car
     (sort (loop with queue = (let ((q (make-instance 'cl-heap:fibonacci-heap
                                                      :sort-fun #'>
                                                      :key #'car)))
                                (cl-heap:add-to-heap q (list 0 '(0 1) (make-hash-set)))
                                q)
                 and result = '()
                 
                 for x = (cl-heap:pop-heap queue)
                 while x
                 do (let ((next-jump (gethash (nth 1 x) special-point-table)))

                      (setf next-jump (remove-if (lambda (n) (set-intersection-p (nth 2 x) (nth 2 n))) next-jump))

                      (setf next-jump (mapcar
                                       (lambda (n) (list (+ (nth 0 n) (nth 0 x))
                                                         (nth 1 n)
                                                         (set-union (nth 2 x)
                                                                    (nth 2 n))))
                                       next-jump))

                      (loop for j in next-jump
                            if (equal end (nth 1 j))
                              do (push j result)
                            else
                              do (cl-heap:add-to-heap queue j)))
                 finally (return result))
           #'> :key #'car))))
