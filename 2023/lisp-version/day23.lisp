(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day23.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day23_demo.input"))

(defun parse-input (input)
  (gen-aoc-map input :coop-ele t :line-op (lambda (l) (concatenate 'list l))))

(defun neighbor (map coop &optional part2)
  (remove-if
   (lambda (c) (or (aoc-map-beyond-the-range map c) (if part2 nil (char= #\# (get-aoc-map-ele map c)))))
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

(defun run (map start end &optional part2)
  (let ((queue (list (list 0 start (list start))))
        result
        (new-sort (lambda (a b) ;; new sort doean't work
                    (> (apply #'+ (mapcar #'abs (mapcar #'- a end)))
                       (apply #'+ (mapcar #'abs (mapcar #'- b end)))))))
    (loop for x = (pop queue)
          while x
          ;;do (format t "x: ~a~%" x)
          do (loop for n in (neighbor map (nth 1 x))
                   for nn = (next-step map x n part2)
                   when nn
                     do (if (equal n end)
                            (progn (push nn result)
                                   ;;(format t "find: ~a,~% in queue: ~a~%" nn queue)
                                   (format t "find: ~a~%" nn)
                                   )
                            (setf queue (append queue (list nn)))))
          do (setf queue (sort queue #'> :key #'car))
          do (setf queue (sort queue new-sort :key #'cadr))
          )
    result))

(defun day23 (input &optional part2)
  (let ((map (parse-input input)))
    (run map
         '(0 1)
         (list (1- (get-aoc-map-rows-len map))
               (- (get-aoc-map-cols-len map) 2))
         part2)))

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

(defun run2 (map start end)
  (let* ((all-special-points (find-all-special-point map))
         (table (make-hash-table :test #'equal)))

    ;; cache table making
    (setf (gethash start table) (make-hash-table :test #'equal))
    (loop for p in all-special-points do (setf (gethash p table) (make-hash-table :test #'equal)))
    
    (loop for point in all-special-points
          for start-to-piont-paths = (run map start point)
          do (loop for other-p in (remove point all-special-points :test #'equal)
                   for paths = (run map point other-p)
                   do (setf (gethash other-p (gethash point table))
                            paths))
          
          do (setf (gethash point (gethash start table))
                   start-to-piont-paths)
          )
    table
    ))
