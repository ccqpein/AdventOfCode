(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day17.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day17_demo.input"))

(defun parse-input (input)
  (gen-aoc-map input
               :ele-frequency t
               :ele-coops t
               :coop-ele t
               :line-op (lambda (l) (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d" l)))))

(defun sum-values (map start dir dis)
  (loop
    with offset = (case dir
                    (#\U '(-1 0))
                    (#\D '(1 0))
                    (#\L '(0 -1))
                    (#\R '(0 1)))
    and this = start
    for d from 1 to dis
    do (setf this (mapcar #'+ this offset))
    if (get-aoc-map-ele map this)
      collect this into all-pass
      and sum (get-aoc-map-ele map this) into v
    else
      return (values nil nil)
    finally (return (list v all-pass))))

(defun allow-dirs (dir)
  (case dir
    (#\U '(#\L #\R))
    (#\D '(#\L #\R))
    (#\L '(#\U #\D))
    (#\R '(#\U #\D))))

(defun gen-end (map dis dir start-coop)
  (let* ((offset (case dir
                   (#\U '(-1 0))
                   (#\D '(1 0))
                   (#\L '(0 -1))
                   (#\R '(0 1))))
         (new-coop (mapcar (lambda (a b) (+ a (* dis b))) start-coop offset))
         (sum-v (sum-values map start-coop dir dis)))
    (if (not (aoc-map-beyond-the-range map new-coop))
        (list new-coop
              (car sum-v)
              (cadr sum-v )
              dir
              dis
              )
        nil)))

(defun record-all (map &optional row-limit col-limit)
  (let ((table (make-hash-table :test 'equal)))
    (loop for row-n from 0 below (or row-limit (get-aoc-map-rows-len map))
          do (loop for col-n from 0 below (or col-limit (get-aoc-map-cols-len map))
                   for ends =
                            (loop for dis from 1 to 3
                                  append (loop for dir in '(#\R #\L #\U #\D)
                                               for end = (gen-end map dis dir (list row-n col-n))
                                               if end
                                                 collect end))
                   do (setf (gethash (list row-n col-n) table) (sort ends #'< :key #'cadr))))
    table))

;;(record-all (parse-input *input-demo*) 3 3)

(defun check-visited (this-coop visited)
  (loop for p in (nth 2 this-coop)
        if (member p visited :test 'equal)
          do (return-from check-visited nil)
        finally (return t)))

(defun run (map visited this-coop end allow-dirs table cache)
  (if (equal this-coop end)
      (progn
        ;;(format t "~a~%" visited)
        (return-from run nil)))
  (let ((before-v (gethash visited cache 0)))
    (loop for d in allow-dirs
          do (loop for next in (gethash this-coop table)
                   if (and (char= (nth 3 next) d) (check-visited next visited))
                     do (setf (gethash (append visited (nth 2 next)) cache) (+ before-v (nth 1 next)))
                     and do (run map (append visited (nth 2 next))
                                 (car next) end
                                 (allow-dirs d) table cache)
                   )))
  cache)

(defun all-next (this-coop table dir)
  (loop for d in (allow-dirs dir)
        append (loop for next in (gethash this-coop table)
                 if (char= (nth 3 next) d)
                   collect next)))

(defun run-2 (map start end)
  (let ((all-visited (make-hash-table :test 'equal))
        (queue (list (list start 0 #\R '((0 0)))
                     (list start 0 #\D '((0 0))))) ;;((this-coop loss-from-start dir all-visited))
        (all-records (record-all map)))
    (loop while queue
          do (let* ((first (pop queue))
                    (all-next (all-next (car first) all-records (nth 2 first)))
                    )
               ;;(format t "first:~a~%" first)
               ;;(format t "all-next:~a~%" all-next)
               (if (equal end (car first)) (return-from run-2 (list (cadr first) (nth 3 first))))
               (loop for next in all-next
                     if (check-visited next (nth 3 first))
                       do (push (list (car next)
                                      (+ (cadr first) (cadr next))
                                      (nth 3 next)
                                      (append (nth 3 first) (nth 2 next))
                                      )
                                queue)))
          do (setf queue (sort queue #'< :key #'cadr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; all fucking trash upper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-3 (map min max end)
  (let ((queue '((0 0 0 0 1 1 ((0 0)))
                 (0 0 0 1 0 1 ((0 0)))
                 )))
    (labels ((add-to-queue (map loss x y dx dy step path)
               (if (not (aoc-map-beyond-the-range map (list x y)))
                   (progn (push (list (+ loss (get-aoc-map-ele map (list x y)))
                                      x y dx dy step (append path (list (list x y))))
                                queue)
                          (setf queue (sort queue #'< :key #'car))))))

      (let ((visited (make-hash-table :test 'equal)))
        (loop while queue
              for (loss x y dx dy step path) = (pop queue)
              ;;do (format t "~a ~a ~a~%" loss x y)
              if (equal (list x y) end)
                do (return-from run-3 loss)

              if (not (gethash (list x y dx dy step) visited))
                do (setf (gethash (list x y dx dy step) visited) loss)
                and do (progn
                         (if (< step max)
                             (add-to-queue map loss (+ x dx) (+ y dy) dx dy (1+ step) path))
                         (if (>= step min)
                             (if (zerop dy)
                                 (progn (add-to-queue map loss x (1+ y) 0 1 1 path)
                                        (add-to-queue map loss x (1- y) 0 -1 1 path))
                                 (progn (add-to-queue map loss (1+ x) y 1 0 1 path)
                                        (add-to-queue map loss (1- x) y -1 0 1 path)))))
              ))
      )))

(defun day17 (input &optional part2)
  (let ((map (parse-input input)))
    (if part2
        (run-3 map 4 10 (list (1- (get-aoc-map-rows-len map))
                              (1- (get-aoc-map-cols-len map))))
        (run-3 map 1 3 (list (1- (get-aoc-map-rows-len map))
                             (1- (get-aoc-map-cols-len map)))))))
