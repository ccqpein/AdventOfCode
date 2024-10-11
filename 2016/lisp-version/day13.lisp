(load "../../tools/tools.lisp")

(defparameter *input* 1352)

;; copy from internet
(defun mvp-binary-from-decimal (n r)
    (if (zerop n)
    r
    (multiple-value-bind (a b)
        (floor n 2)
        (mvp-binary-from-decimal a (cons b r)))))

(defun binary-from-decimal (n)
    (if (and (numberp n) (plusp n))
    (mvp-binary-from-decimal n '())
    (if (eql n 0) '(0) nil)))

(defun if-open (x y f)
  (zerop (mod (count 1 (binary-from-decimal
                        (+ (* (+ x y) (+ x y))
                           (* 3 x)
                           y
                           f)))
              2)))

(defun part1 (from to &optional (input *input*))
  (let* ((lines (loop for y from 0 to (* 2 (first to))
                      collect (loop for x from 0 to (* 2 (second to))
                                    collect (if (if-open x y input) "." "#"))))
         (m (gen-aoc-map lines :coop-ele t)))
    (print-raw-map m)

    ;; make graph
    (loop with g = (make-graph)
          for x from 0 to (get-aoc-map-rows-len m)
          do (loop for y from 0 to (get-aoc-map-cols-len m)
                   do (loop for offset in '((1 0) (-1 0) (0 1) (0 -1))
                            for e = (get-aoc-map-ele m (mapcar #'+ offset (list x y)))
                            ;;do (print e)
                            when (and e (string= e "."))
                              do (insert-graph-node g (list x y) (mapcar #'+ offset (list x y)) 1)))
          finally (progn (print (alexandria:hash-table-plist (agraph-table g)))
                         (return (dijkstra g from to))))
    ))

