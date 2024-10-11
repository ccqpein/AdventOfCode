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
  (let* ((lines (loop for y from 0 to (* 2 (second to))
                      collect (loop for x from 0 to (* 2 (first to))
                                    collect (if (if-open x y input) "." "#"))))
         (m (gen-aoc-map lines :coop-ele t)))
    ;;(print-raw-map m)

    ;; make graph
    ;; *quiz x y is actually y x for this map*
    (loop with g = (make-graph :graph-type 'undirected)
          for x from 0 below (get-aoc-map-rows-len m)
          do (loop for y from 0 below (get-aoc-map-cols-len m)
                   do (format t "~a ~a ~a~%" x y (get-aoc-map-ele m (list x y)))
                   when (string= (get-aoc-map-ele m (list x y)) ".")
                     do (loop for offset in '((1 0) (-1 0) (0 1) (0 -1))
                              for e = (get-aoc-map-ele m (mapcar #'+ offset (list x y)))
                              ;;do (print e)
                              when (and e (string= e "."))
                                do (insert-graph-node g (list x y) (mapcar #'+ offset (list x y)) 1)))
          finally (progn ;;(format t "~a~%" (get-all-nodes-of-id g '(4 6)))
                         (return (dijkstra g from (reverse to) ;; row col reverse
                                           ))))
    ))

