(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day12.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day12_demo.input"))

;; (defun around-coops (coop)
;;   (loop for offset in '((-1 0) (0 -1) (0 1) (1 0))
;;         collect (mapcar #'+ offset coop)))

;; (defun walk-chunk (map coop visit)
;;   (if (gethash coop visit) (return-from walk-chunk (values 0 '() '() '())))
;;   (do* ((inside-visit (make-hash-table :test 'equal))
;;         (ele (get-aoc-map-ele map coop))
;;         (next (list coop) (cdr next))
;;         (this (car next) (car next))
;;         (edge (make-hash-table :test 'equal))
;;         touched
;;         (area 0)
;;         (perimeter 0))
;;        ((not this)
;;         (format t "~a ~a~%" area perimeter)
;;         (values (* area perimeter)
;;                 touched
;;                 (alexandria:hash-table-keys inside-visit)))
;;     (unless (gethash this inside-visit)
;;       (if (aoc-map-beyond-the-range map this)
;;           (progn (incf perimeter)
;;                  ;;(setf (gethash this edge) t)
;;                  ;;(format t "edge: ~a~%" this)
;;                  )
;;           (if (equal ele (get-aoc-map-ele map this))
;;               (progn (incf area)
;;                      (setf (gethash this inside-visit) t
;;                            next (append next (around-coops this))))
;;               (progn (incf perimeter)
;;                      ;;(setf (gethash this edge) t)
;;                      ;;(format t "edge: ~a~%" this)
;;                      (push this touched)
;;                      )
;;               )))))

;; (let ((m (parse-input *input-demo*))
;;       (visit (make-hash-table :test 'equal)))
;;   (format t "~a~%" (multiple-value-list (walk-chunk m '(0 0) visit)))
;;   (format t "~a~%" (alexandria:hash-table-alist visit)))

;; (defun walk-map (map)
;;   (let ((visit (make-hash-table :test 'equal)))
;;     (do* ((next '((0 0)) (cdr next))
;;           (coop (car next) (car next))
;;           (answer 0))
;;          ((not coop) answer)
;;       (multiple-value-bind (fence next-r visited)
;;           (walk-chunk map coop visit)
;;         (dolist (v visited) (setf (gethash v visit) t))
;;         (setf next (append next next-r))
;;         (incf answer fence)
;;         ;;(format t "len next:~a~%" (length next))
;;         ))))

;; (let ((m (parse-input *input-demo*)))
;;   (walk-map m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-input (input)
  (gen-aoc-map (loop for line in input
                     collect (concatenate 'list line))
               :ele-coops t
               :coop-ele t))

(defun around-coops-2 (coop)
  (loop for (offset dir) in '(((-1 0) "U") ((0 -1) "L") ((0 1) "R") ((1 0) "D"))
        collect (list (mapcar #'+ offset coop) dir)))

(defun walk-chunk-2 (map coop visit)
  (if (gethash coop visit) (return-from walk-chunk-2 (values 0 '() '() '())))
  (do* ((inside-visit (make-hash-table :test 'equal))
        (ele (get-aoc-map-ele map coop))
        (next (list (list coop "O")) (cdr next))
        (this (car next) (car next))
        touched
        (area 0)
        (edges '()))
       ((not this)
        (values area edges touched (alexandria:hash-table-keys inside-visit)))
    (unless (gethash (first this) inside-visit)
      (if (aoc-map-beyond-the-range map (first this))
          (push this edges)
          (if (equal ele (get-aoc-map-ele map (first this)))
              (progn (incf area)
                     (setf (gethash (first this) inside-visit) t
                           next (append next (around-coops-2 (first this)))))
              (progn (push this edges)
                     (push (first this) touched)))))))

(defun group-it (l)
  (if (null l) 0
      (let ((l (sort l #'<)))
        (1+ (loop with x = (first l)
                  for y in (cdr l)
                  if (> (- y x) 1)
                    count 1
                  do (setf x y))))))

(defun cal-sides (edges)
  (if (null edges) (return-from cal-sides 0))
  (let ((min-r (apply #'min (mapcar (lambda (e) (first (first e))) edges)))
        (max-r (apply #'max (mapcar (lambda (e) (first (first e))) edges)))
        (min-c (apply #'min (mapcar (lambda (e) (second (first e))) edges)))
        (max-c (apply #'max (mapcar (lambda (e) (second (first e))) edges))))
    (+ (loop for r from min-r to max-r
             for cs = (loop for x in edges
                            when (and (= (first (first x)) r)
                                      (string= (second x) "U"))
                              collect (second (first x)))
             sum (group-it cs))
       (loop for r from min-r to max-r
             for cs = (loop for x in edges
                            when (and (= (first (first x)) r)
                                      (string= (second x) "D"))
                              collect (second (first x)))
             sum (group-it cs))
       (loop for c from min-c to max-c
             for cs = (loop for x in edges
                            when (and (= (second (first x)) c)
                                      (string= (second x) "L"))
                              collect (first (first x)))
             sum (group-it cs))
       (loop for c from min-c to max-c
             for cs = (loop for x in edges
                            when (and (= (second (first x)) c)
                                      (string= (second x) "R"))
                              collect (first (first x)))
             sum (group-it cs)))))

(defun walk-map-2 (part2 map)
  (let ((visit (make-hash-table :test 'equal)))
    (do* ((next '((0 0)) (cdr next))
          (coop (car next) (car next))
          (answer 0))
         ((not coop) answer)
      (multiple-value-bind (area edges next-r visited)
          (walk-chunk-2 map coop visit)
        (dolist (v visited) (setf (gethash v visit) t))
        (setf next (append next next-r))
        (if part2
            (incf answer (* area (cal-sides edges)))
            (incf answer (* area (length edges))))))))

(defun day12 (&optional part2 (input *input*))
  (let ((m (parse-input input)))
    (walk-map-2 part2 m)))
