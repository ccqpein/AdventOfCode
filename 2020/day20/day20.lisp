(load "../tools.lisp")

(defun chunk-input (ll-str) ;; list of all input
  (loop
    with result ='()
    and cache ='()
    for s in ll-str
    if (string/= "" s)
      do (push s cache)
    else
      do (push (reverse cache) result)
      and do (setf cache '())
    finally (progn (push (reverse cache) result)
                   (return (reverse result)))))

(defun tag-id (chuck-strs)
  "((id (content)))"
  (mapcar (lambda (ll)
            (cons (car ll) (list (cdr ll)))
            )
          chuck-strs))

(defstruct tile
  id
  ori-tile
  borads ;; from 12 clock wise
  )

(defun get-bit (str)
  (parse-integer (format nil "~{~a~}"
                         (loop
                           with cache ='()
                           for i across str
                           if (char= #\# i)
                             do (push 1 cache)
                           else
                             do (push 0 cache)
                           finally (return (reverse cache)))
                         )
                 :radix 2))

(defun create-tile (chuck)
  (let* ((content (cadr chuck))
         (m (make-matrix-from-aoc content))
         )
    (make-tile :id (car chuck)
               :ori-tile m
               :borads (list (get-bit (car content))
                             (get-bit (concatenate 'string
                                                   (loop for l in content
                                                         collect (elt l 9))))
                             (get-bit (nth 9 content))
                             (get-bit (concatenate 'string
                                                   (loop for l in content
                                                         collect (elt l 0))))))))

;;; not finish this function
(defun find-four-coners (tiles)
  (loop
    with cache = (make-list 9 :initial-element (list))
    for ind from 0 below 9
    for tile = (nth ind tiles)
    for dims = (tile-borads tile)
    do (loop
         for ii from 0 below 9
         for this-dims = (tile-borads (nth ii tiles))
         when (/= ii ind)
           do (loop for b from 0 below 4
                    if (member (nth b dims) this-dims)
                      do (pushnew b (nth ind cache)))
         )
    finally (return cache)))

(defun day20-p1 ()
  (let* ((input (read-file-by-line "./day20.input"))
         (a (chunk-input input))
         (b (tag-id a))
         (tiles (mapcar #'create-tile b))
         )
    (find-four-coners tiles)))
