(load "../../2020/tools.lisp")

(defun parse-input (inputs)
  (let ((enhance-str (mapcar (lambda (c) (if (char= c #\#) "1" "0"))
                             (concatenate 'list (nth 0 inputs))))
        (image (make-hash-set)))
    (setf inputs (cddr inputs))
    (loop for x from 0 below (length inputs)
          do (loop for y from 0 below (length (car inputs))
                   when (char= (elt (nth x inputs) y) #\#)
                     do (set-insert image (list x y))))
    (values enhance-str image)
    ))

(defun edge (image-set)
  (loop for (x y) in (set-to-list image-set)
        maximize x into x2
        minimize x into x1
        maximize y into y2
        minimize y into y1
        finally (return (values (list x1 y1) (list x2 y2)))))

(defun day20 (path step)
  (let ((inputs (read-file-by-line path))
        (null-point "0"))
    (multiple-value-bind (enhance-str image)
        (parse-input inputs)

      (dotimes (a step (set-count image))
        (multiple-value-bind (left-upper right-downer)
            (edge image)
          (let ((new-image (make-hash-set)))
            (loop for x from (1- (car left-upper)) to (1+ (car right-downer))
                  do (loop for y from (1- (cadr left-upper)) to (1+ (cadr right-downer))
                           for index-enhance =
                                             (loop for (xx yy) in (list (list (1- x) (1- y))
                                                                        (list (1- x) y)
                                                                        (list (1- x) (1+ y))
                                                                        (list x (1- y))
                                                                        (list x y)
                                                                        (list x (1+ y))
                                                                        (list (1+ x) (1- y))
                                                                        (list (1+ x) y)
                                                                        (list (1+ x) (1+ y)))
                                                   collect (if (or (< xx (car left-upper))
                                                                   (> xx (car right-downer))
                                                                   (< yy (cadr left-upper))
                                                                   (> yy (cadr right-downer)))
                                                               null-point
                                                               (if (set-get image (list xx yy))
                                                                   "1"
                                                                   "0")))
                           for this-light = (nth (parse-integer (apply #'str:concat index-enhance) :radix 2) enhance-str)
                           when (string= "1" this-light)
                             do (set-insert new-image (list x y))))
            (setf null-point (if (string= null-point "1") "0" "1")
                  image new-image))
          )))))
