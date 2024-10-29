(load "../../tools/tools.lisp")

(defparameter *input* 3014603)

;;(declaim (optimize (speed 3)))

(defun merge-two-ele (a b)
  (if b
      (list (first a) (+ (second a) (second b)))
      a))

(defun merge-list (ll)
  (loop for (a b) on ll by #'cddr
        if b
          collect (merge-two-ele a b) into result
        else
          do (setf result (cons a result))
        finally (return result)))

(defun merge-two-ele-2 (ll)
  (let ((offset (floor (/ (list-length ll) 2))))
    (append (subseq ll 1 offset)
            (subseq ll (1+ offset))
            (list (merge-two-ele (nth 0 ll) (nth (+ 0 offset) ll))))
    ))

(defun merge-list-2 (ll)
  (loop while (> (list-length ll) 1)
        do (setf ll (merge-two-ele-2 ll))
        finally (return ll)))

(defun merge-list-3 (ll)
  (do* ((offset (floor (/ (list-length ll) 2)))
        (this-ind 0 (1+ this-ind))
        (suppose-ind (+ offset this-ind) (+ offset this-ind))
        (visited '())
        (result)
        (terminate nil))
       (terminate (append (loop for i from (1- this-ind) below (list-length ll)
                                if (not (member i visited))
                                  collect (nth i ll))
                          (reverse result)))
    ;;(format t "this-ind: ~a, suppose-ind: ~a, visited: ~a~%" this-ind suppose-ind visited)
    (let ((new-ind (+ suppose-ind (count-if (lambda (i) (< i suppose-ind)) visited)
                      (if (and (> (list-length visited) 0)
                               (evenp (list-length ll)))
                          -1
                          0))))
      (if (or (>= new-ind (length ll)) (member this-ind visited))
          (setf terminate t)
          (progn (push (merge-two-ele (nth this-ind ll) (nth new-ind ll)) result)
                 (push new-ind visited))))))

(defun day19 (&optional (input *input*))
  (let ((ll (loop for i from 1 to input
                  collect (list i 1))))
    (loop while (> (length ll) 1)
          do (setf ll (merge-list ll))
             ;;do (format t "~a~%" ll)
          finally (return (first ll)))))

(defun day19-2 (&optional (input *input*))
  (let ((ll (loop for i from 1 to input
                  collect (list i 1))))
    (loop while (> (length ll) 1)
          do (setf ll (merge-list-3 ll))
          finally (return (first ll)))))
