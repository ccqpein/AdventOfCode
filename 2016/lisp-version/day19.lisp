(load "../../tools/tools.lisp")

(defparameter *input* 3014603)

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

(defun day19 (&optional (input *input*))
  (let ((ll (loop for i from 1 to input
                  collect (list i 1))))
    (loop while (> (length ll) 1)
            do (setf ll (merge-list ll))
               ;;do (format t "~a~%" ll)
          finally (return (first ll)))))
