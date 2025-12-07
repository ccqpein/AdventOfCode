(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day7.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day7_demo.input"))

;; (defun parse-input (input)
;;   (values (loop for n in (concatenate 'list (car input))
;;                 if (char= #\. n)
;;                   collect nil
;;                 else
;;                   collect t)
;;           (loop for line in (cdr input)
;;                 collect (loop for n in (concatenate 'list line)
;;                               if (char= #\. n)
;;                                 collect nil
;;                               else
;;                                 collect t))))

;; (defun day7 (input)
;;   (multiple-value-bind (start rest-inds)
;;       (parse-input input)
;;     (do* ((this start)
;;           (rest rest-inds (cdr rest))
;;           (next (car rest) (car rest))
;;           (res 0))
;;          ((not rest) res)
;;       ;;(print this)
;;       (loop with cache = (loop for n from 0 below (length this) collect nil)
;;             for x in this
;;             for y in next
;;             for ind from 0
;;             if (and x y)
;;               do (if (/= ind 0) (setf (nth (1- ind) cache) t))
;;                  (if (/= ind (1- (length this))) (setf (nth (1+ ind) cache) t))
;;                  (incf res)
;;             else if x
;;                    do (setf (nth ind cache) t)
;;             finally (setf this cache)))))

(defun parse-input (input)
  (values (loop for n in (concatenate 'list (car input))
                if (char= #\. n)
                  collect 0
                else
                  collect 1)
          (loop for line in (cdr input)
                collect (loop for n in (concatenate 'list line)
                              if (char= #\. n)
                                collect nil
                              else
                                collect t))))

(defun day7 (input)
  (multiple-value-bind (start rest-inds)
      (parse-input input)
    (do* ((this start)
          (rest rest-inds (cdr rest))
          (next (car rest) (car rest))
          (res 0))
         ((not rest) (values res (apply #'+ this))) ;; <= par1 and part2
      (loop with cache = (loop for n from 0 below (length this) collect 0)
            for x in this
            for y in next
            for ind from 0
            if (and (> x 0) y)
              do (if (/= ind 0) (incf (nth (1- ind) cache) x))
                 (if (/= ind (1- (length this))) (incf (nth (1+ ind) cache) x))
                 (incf res)
            else if (> x 0)
                   do (incf (nth ind cache) x)
            finally (setf this cache)))))
