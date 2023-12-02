(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day2.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day2_demo.input"))

(defun parse-input (input)
  (mapcar
   (lambda (line)
     (loop
       for a in (str:split "; " (cadr (str:split ": " line)))
       collect (mapcar (lambda (x) (str:split " " x))
                       (str:split ", " a))))
   input))

(defun parse-input-regex (input)
  (loop for line in input
        collect (loop
                  for pick in (cl-ppcre:all-matches-as-strings "(\\d+)\\s+[^,;]+" line)
                  append (str:split " " pick))))

(defun day2 (&optional (input *input-demo*))
  (let ((inner-input (parse-input-regex input)))
    (loop 
      for ind upfrom 1
      for game in inner-input
      for cache = (make-hash-table :test 'equal)
      do (loop for (num color) on game by #'cddr
               do (if (>= (parse-integer num) (gethash color cache 0))
                      (setf (gethash color cache) (parse-integer num))))
      sum (if (and (<= (gethash "red" cache 0) 12)
                   (<= (gethash "green" cache 0) 13)
                   (<= (gethash "blue" cache 0) 14))
              ind
              0))))

(defun day2-2 (&optional (input *input-demo*))
  (let ((inner-input (parse-input-regex input)))
    (loop 
      for game in inner-input
      for cache = (make-hash-table :test 'equal)
      do (loop for (num color) on game by #'cddr
               do (if (>= (parse-integer num) (gethash color cache 0))
                      (setf (gethash color cache) (parse-integer num))))
      sum (* (gethash "red" cache 0)
             (gethash "green" cache 0)
             (gethash "blue" cache 0)))))


;; (defun day2 (&optional (input *input-demo*))
;;   (let ((inner-input (parse-input input)))
;;     (loop 
;;       for ind upfrom 1
;;       for game in inner-input
;;       for cache = (make-hash-table :test 'equal)
;;       do (loop for round in game
;;                do (loop for pick in round
;;                         do (if (>= (parse-integer (car pick)) (gethash (cadr pick) cache 0))
;;                                (setf (gethash (cadr pick) cache) (parse-integer (car pick))))))
;;       sum (if (and (<= (gethash "red" cache 0) 12)
;;                    (<= (gethash "green" cache 0) 13)
;;                    (<= (gethash "blue" cache 0) 14))
;;               ind
;;               0))
;;     ))

;; (defun day2-2 (&optional (input *input-demo*))
;;   (let ((inner-input (parse-input input)))
;;     (loop 
;;       for game in inner-input
;;       for cache = (make-hash-table :test 'equal)
;;       do (loop for round in game
;;                do (loop for pick in round
;;                         do (if (>= (parse-integer (car pick)) (gethash (cadr pick) cache 0))
;;                                (setf (gethash (cadr pick) cache) (parse-integer (car pick))))))
;;       sum (* (gethash "red" cache 0)
;;              (gethash "green" cache 0)
;;              (gethash "blue" cache 0)))))
