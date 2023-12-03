(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day3.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day3_demo.input"))

(defun parse-input (input)
  (mapcar (lambda (s) (mapcar #'string (concatenate 'list s)))
          input))

(defun is-num (s)
  (member s '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
          :test 'equal))

(defun walk-point (input)
  (loop with bucket = '()
        for r from 0 below (length input)
        do (loop
             with this-num = '()
             and coops = '()
             and flag = nil
             for c from 0 below (length (car input))
             do (cond ((start-num input r c)
                       (setf this-num (append this-num (list (nth-nest input (list r c))))
                             coops (append coops (list (list r c)))
                             ))
                      ((end-num input r c)
                       (setf this-num (append this-num (list (nth-nest input (list r c))))
                             coops (append coops (list (list r c)))
                             )
                       (setf bucket (append bucket (list (list this-num coops)))
                             this-num nil
                             coops nil))
                      ((is-num (nth-nest input (list r c)))
                       (setf this-num (append this-num (list (nth-nest input (list r c))))
                             coops (append coops (list (list r c)))
                             ))
                      ;;; fuck
                      (t (when this-num
                           (setf bucket (append bucket (list (list this-num coops)))
                                 this-num nil
                                 coops nil)))))
        finally (return bucket)))

(defun day3 (input)
  (let* ((input (parse-input input))
         (bucket (walk-point input)))
    (apply #'+
           (loop with nums = '()
                 for (num coops) in bucket
                 do (loop for (r c) in coops
                          if (or (adjacent-symbol input (list (1- r) c))
                                 (adjacent-symbol input (list (1+ r) c))
                                 (adjacent-symbol input (list r (1- c)))
                                 (adjacent-symbol input (list r (1+ c)))
                                 (adjacent-symbol input (list (1+ r) (1+ c)))
                                 (adjacent-symbol input (list (1- r) (1+ c)))
                                 (adjacent-symbol input (list (1+ r) (1- c)))
                                 (adjacent-symbol input (list (1- r) (1- c))))
                            do (push (parse-integer (apply #'str:concat num)) nums)
                            and return nil
                          )
                 finally (return nums)))))

(defun day3-2 (input)
  (let* ((input (parse-input input))
         (bucket (walk-point input))
         (star-map (make-hash-table :test 'equal)))
    (loop 
      for (num coop) in bucket
      do (loop for (r c) in coop
               for v = (adjacent-* input r c)
               if v
                 do (push (parse-integer (apply #'str:concat num)) (gethash v star-map))
                 and return nil
               ))

    (loop for v being the hash-values of star-map
          when (= 2 (length v))
            sum (apply #'* v))))

(defun start-num (input r c)
  (and (is-num (nth-nest input (list r c)))
       (or (not (nth-nest input (list r (1- c))))
           (not (is-num (nth-nest input (list r (1- c))))))))

(defun end-num (input r c)
  (and (is-num (nth-nest input (list r c)))
       (not (is-num (nth-nest input (list r (1+ c)))))))

(defun adjacent-symbol (input coop)
  (let ((v (nth-nest input coop)))
    (and (string/= "." v)
         (not (is-num v))
         v)))

(defun adjacent-* (input r c)
  (cond ((string= "*" (nth-nest input (list (1+ r) c)))
         (list (1+ r) c))
        ((string= "*" (nth-nest input (list (1+ r) (1+ c))))
         (list (1+ r) (1+ c)))
        ((string= "*" (nth-nest input (list (1+ r) (1- c))))
         (list (1+ r) (1- c)))
        ((string= "*" (nth-nest input (list (1- r) c)))
         (list (1- r) c))
        ((string= "*" (nth-nest input (list (1- r) (1- c))))
         (list (1- r) (1- c)))
        ((string= "*" (nth-nest input (list (1- r) (1+ c))))
         (list (1- r) (1+ c)))
        ((string= "*" (nth-nest input (list r (1+ c))))
         (list r (1+ c)))
        ((string= "*" (nth-nest input (list r (1- c))))
         (list r (1- c)))
        (t nil)))


