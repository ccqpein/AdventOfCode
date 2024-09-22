(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day9.input"))

;;(declaim (optimize (speed 3)))

;; concat string too slow
;; (defun handler-line (line)
;;   (str:match line
;;     ((head "\\(" code "\\)" tail)
;;      (format t "~a ~a ~a~%" head code tail)
;;      (multiple-value-bind (count times)
;;          (parse-code code)
;;        (apply #'str:concat
;;               (alexandria:flatten
;;                (list head
;;                      (handler-line
;;                       (apply #'str:concat
;;                              (append
;;                               (loop repeat times
;;                                     collect (str:substring 0 count tail))
;;                               (list (subseq tail count))))))))))
;;     (t line)))

(defun handler-line (line)
  (str:match line
    ((head "\\(" code "\\)" tail)
     ;;(format t "~a ~a ~a~%" head code tail)
     (multiple-value-bind (count times)
         (parse-code code)
       (+ (length head)
          (* count times) ;; no recursive
          (handler-line
           (subseq tail count)))))
    (t (length line))))

(defun handler-line-2 (line)
  (str:match line
    ((head "\\(" code "\\)" tail)
     ;;(format t "~a ~a ~a~%" head code tail)
     (multiple-value-bind (count times)
         (parse-code code)
       (+ (length head)
          (* times (handler-line-2 (str:substring 0 count tail))) ;; recursive
          ;;(* count times)
          (handler-line-2
           (subseq tail count)))))
    (t (length line))))

(defun parse-code (code)
  (str:match code
    ((count "x" times)
     (values (parse-integer count) (parse-integer times)))))

(defun day9 (&optional part2)
  (if part2
      (handler-line-2 (car *input*))
      (handler-line (car *input*))))

