(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day3.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day3_demo.input"))

(lru (sl left)
     (defun get-the-largest-2 (sl left) ;; => return ((...)*)
       (if (< (length sl) left) (return-from get-the-largest-2 nil))
       (if (= 1 left) (return-from get-the-largest-2 (list (list (apply #'max sl)))))
       (if (= (length sl) left) (return-from get-the-largest-2 (list sl)))
       
       (list (car
              (loop for key from 9 downto 1
                    append (loop for next-round in (find-key-start sl key)
                                 append (loop for next-round-res in (get-the-largest-2 (cdr next-round) (1- left))
                                              collect (cons key next-round-res))))))))

(defun find-key-start (sl ele)
  (loop for ll on sl by #'cdr
        while ll
        if (= ele (car ll))
          collect ll))

(defun day3 (input &optional part2)
  (loop for line in input
        sum (let ((s (mapcar #'parse-integer
                             (mapcar #'string
                                     (concatenate 'list line)))))
              (if part2
                  (parse-integer (format nil "~{~a~}" (car (get-the-largest-2 s 12))))
                  (parse-integer (format nil "~{~a~}" (car (get-the-largest-2 s 2))))))))
