(load "../../tools/tools.lisp")

;;(defparameter *input* (read-file-by-line "../inputs/day11.input"))
;;(defparameter *input-demo* (read-file-by-line "../inputs/day11_demo.input"))

(lru (stones)
     (defun blink (stones)
       (do* ((stones stones (cdr stones))
             (this (write-to-string (car stones)) (write-to-string (car stones)))
             result)
            ((string= "NIL" this) (reverse result))
         ;;(pprint this)
         (cond ((string= "0" this)
                (push 1 result))
               ((= 0 (mod (length this) 2))
                (push (parse-integer (subseq this 0 (/ (length this) 2))) result)
                (push (parse-integer (subseq this (/ (length this) 2))) result))
               (t (push (* 2024 (parse-integer this)) result))))))

(defun day11 (input times)
  (loop with count = 1
        repeat times
        do (setf input (blink input))
           (incf count))
  input)

;; '(572556 22 0 528 4679021 1 10725 2790)

(defun day11-2 (input)
  (let ((c-table (make-hash-table :test 'equal))
        (b-table (make-hash-table :test 'equal)))
    (loop for a in input
          sum (loop for b in (day11 (list a) 25)
                    if (gethash b b-table)
                      sum (gethash b b-table)
                    else
                      do (setf (gethash b b-table)
                               (loop for c in (day11 (list b) 25)
                                     if (gethash c c-table)
                                       sum (gethash c c-table)
                                     else 
                                       do (setf (gethash c c-table) (length (day11 (list c) 25)))
                                       and sum (gethash c c-table)))
                      and sum (gethash b b-table)))))
