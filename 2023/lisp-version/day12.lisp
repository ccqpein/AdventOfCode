(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day12.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day12_demo.input"))

(defun parse-input (input)
  (loop for line in input
        for (first second) = (split-sequence:split-sequence #\  line)
        collect (list first (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" second)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun loop-match (ll str)
  (let* ((str-len (length str))
         (place-len (- str-len (apply #'+ ll)))
         (groups (loop for n in ll
                       collect (format nil "[?#]{~a}" n))))
    ;;groups
    (loop for be in (make-between-number (1- (length groups)) place-len)
          for rre = (mapcar (lambda (n) (format nil "[?.]{~a}" n)) be)
          for z = (str:concat "^[?.]*"
                              (apply #'str:concat (zip groups rre))
                              "[?.]*$")
          sum (cl-ppcre:count-matches z str))
    ))

(defun make-between-number (places whole-number)
  (if (= 1 places)
      (loop for i from 1 to whole-number collect (list i))
      (loop for i from 1 below whole-number
            append (loop for next in (make-between-number (1- places) (- whole-number i))
                          collect (cons i next))
            )))

(defun zip (list1 list2)
  (do ((l1 list1 (cdr l1))
       (l2 list2 (cdr l2))
       result)
      ((not l2) (append result l1))
    (setf result (append result (list (car l1) (car l2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-regex (ll)
  (loop for i in ll collect (format nil "[.?][#?]{~a}[.?]" i)))

;; (defparameter *cache* (make-hash-table :test 'equal))

;; (defun scan-match (str patterns)
;;   (if (gethash (list str patterns) *cache*)
;;       (return-from scan-match (gethash (list str patterns) *cache*)))

;;   (if (not patterns)
;;       (return-from scan-match (if (str:containsp "#" str) 0 1)))
  
;;   (loop with index = 0
;;         for scan = (multiple-value-list (cl-ppcre:scan (car patterns) (subseq str index)))
;;         for m = (if (car scan) scan nil)
;;         if (and m (not (str:containsp "#" (str:substring 0 (+ index (car m)) str))))
;;           sum (scan-match (subseq str (+ index (cadr m) -1)) (cdr patterns)) into result
;;           and do (incf index (+ (car m) 1))
;;         else
;;           do (setf (gethash (list str patterns) *cache*) result)
;;           and return result))

(defun-lru scan-match (str patterns)
  (if (not patterns)
      (return-from scan-match (if (str:containsp "#" str) 0 1)))
  
  (loop with index = 0
        for scan = (multiple-value-list (cl-ppcre:scan (car patterns) (subseq str index)))
        for m = (if (car scan) scan nil)
        if (and m (not (str:containsp "#" (str:substring 0 (+ index (car m)) str))))
          sum (scan-match (subseq str (+ index (cadr m) -1)) (cdr patterns)) into result
          and do (incf index (+ (car m) 1))
        else
          do (setf (gethash (list str patterns) *cache*) result)
          and return result))

(defun day12 (input)
  (let ((input (parse-input input)))
    (loop for x in input
          sum (scan-match (str:concat "." (car x) ".") (make-regex (cadr x))))))


(defun day12-2 (input)
  (let ((input (parse-input input)))
    ;;(setf *cache* (make-hash-table :test 'equal))
    (loop for x in input
          sum (scan-match
               (str:concat "." (str:join "?" (loop repeat 5 collect (car x))) ".")
               (make-regex (loop repeat 5 append (cadr x)))))))
