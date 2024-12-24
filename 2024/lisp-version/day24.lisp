(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day24.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day24_demo.input"))

(defun f-and (x y) (if (and (= 1 x) (= 1 y)) 1 0))
(defun f-or (x y) (if (not (and (= 0 x) (= 0 y))) 1 0))

(defun parse-input (input)
  (let ((input (split-sequence:split-sequence-if (lambda (s) (string= s "")) input)))
    (values (loop with vs = (make-hash-table :test 'equal)
                  for line in (first input)
                  do (str:match line
                       ((v ": " n) (setf (gethash v vs) (if (string= n "1") 1 0))))
                  finally (return vs))
            (loop for line in (second input)
                  collect (str:match line
                            ((a " AND " b " -> " to) (list a b to #'f-and))
                            ((a " OR " b " -> " to) (list a b to #'f-or))
                            ((a " XOR " b " -> " to) (list a b to #'logxor)))))))

(defun cal-zs (zs-table)
  (let ((zs (loop for n upfrom 0
                  for z = (gethash n zs-table)
                  while z
                  collect z)))
    (loop for z in zs for exp upfrom 0 sum (* z (expt 2 exp)))))

(defun day24 (input)
  (multiple-value-bind (vs comms)
      (parse-input input)
    (do ((comms comms)
         (flag t)
         (zs-table (make-hash-table)))
        ((not flag)
         (cal-zs zs-table))
      (setf flag nil)
      (loop for (a b c d) in comms
            when (and (gethash a vs) (gethash b vs))
              do (let ((re (apply d (list (gethash a vs) (gethash b vs)))))
                   (unless (gethash c vs)
                     (setf (gethash c vs) re
                           flag t)
                     (when (str:starts-with? "z" c)
                       (setf (gethash (parse-integer (subseq c 1)) zs-table) re))))))))


(defun parse-input-2 (input)
  (let ((input (split-sequence:split-sequence-if (lambda (s) (string= s "")) input))
        (table (make-hash-table :test 'equal)))
    (loop for line in (second input)
          do (str:match line
               ((a " AND " b " -> " to) (setf (gethash to table)
                                              (cons 'and (sort (list a b) #'string<))))
               ((a " OR " b " -> " to) (setf (gethash to table)
                                             (cons 'or (sort (list a b) #'string<))))
               ((a " XOR " b " -> " to) (setf (gethash to table)
                                              (cons 'xor (sort (list a b) #'string<))))))
    table))

(defun get-to-x-y (table key)
  (if (or (str:starts-with-p "x" key)
          (str:starts-with-p "y" key))
      key
      (let ((v (gethash key table)))
        (unless v (return-from get-to-x-y nil) )
        ;;(format t "convert ~a to ~a~%" key v)
        (list (first v)
              (get-to-x-y table (second v))
              (get-to-x-y table (third v))))))

(defun show-all-zs (table)
  (loop for i upfrom 0
        for z-key = (format nil "z~2,'0d" i)
        for vv = (gethash z-key table)
        while vv
        collect (list i (gethash z-key table))))

(defun get-z (table z)
  (let ((vv (gethash z table)))
    (list (first vv)
                  (get-to-x-y table (second vv))
                  (get-to-x-y table (third vv)))))

;;(z06 fhc)
;;(z11 qhj)
;;(hqk z35)

(defun checking-z (zl n)
  (if (eq (first zl) 'xor)
      (or (and (checking-xor-x-y (second zl) n)
               (checking-c (third zl) n))
          (and (checking-xor-x-y (third zl) n)
               (checking-c (second zl) n)))
      nil))

(defun checking-c (cl n)
  ;;(format t "~a~% ~a~%" n cl)
  (if (eq (first cl) 'or)
      (or (and (checking-and-x-y (second cl) (1- n))
               (second-part-of-c (third cl) (1- n)))
          (and (checking-and-x-y (third cl) (1- n))
               (second-part-of-c (second cl) (1- n))))
      (equal cl '(and "x00" "y00"))))

(defun checking-and-x-y (l n)
  (equal l (list 'and (format nil "x~2,'0d" n) (format nil "y~2,'0d" n))))

(defun second-part-of-c (l n)
  (or (and (checking-xor-x-y (second l) n)
           (checking-c (third l) n))
      (and (checking-xor-x-y (third l) n)
           (checking-c (second l) n))))

(defun checking-xor-x-y (l n)
  (equal l (list 'xor (format nil "x~2,'0d" n) (format nil "y~2,'0d" n))))

(defun swap (table node0 node1)
  (let ((a (gethash node0 table)))
    (setf (gethash node0 table) (gethash node1 table)
          (gethash node1 table) a)))

(defun one-more-check ()
  (let ((table (parse-input-2 *input*)))
    (swap table "z06" "fhc")
    (swap table "z11" "qhj")
    (swap table "z35" "hqk")
    (swap table "ggt" "mwh") ;; the last one
    (loop for i from 1 to 44
          for z-key = (format nil "z~2,'0d" i)
          do (format t "check ~a~%" z-key)
             (if (not (checking-z (get-z table z-key) i)) (format t "wrong z: ~a~%" i)))))

;; (let ((results '("z06" "fhc" "z11" "qhj" "z35" "hqk" "ggt" "mwh")))
;;   (format t "~{~a~^,~}" (sort results #'string<)))

;;(checking-c (second (get-z (parse-input-2 *input*) "z08")) 8)

;;(checking-z (get-z (parse-input-2 *input*) "z23") 23)

