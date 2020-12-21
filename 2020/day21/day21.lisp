(load "../tools.lisp")

(defun parse-a-line (line)
  (let* ((cuted-line (str:split " (" line))
         (foods (car cuted-line)))
    (values (str:split " " foods)
            (str:split ", "(str:substring 9 -1 (cadr cuted-line))))))

(defun update-set-with-line (foods allergens table)
  (dolist (a allergens)
    (let ((ff (gethash a table)))
      ;;(format t "ff: ~a with allergen ~a~%" ff a)
      (if ff
          (setf (gethash a table) (intersection foods ff :test #'string=))
          (setf (gethash a table) foods)))))

(defun part1 ()
  (let* ((input (read-file-by-line "./day21.input"))
         (table (make-hash-table :test 'equal))
         (all-foods '())
         all-allergens
         )
    
    (loop
      for line in input
      do (multiple-value-bind (f as)
             (parse-a-line line)
           ;;(format t "~a; ~a~%" f as)
           (update-set-with-line f as table)
           (setf all-foods (append f all-foods))))

    ;;(return-from part1 table)
    
    (setf all-allergens
          (loop
            for v being the hash-values of table
            collect v into cache
            finally (return (remove-duplicates (alexandria:flatten cache)
                                               :test #'string=))))
    
    ;;(format t "all allergens ~a~%" all-allergens)
    
    (print
     (length (loop
               for f in all-foods
               unless (member f all-allergens :test #'string=)
                 collect f
               ;;and do (print f)
               )))
    table
    ))

(defun part2 ()
  (let ((table (part1)))
    (loop
      for k being the hash-keys 
        using (hash-value v) of table
      do (format t "key: ~a, value: ~a~%" k v))))

;;key: sesame, value: (chpdjkf)
;;key: dairy, value: (ltbj)
;;key: peanuts, value: (jxbnb)
;;key: fish, value: (nrfmm)
;;key: soy, value: (zzkq)
;;key: shellfish, value: (jtqt)
;;key: wheat, value: (jqnhd)
;;key: nuts, value: (pvhcsn)
