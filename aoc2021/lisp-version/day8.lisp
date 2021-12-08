(load "../../2020/tools.lisp")

(defun parse-line (line)
  (let ((a (str:split #\  line)))
    (values-list (split-sequence:split-sequence-if
                  (lambda (s) (string= s "|"))
                  a))))

(defun part1 (path)
  (let ((content (read-file-by-line path)))
    (loop
      with first and second
      for line in content
      do (multiple-value-setq (first second) (parse-line line))
      sum (loop for word in second
                count (let ((count (length word)))
                        (or (= 7 count)
                            (= 4 count)
                            (= 2 count)
                            (= 3 count)))))))

(defun parse-first-part (commands)
  (let ((commands-sets (mapcar #'make-hash-set-from-list
                               (loop for comm in commands
                                     collect (concatenate 'list comm)))))
    (let* ((seven (find 3 commands-sets :key #'set-count))
           (one (find 2 commands-sets :key #'set-count))
           (four (find 4 commands-sets :key #'set-count))
           (eight (find 7 commands-sets :key #'set-count))
           
           (six (find-if (lambda (s)
                               (and (= 1 (length (hash-set-difference eight s)))
                                    (= 1 (length (hash-set-difference seven s)))))
                         commands-sets))
           (nine (find-if (lambda (s)
                            (and (= 6 (set-count s))
                                 (= 1 (length (hash-set-difference s (set-union seven four))))))
                          commands-sets))
           (zero (find-if (lambda (s)
                            (and (= 1 (length (hash-set-difference eight s)))
                                 (/= 0 (length (hash-set-difference s nine)))
                                 (= 0 (length (hash-set-difference seven s)))))
                          commands-sets))

           (two (find-if (lambda (s)
                           (and (= 2 (length (hash-set-difference eight s)))
                                (/= 0 (length (hash-set-difference s nine)))
                                (/= 0 (length (hash-set-difference one s)))))
                         commands-sets))
           
           (five (make-hash-set-from-list (hash-set-difference
                                           six
                                           (make-hash-set-from-list (hash-set-difference eight nine)))))

           (three (set-union (set-union one
                                        (make-hash-set-from-list (hash-set-difference nine four)))
                             (make-hash-set-from-list (hash-set-difference eight zero))))
           )
      (values zero one two three four five six seven eight nine))))

(defun cal-value (second first-part-set)
  (loop for c in (mapcar #'make-hash-set-from-list
                         (loop for a in second
                               collect (concatenate 'list a)))
        collect (digit-char (position-if (lambda (s) (set-same s c)) first-part-set))))

(defun part2 (path)
  (let ((content (read-file-by-line path)))
    (loop
      with first and second
      with num-list
      for line in content
      do (multiple-value-setq (first second) (parse-line line))
      do (setf num-list (multiple-value-list (parse-first-part first)))
      sum (parse-integer (str:concat (cal-value second num-list)))
      )))
