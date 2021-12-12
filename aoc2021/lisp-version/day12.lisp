(load "../../2020/tools.lisp")

(defun day12 (path)
  (let ((content (read-file-by-line path))
        (relation-table (make-hash-table :test 'equal)))
    (loop
      for l in content
      for (a b) = (str:split "-" l)
      do (if (gethash a relation-table)
             (set-insert (gethash a relation-table) b)
             (setf (gethash a relation-table) (make-hash-set-from-list (list b))))
      do (if (gethash b relation-table)
             (set-insert (gethash b relation-table) a)
             (setf (gethash b relation-table) (make-hash-set-from-list (list a)))))

    (format t "part1:~a~%"
            (loop
              for next in (set-to-list (gethash "start" relation-table))
              sum (helper relation-table (make-hash-set-from-list '("start")) next)))
    
    (format t "part2:~a~%"
            (loop
              for next in (set-to-list (gethash "start" relation-table))
              sum (helper2 relation-table
                           (make-hash-set-from-list '("start"))
                           (make-hash-table :test 'equal)
                           t
                           next)))))

(defun helper (table already this)
  (if (string= this "end")
      1
      (let ((all-next (gethash this table)))
        (loop for next in (set-to-list all-next)
              sum (if (set-get already next)
                      0
                      (helper table (if (str:upcasep this)
                                        (copy-hash-set already)
                                        (let ((s (copy-hash-set already)))
                                          (set-insert s this)
                                          s))
                              next))))))

(defun helper2 (table already time-count flag this)
  (if (string= this "end")
      1
      (let ((all-next (gethash this table)))
        (loop for next in (set-to-list all-next)
              for next-flag = flag
              for next-times-count = (alexandria:copy-hash-table time-count)

              for next-set = (if (str:upcasep this)
                                 (copy-hash-set already)
                                 (let ((a (copy-hash-set already)))
                                   (if next-flag
                                       (if (= 1 (gethash this next-times-count 0))
                                           (progn
                                             (loop
                                               for k being the hash-keys of next-times-count
                                               do (set-insert a k))
                                             (setf next-flag nil))
                                           (setf (gethash this next-times-count) 1))
                                       (set-insert a this))
                                   a))
              sum (if (set-get next-set next)
                      0
                      (helper2 table next-set next-times-count next-flag next))))))
