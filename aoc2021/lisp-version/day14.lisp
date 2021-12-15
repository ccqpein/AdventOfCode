(load "../../2020/tools.lisp")

(defun day14 (path step)
  (let* ((content (read-file-by-line path))
         (template (car content))
         (all-map (let ((table (make-hash-table :test 'equal)))
                    (dolist (l (cddr content) table)
                      (let ((a (str:split " -> " l)))
                        (setf (gethash (concatenate 'list (car a)) table)
                              (elt (cadr a) 0)))))))
    (let ((counts (make-hash-table :test 'equal))
          (pairs (make-hash-table :test 'equal)))
      (loop for c across template do (setf (gethash c counts) (1+ (gethash c counts 0))))

      (loop
        with chars-temp = (concatenate 'list template)
        for ind from 0 to (- (length template) 2)
        for v = (gethash (list (nth ind chars-temp)
                               (nth (1+ ind) chars-temp))
                         pairs 0)
        do (setf (gethash (list (nth ind chars-temp)
                                (nth (1+ ind) chars-temp))
                          pairs)
                 (1+ v)))

      (loop repeat step
            do (loop with new-pairs = (make-hash-table :test 'equal)
                     for (a b) being the hash-keys of pairs
                       using (hash-value count)
                     for c = (gethash (list a b) all-map)
                     do (setf (gethash c counts) (+ count (gethash c counts 0))
                              (gethash (list a c) new-pairs) (+ count (gethash (list a c) new-pairs 0))
                              (gethash (list c b) new-pairs) (+ count (gethash (list c b) new-pairs 0)))
                     finally (setf pairs new-pairs)))

      (let ((all-values (loop for v being the hash-values of counts collect v)))
        (- (apply #'max all-values)
           (apply #'min all-values))))))
