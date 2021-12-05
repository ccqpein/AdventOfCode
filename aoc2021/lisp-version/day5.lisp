(load "../../2020/tools.lisp")

(defun parse-line (line)
  (mapcar (lambda (a) (mapcar #'parse-integer
                              (str:split "," a)))
          (str:split " -> " line)))

(defun coord-picker (line)
  "line = ((a b) (c d))"
  (values (caar line) (cadar line) (caadr line) (cadadr line)))

(defun part1 (path)
  (let ((content (mapcar #'parse-line
                         (read-file-by-line path)))
        (result-table (make-hash-table :test 'equal)))
    (do* ((rest content (cdr rest))
          (line (car rest) (car rest)))
         ((not line) (loop for v being the hash-value of result-table
                           count (>= v 2)))
      (multiple-value-bind (a b c d)
          (coord-picker line)
        (cond ((= a c)
               (loop for y from (min b d) to (max b d) do (incf (gethash (list a y) result-table 0))))
              ((= b d)
               (loop for x from (min a c) to (max a c) do (incf (gethash (list x b) result-table 0)))))
        ))))

(defun part2 (path)
  (let ((content (mapcar #'parse-line
                         (read-file-by-line path)))
        (result-table (make-hash-table :test 'equal)))
    (do* ((rest content (cdr rest))
          (line (car rest) (car rest)))
         ((not line) (loop for v being the hash-value of result-table
                           count (>= v 2)))
      (multiple-value-bind (a b c d)
          (coord-picker line)
        (cond ((= a c)
               (loop for y from (min b d) to (max b d) do (incf (gethash (list a y) result-table 0))))
              ((= b d)
               (loop for x from (min a c) to (max a c) do (incf (gethash (list x b) result-table 0))))
              ((= (abs (- a c))
                  (abs (- b d)))
               (let ((x-offset (if (< a c) 1 -1))
                     (y-offset (if (< b d) 1 -1)))
                 (loop for step from 0 to (abs (- a c))
                       do (incf (gethash (list (+ a (* step x-offset))
                                               (+ b (* step y-offset)))
                                         result-table 0))))))
        ))))

(defmacro handlers-gen (line &rest hls)
  `(multiple-value-bind (a b c d)
       (coord-picker ,line)
     (cond ,@(loop for hl in hls collect hl))))

(defun part2-macro (path)
  (let ((content (mapcar #'parse-line
                         (read-file-by-line path)))
        (result-table (make-hash-table :test 'equal)))
    (do* ((rest content (cdr rest))
          (line (car rest) (car rest)))
         ((not line) (loop for v being the hash-value of result-table
                           count (>= v 2)))
      (handlers-gen line
                    ((= a c)
                     (loop for y from (min b d) to (max b d) do (incf (gethash (list a y) result-table 0))))
                    ((= b d)
                     (loop for x from (min a c) to (max a c) do (incf (gethash (list x b) result-table 0))))
                    ((= (abs (- a c))
                        (abs (- b d)))
                     (let ((x-offset (if (< a c) 1 -1))
                           (y-offset (if (< b d) 1 -1)))
                       (loop for step from 0 to (abs (- a c))
                             do (incf (gethash (list (+ a (* step x-offset))
                                                     (+ b (* step y-offset)))
                                               result-table 0)))))
                    ))))

(defun reuse-part (line result-table &rest handlers)
  (multiple-value-bind (a b c d)
      (coord-picker line)
    (cond ((= a c)
           (loop for y from (min b d) to (max b d) do (incf (gethash (list a y) result-table 0))))
          ((= b d)
           (loop for x from (min a c) to (max a c) do (incf (gethash (list x b) result-table 0)))))
    (dolist (hl handlers)
      (funcall hl a b c d result-table))))

(defun part1-reuse (path)
  (let ((content (mapcar #'parse-line
                         (read-file-by-line path)))
        (result-table (make-hash-table :test 'equal)))
    (do* ((rest content (cdr rest))
          (line (car rest) (car rest)))
         ((not line) (loop for v being the hash-value of result-table
                           count (>= v 2)))
      (reuse-part line result-table))))

(defun part2-reuse (path)
  (let ((content (mapcar #'parse-line
                         (read-file-by-line path)))
        (result-table (make-hash-table :test 'equal)))
    (do* ((rest content (cdr rest))
          (line (car rest) (car rest)))
         ((not line) (loop for v being the hash-value of result-table
                           count (>= v 2)))
      (reuse-part line result-table
                  (lambda (a b c d record)
                    (if (= (abs (- a c))
                           (abs (- b d)))
                        (let ((x-offset (if (< a c) 1 -1))
                              (y-offset (if (< b d) 1 -1)))
                          (loop for step from 0 to (abs (- a c))
                                do (incf (gethash (list (+ a (* step x-offset))
                                                        (+ b (* step y-offset)))
                                                  result-table 0))))))))))
