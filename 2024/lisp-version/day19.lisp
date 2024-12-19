(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day19.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day19_demo.input"))

(defun parse-input (input)
  (let ((input (split-sequence:split-sequence-if (lambda (l) (string= "" l)) input)))
    (values (mapcar (lambda (w) (concatenate 'list w)) (str:split ", " (first (first input))))
            (mapcar (lambda (w) (concatenate 'list w)) (second input)))))

(defun can-be-2 (sl set cache)
  (if (gethash sl cache) (return-from can-be-2 (values t (gethash sl cache))))
  (if (null sl)
      (values t 1)
      (loop with result = 0
            for i from (length sl) downto 1
            when (set-get set (subseq sl 0 i))
              do (multiple-value-bind (x v)
                     (can-be-2 (subseq sl i) set cache)
                   (if x (incf result v)))
            finally (progn
                      (setf (gethash sl cache) result)
                      (return (values (/= 0 result) result))))))

(defun day19 (input &optional part2)
  (multiple-value-bind (towls wants)
      (parse-input input)
    (let ((set (make-hash-set))
          (cache (make-hash-table :test 'equal)))
      (dolist (x towls) (set-insert set x))
      (if part2
          (loop for w in wants
                sum (second (multiple-value-list (can-be-2 w set cache))))
          (loop for w in wants
                when (can-be-2 w set cache)
                  count 1)))))
