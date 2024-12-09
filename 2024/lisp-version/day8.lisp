(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day8.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day8_demo.input"))

(defun parse-input (input)
  (gen-aoc-map (loop for line in input
                     collect (concatenate 'list line))
               :ele-coops t
               :coop-ele t))

(defun anti-node (coop0 coop1)
  (let ((row-offset (- (first coop1) (first coop0)))
        (col-offset (- (second coop1) (second coop0))))
    (list (mapcar #'+ coop1 `(,row-offset ,col-offset))
          (mapcar #'- coop0 `(,row-offset ,col-offset)))))

(defun list-anti-nodes (m coops)
  (loop for x from 0 below (1- (length coops))
        append (loop for y from (1+ x) below (length coops)
                     append (remove-if (lambda (c) (aoc-map-beyond-the-range m c))
                                       (anti-node (nth x coops) (nth y coops))))))

(defun anti-node-2 (m coop0 coop1)
  (let ((row-offset (- (first coop1) (first coop0)))
        (col-offset (- (second coop1) (second coop0))))
    (append
     (do ((a coop1 (mapcar #'+ a `(,row-offset ,col-offset)))
          result)
         ((aoc-map-beyond-the-range m a) result)
       (push a result))
     (do ((a coop0 (mapcar #'- a `(,row-offset ,col-offset)))
          result)
         ((aoc-map-beyond-the-range m a) result)
       (push a result)))))

(defun list-anti-nodes-2 (m coops)
  (loop for x from 0 below (1- (length coops))
        append (loop for y from (1+ x) below (length coops)
                     append (anti-node-2 m (nth x coops) (nth y coops)))))

(defun day8 (&optional part2 (input *input*))
  (let ((m (parse-input input))
        all-anti-nodes
        (table (make-hash-table :test 'equal)))
    (setf all-anti-nodes
          (loop for ele being the hash-key in (amap-ele-coops m)
                  using (hash-value coops)
                when (char/= #\. ele)
                  append (if part2
                             (list-anti-nodes-2 m coops)
                             (list-anti-nodes m coops))))

    (loop for coop in all-anti-nodes
          do (setf (gethash coop table) t))

    (length (alexandria:hash-table-keys table))))
