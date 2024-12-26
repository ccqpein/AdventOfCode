(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day23.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day23_demo.input"))

(defun parse-input (input)
  (let ((g (make-graph :graph-type 'undirected)))
    (loop for line in input
          do (str:match line
               ((a "-" b) (insert-graph-node g a b 1))))
    g))

;; (defun find-id (g start end step)
;;   (if (= 1 step)
;;       (loop for n in (get-all-nodes-of-id-without-weight g start)
;;             when (equal n end)
;;               collect (list start))
;;       (loop for n in (get-all-nodes-of-id-without-weight g start)
;;             append (loop for r in (find-id g n end (1- step))
;;                          collect (cons start r)))))

;; (defun walk-3 (g)
;;   (let ((all-nodes (get-all-nodes g)))
;;     (loop with set = (make-hash-set)
;;           for s-id in (mapcar #'first all-nodes)
;;           do (loop for x in (mapcar (lambda (ss) (sort ss #'string<))
;;                                     (find-id g s-id s-id 3))
;;                    do (set-insert set x))
;;           finally (return set))))

;;; use the tools:find-id
(defun walk-3 (g)
  (let ((all-nodes (get-all-nodes g)))
    (loop with set = (make-hash-set)
          for s-id in (mapcar #'first all-nodes)
          do (loop for x in (mapcar (lambda (ss) (sort (butlast ss) #'string<))
                                    (find-id g s-id s-id :steps 3))
                   do (set-insert set x))
          finally (return set))))

(defun has-t (sl)
  (some (lambda (s) (str:starts-with-p "t" s)) sl))

(defun day23 (input)
  (loop for s in (set-to-list (walk-3 (parse-input input)))
        when (has-t s)
          count 1))

(defun all-connect (g rest)
  (if (null rest) (return-from all-connect nil))
  (let ((all-ns (get-all-nodes-of-id-without-weight g (first rest))))
    (cons (first rest)
          (all-connect g
                       (loop for n in (cdr rest)
                             when (member n all-ns :test 'equal)
                               collect n)))))

(defun group (g)
  (let ((all-nodes (get-all-nodes g)))
    (loop for nn in all-nodes
          for children = (mapcar #'car (cdr nn))
          collect (cons (first nn) (all-connect g children)))))

(defun day23-2 (input)
  (let ((g (parse-input input)))
    (let ((g-with-len (mapcar (lambda (gg) (list (length gg) gg)) (group g))))
      (format nil "~{~a~^,~}"
              (sort (second (first (sort g-with-len #'> :key #'first)))
                    #'string<)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; should be the "real" correct solution below

(defun all-connect-2 (g rest cache)
  (if (gethash rest cache) (return-from all-connect-2 (gethash rest cache)))
  (if (= 1 (length rest)) (return-from all-connect-2 (list rest)))
  (let ((result (loop for this in rest
                      for all-ns = (get-all-nodes-of-id-without-weight g this)
                      for next-round = (sort (loop for n in rest
                                                   when (member n all-ns :test 'equal)
                                                     collect n)
                                             #'string<)
                      append (loop for g in (all-connect-2 g next-round cache)
                                   collect (cons this g)))))
    (setf (gethash rest cache)
          (remove-duplicates-g result))
    (gethash rest cache)))

(defun remove-duplicates-g (gs)
  (remove-duplicates (mapcar (lambda (g) (sort (copy-seq g) #'string<)) gs) :test 'equal))

(defun day23-21 (input)
  (let ((g (parse-input input)))
    (let ((all-nodes (get-all-nodes g))
          (table (make-hash-table :test 'equal)))
      (let ((re (remove-duplicates-g
                 (loop for nn in all-nodes
                       for children = (sort (mapcar #'car (cdr nn)) #'string<)
                       append
                       (mapcar (lambda (c) (cons (first nn) c)) (all-connect-2 g children table))))))
        (let ((g-with-len (mapcar (lambda (gg) (list (length gg) gg)) re)))
          (format nil "~{~a~^,~}"
                  (sort (second (first (sort g-with-len #'> :key #'first)))
                        #'string<)))))))
