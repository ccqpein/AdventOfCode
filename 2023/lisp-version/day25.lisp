(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day25.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day25_demo.input"))

(defun parse-input2 (input)
  (let ((table (make-hash-table :test 'equal)))
    (list table (loop with count = 0
                      for line in input
                      for ll = (str:split ": " line)
                      do (if (not (gethash (car ll) table)) (progn (setf (gethash (car ll) table) count) (incf count)))
                      append (loop for v in (str:split " " (nth 1 ll))
                                   do (if (not (gethash v table)) (progn (setf (gethash v table) count) (incf count)))
                                   collect (list (gethash (nth 0 ll) table)
                                                 (gethash v table)))))))

(defun run2 (n input)
  (loop for i from 0 below (- (length input) 2)
        do (loop for j from (1+ i) below (- (length input) 1)
                 do (loop for z from (1+ j) below (length input)
                          for g1 = (length (remove-duplicates
                                            (alexandria:flatten
                                             (grouping (append (subseq input 0 i)
                                                               (subseq input (1+ i) j)
                                                               (subseq input (1+ j) z)
                                                               (subseq input (1+ z)))))))
                          when (/= g1 n)
                            do (format t "g1: ~a~%" g1)
                            and do (return-from run2 (* g1 (- n g1)))
                          ))))

(defun grouping (input)
  (let (g1)
    (loop with queue = (list (car (car input)))
          and visited = '()
          for x = (pop queue)
          while x
          do (push x visited)
          do (loop for a in input
                   do (cond ((= x (car a))
                             (push a g1)
                             (if (not (member (cadr a) visited)) (push (cadr a) queue))
                             )
                            ((= x (cadr a))
                             (push a g1)
                             (if (not (member (car a) visited)) (push (car a) queue))))))
    g1))

(defun day25 (input)
  (let ((input (parse-input2 input)))
    (run2 (length (alexandria:hash-table-keys (car input)))
          (cadr input))))

(grouping '((0 1) (0 2) (4 5) (4 6) (4 7) (2 8) (9 10) (9 3) (9 11) (1 2)
  (1 12) (1 8) (12 2) (12 8) (6 7) (6 3) (10 3) (13 0) (13 8) (13 12)
  (13 2) (3 11) (7 11) (14 10) (14 9) (14 7) (14 4) (5 10) (5 11) (5 7)))

;; 15
;; 1515
;; try to copy leetcode 1192 solution
;; (defun run (n input)
;;   (let ((visited (make-list n :initial-element 0))
;;         (time (make-list n :initial-element 0))
;;         (low (make-list n :initial-element 0))
;;         (adj (make-list n :initial-element (list)))
;;         (bridges '())
;;         (c 1))
;;     (labels ((dfs (sv parent c)
;;                (setf (nth sv visited) 1
;;                      (nth sv time) c
;;                      (nth sv low) c)
;;                (incf c)
;;                (loop for u in (nth sv adj)
;;                      when (/= u parent)
;;                        if (zerop (nth u visited))
;;                          do (dfs u sv c)
;;                          and do (setf (nth sv low) (min (nth sv low) (nth u low)))
;;                          and do (if (> (nth u low) (nth sv time))
;;                                     (setf bridges (append bridges (list (list sv u)))))
;;                      else
;;                        do (setf (nth sv low) (min (nth sv low) (nth u low)))
;;                      )))
;;       (loop for (u v) in input
;;             do (setf (nth u adj) (append (nth u adj) (list v))
;;                      (nth v adj) (append (nth v adj) (list u))))
;;       (dfs 0 -1 c)
;;       bridges
;;       )))

;;;;;;;;;;;;;;;;;;

(defun parse-input3 (input)
  (let ((table (make-hash-table :test #'equal)))
    (list table (loop for line in input
                      for ll = (str:split ": " line)
                      do (if (not (gethash (car ll) table))
                             (setf (gethash (car ll) table) '()))
                      append (loop for v in (str:split " " (nth 1 ll))
                                   do (if (not (gethash v table))
                                          (setf (gethash v table) '()))
                                   do (push (car ll) (gethash v table))
                                   do (push v (gethash (car ll) table))
                                   collect (list (nth 0 ll) v))))))

(defun make-connection (table key)
  (mapcar (lambda (c) (list key c)) (gethash key table)))

(defun add-in-record (v record)
  (cond ((gethash v record)
         (incf (gethash v record)))
        ((gethash (list (nth 1 v) (nth 0 v)) record)
         (incf (gethash (list (nth 1 v) (nth 0 v)) record)))
        (t (setf (gethash v record) 1))))

(defun visit-all-nodes (table start visited-path visited-points record)
  (loop with next-nodes = (make-connection table start)
        for (a b) in next-nodes
        ;; do (format t "~a~%" visited-path)
        ;; do (format t "~a~%" (list a b))
        ;; do (format t "~a~%~%" visited-points)
        unless (gethash b visited-points)
          do (loop for v in visited-path
                   do (add-in-record v record))
          and do (add-in-record (list a b) record)
          and do (setf (gethash b visited-points) t)
          and collect b into next-starts
        finally (loop for b in next-starts
                      do (visit-all-nodes table
                                          b
                                          (append visited-path (list (list a b)))
                                          visited-points
                                          record))
        ))

;; (let ((table (parse-input3 *input-demo*))
;;       (record (make-hash-table :test #'equal)))
;;   (visit-all-nodes table "jqt" '() (make-hash-table :test 'equal) record)
;;   record)

(defun grouping2 (input)
  (let (g1)
    (loop with queue = (list (car (car input)))
          and visited = '()
          for x = (pop queue)
          while x
          ;;do (format t "visited: ~a~%" visited)
          do (push x visited)
          ;;do (format t "queue: ~a~%" queue)
          ;;do (format t "visited: ~a~%" visited)
          do (loop for a in input
                   do (cond ((string= x (car a))
                             (push a g1)
                             ;;(format t "first ~a~%" a)
                             (if (not (member (cadr a) visited :test 'equal))
                                 (push (cadr a) queue))
                             )
                            ((string= x (cadr a))
                             ;;(format t "second ~a~%" a)
                             (push a g1)
                             (if (not (member (car a) visited :test 'equal))
                                 (push (car a) queue))))))
    g1))

(grouping2 '(("rzs" "rsh") ("cmg" "rzs") ("jqt" "ntq") ("ntq" "bvb")
             ("frs" "lsr") ("pzl" "nvd") ("rsh" "pzl")
             ("lhk" "frs") ("frs" "qnr") ("nvd" "lhk") ("ntq" "hfx") ("rsh" "frs")
             ("rhn" "hfx") ("nvd" "qnr") ("rzs" "lsr") ("xhk" "bvb") ("nvd" "cmg")
             ("pzl" "lsr") ("hfx" "bvb") ("xhk" "ntq") ("xhk" "hfx") ("qnr" "rzs")
             ("cmg" "qnr") ("cmg" "lhk") ("lhk" "lsr") ("rhn" "bvb") ("jqt" "rhn")
             ("jqt" "xhk") ("rhn" "xhk") ("rsh" "lsr")))

(defun run3 (table)
  (let ((record (make-hash-table :test #'equal)))
    (loop for node being the hash-keys of table
          ;;using (hash-value connections)
          do (visit-all-nodes table node '() (make-hash-table :test 'equal) record)
          )
    record))

(defun day25-2 (input)
  (let ((input (parse-input3 input))
        record)
    (setf record (run3 (car input)))
    (let ((sorted-path (sort (alexandria:hash-table-alist record) #'> :key 'cdr))
          (all-nodes-num (length (alexandria:hash-table-keys (car input)))))
      (format t "sorted paths: ~a~%" sorted-path)
      (format t "length of : ~a~%" all-nodes-num)
      (setf sorted-path (mapcar #'car sorted-path))
      (loop for i from 0 below (- (length sorted-path) 2)
            do (loop for j from (1+ i) below (- (length sorted-path) 1)
                     do (loop for z from (1+ j) below (length sorted-path)
                              for g1 = (length (remove-duplicates
                                                (alexandria:flatten
                                                 (grouping2 (append (subseq sorted-path 0 i)
                                                                    (subseq sorted-path (1+ i) j)
                                                                    (subseq sorted-path (1+ j) z)
                                                                    (subseq sorted-path (1+ z)))))
                                                :test 'equal))
                              when (/= g1 all-nodes-num)
                                do (format t "g1: ~a~%" g1)
                                and do (return-from day25-2 (* g1 (- all-nodes-num g1)))
                              ))))
    ))
