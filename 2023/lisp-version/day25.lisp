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

;; (grouping '((0 1) (0 2) (4 5) (4 6) (4 7) (2 8) (9 10) (9 3) (9 11) (1 2)
;;   (1 12) (1 8) (12 2) (12 8) (6 7) (6 3) (10 3) (13 0) (13 8) (13 12)
;;   (13 2) (3 11) (7 11) (14 10) (14 9) (14 7) (14 4) (5 10) (5 11) (5 7)))

;; 15
;; 1515
;; try to copy leetcode 1192 solution
(defun run (n input)
  (let ((visited (make-list n :initial-element 0))
        (time (make-list n :initial-element 0))
        (low (make-list n :initial-element 0))
        (adj (make-list n :initial-element (list)))
        (bridges '())
        (c 1))
    (labels ((dfs (sv parent c)
               (setf (nth sv visited) 1
                     (nth sv time) c
                     (nth sv low) c)
               (incf c)
               (loop for u in (nth sv adj)
                     when (/= u parent)
                       if (zerop (nth u visited))
                         do (dfs u sv c)
                         and do (setf (nth sv low) (min (nth sv low) (nth u low)))
                         and do (if (> (nth u low) (nth sv time))
                                    (setf bridges (append bridges (list (list sv u)))))
                     else
                       do (setf (nth sv low) (min (nth sv low) (nth u low)))
                     )))
      (loop for (u v) in input
            do (setf (nth u adj) (append (nth u adj) (list v))
                     (nth v adj) (append (nth v adj) (list u))))
      (dfs 0 -1 c)
      bridges
      )))
