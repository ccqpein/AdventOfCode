(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day6.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day6_demo.input"))

(defun parse-input (input)
  (let (start)
    (values
     (gen-aoc-map
      (loop for line in input
            for l = (concatenate 'list line)
            for r upfrom 0
            for start-c = (position #\^ l)
            when start-c
              do (setf start (list r start-c))
            collect l)
      :ele-coops t
      :coop-ele t)
     start)))

(defun next-offset (this)
  (cond ((equal this '(-1 0))
         '(0 1))
        ((equal this '(0 1))
         '(1 0))
        ((equal this '(1 0))
         '(0 -1))
        ((equal this '(0 -1))
         '(-1 0))))

(defun day6 (&optional (input *input*))
  (multiple-value-bind (map start)
      (parse-input input)
    (let ((set (make-hash-table :test 'equal))
          (dir '(-1 0)))
      (loop do (setf (gethash start set) dir)
            if (aoc-map-beyond-the-range map (mapcar #'+ start dir))
              return nil
            else if (char= #\# (get-aoc-map-ele map (mapcar #'+ start dir)))
                   do (setf dir (next-offset dir)
                   ;;; start (mapcar #'+ dir start) ;; fuck this
                            )
            else do (setf start (mapcar #'+ dir start)))
      ;;(format t "~a" (alexandria:hash-table-keys set))
      (length (alexandria:hash-table-keys set))
      )))

(defun day6-2 (&optional (input *input*))
  (multiple-value-bind (map start)
      (parse-input input)
    (let ((set (make-hash-table :test 'equal))
          (dir '(-1 0)))
      (set-aoc-map-ele map start #\.)
      (loop for r from 0 below (get-aoc-map-rows-len map)
            do (loop for c from 0 below (get-aoc-map-cols-len map)
                     unless (or (equal start `(,r,c)) (char= #\# (get-aoc-map-ele map `(,r ,c))))
                       do (set-aoc-map-ele map `(,r ,c) #\#)
                          (loop with new-start = start
                                and new-dir = dir                                
                                for step upfrom 0
                                if (> step 10000)
                                  do (setf (gethash `(,r ,c) set) t)
                                  and return nil
                                if (aoc-map-beyond-the-range map (mapcar #'+ new-start new-dir))
                                  return nil
                                else if (char= #\# (get-aoc-map-ele map (mapcar #'+ new-start new-dir)))
                                       do (setf new-dir (next-offset new-dir)
                                                ;;new-start (mapcar #'+ new-dir new-start) ;; bug line
                                                )
                                else do (setf new-start (mapcar #'+ new-dir new-start)))
                          (set-aoc-map-ele map `(,r ,c) #\.)))
      (length (alexandria:hash-table-keys set)))))

;; (defun check (start this-dir cache map)
;;   (let ((next-d (next-offset this-dir)))
;;     (do ((point start (mapcar #'+ next-d point)))
;;         (nil)
;;       ;;(format t "point:~a~%" point)
;;       (if (aoc-map-beyond-the-range map point)
;;           (return-from check nil))
;;       (if (char= #\# (get-aoc-map-ele map point))
;;           ;;(return-from check nil)
;;           (setf point (mapcar #'- next-d point)
;;                 next-d (next-offset next-d)))
;;       (when (gethash point cache)
;;         ;;(format t "dir: ~a~%" (gethash point cache))
;;         (if (equal (gethash point cache) next-d)
;;             (progn ;;(format t "bingo~%")
;;                    (return-from check t))
;;             ;;(return-from check nil)
;;             ))
;;       (setf (gethash point cache) this-dir)
;;       )))

;; (defun check (start this-dir cache map)
;;   (if (char= #\# (get-aoc-map-ele map (mapcar #'+ start (next-offset this-dir))))
;;       (return-from check nil))
  
;;   (let* ((this-dir (next-offset this-dir))
;;          (start (mapcar #'+ start this-dir))) ;; one step right
;;     (loop
;;       ;;do (format t "pp: ~a~%" start)
;;       if (aoc-map-beyond-the-range map (mapcar #'+ start this-dir))
;;         return nil
;;       else if (equal (gethash start cache '()) this-dir)
;;              return t
;;       else if (char= #\# (get-aoc-map-ele map (mapcar #'+ start this-dir)))
;;              do (setf (gethash start cache) this-dir
;;                       this-dir (next-offset this-dir)
;;                       start (mapcar #'+ this-dir start))
;;       else do (setf (gethash start cache) this-dir
;;                     start (mapcar #'+ this-dir start)
;;                     ))))

;; (multiple-value-bind (map start)
;;     (parse-input *input-demo*)
;;   (check '(8 2) '(0 -1) (make-hash-table :test 'equal) map))

;; (defun copy-hash-table (hash-table)
;;   (let ((ht (make-hash-table 
;;              :test (hash-table-test hash-table)
;;              :rehash-size (hash-table-rehash-size hash-table)
;;              :rehash-threshold (hash-table-rehash-threshold hash-table)
;;              :size (hash-table-size hash-table))))
;;     (loop for key being each hash-key of hash-table
;;        using (hash-value value)
;;        do (setf (gethash key ht) value)
;;        finally (return ht))))

;; (defun day6-2 (&optional (input *input*))
;;   (multiple-value-bind (map start)
;;       (parse-input input)
;;     (let ((cache (make-hash-table :test 'equal))
;;           (obstruction 0)
;;           (dir '(-1 0)))
;;       (loop do (setf (gethash start cache) dir)
;;             if (check start dir (copy-hash-table cache) map)
;;               do (incf obstruction)
;;                  (format t "here: ~a~%" start)
                 
;;             if (aoc-map-beyond-the-range map (mapcar #'+ start dir))
;;               return nil
;;             else if (char= #\# (get-aoc-map-ele map (mapcar #'+ start dir)))
;;                    do (setf dir (next-offset dir)
;;                             start (mapcar #'+ dir start))
;;             else do (setf start (mapcar #'+ dir start)))
;;       obstruction)))


