(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day10.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day10_demo.input"))

(defun handler-init (line bot-table bot-config-table)
  (str:match line
    (("value " v " goes to bot " bot-num)
     (unless (gethash bot-num bot-table)
       (setf (gethash bot-num bot-table) (make-hash-set)))

     (set-insert (gethash bot-num bot-table) (parse-integer v)))

    (("bot " bot-num " gives.*")
     (setf (gethash bot-num bot-config-table) line))
    ))

(defun handler-line (line bot-table output-table)
  (str:match line
    (("bot " bot-num " gives low to bot " low-to-bot " and high to bot " high-to-bot)
     (let* ((ll (sort (set-to-list (gethash bot-num bot-table)) #'<))
            (min (first ll))
            (max (car (last ll))))

       (unless (gethash low-to-bot bot-table)
         (setf (gethash low-to-bot bot-table) (make-hash-set)))
       (set-insert (gethash low-to-bot bot-table) min)
       
       (unless (gethash high-to-bot bot-table)
         (setf (gethash high-to-bot bot-table) (make-hash-set)))
       (set-insert (gethash high-to-bot bot-table) max)

       (set-delete (gethash bot-num bot-table) min max)))
    
    (("bot " bot-num " gives low to output " low-to-output " and high to bot " high-to-bot)
     (let* ((ll (sort (set-to-list (gethash bot-num bot-table)) #'<))
            (min (first ll))
            (max (car (last ll))))

       (unless (gethash low-to-output output-table)
         (setf (gethash low-to-output output-table) (make-hash-set)))
       (set-insert (gethash low-to-output output-table) min)
       
       (unless (gethash high-to-bot bot-table)
         (setf (gethash high-to-bot bot-table) (make-hash-set)))
       (set-insert (gethash high-to-bot bot-table) max)

       (set-delete (gethash bot-num bot-table) min max)))
    
    (("bot " bot-num " gives low to output " low-to-output " and high to output " high-to-output)
     (let* ((ll (sort (set-to-list (gethash bot-num bot-table)) #'<))
            (min (first ll))
            (max (car (last ll))))
       
       (unless (gethash low-to-output output-table)
         (setf (gethash low-to-output output-table) (make-hash-set)))
       (set-insert (gethash low-to-output output-table) min)

       (unless (gethash high-to-output output-table)
         (setf (gethash high-to-output output-table) (make-hash-set)))
       (set-insert (gethash high-to-output output-table) max)
       
       (set-delete (gethash bot-num bot-table) min max)))
    
    (t (error "unmatched"))))

;; (let ((bot-table (make-hash-table :test 'equal))
;;       (output-table (make-hash-table :test 'equal)))
;;   (loop for line in *input-demo*
;;         do (handler-init line bot-table output-table))

;;   (loop for line in *input-demo*
;;         do (handler-line line bot-table output-table))
  
;;   (format t "~a~%" (set-to-list (gethash "0" output-table)))
;;   (format t "~a~%" (set-to-list (gethash "1" output-table)))
;;   (format t "~a~%" (set-to-list (gethash "2" output-table)))
;;   (format t "~a~%" (set-to-list (gethash "2" bot-table)))
;;   )

(defun part1 (&optional (input *input*))
  (let ((bot-table (make-hash-table :test 'equal))
        (output-table (make-hash-table :test 'equal))
        (bot-config-table (make-hash-table :test 'equal)))
    
    (loop for line in input
          do (handler-init line bot-table bot-config-table))

    ;;(format t "~a~%" (set-to-list (gethash "45" bot-table)))
    ;;(format t "~a~%" (set-to-list (gethash "164" bot-table)))
    ;;(format t "~a~%" (set-to-list (gethash "0" bot-table)))
    ;;(format t "~a~%" (gethash "164" bot-config-table))
    
    ;; (loop for line in input
    ;;       do (handler-line line bot-table output-table))

    ;; add missing bot to table
    (loop for bot-num being each hash-key in bot-config-table
          unless (gethash bot-num bot-table)
            do (setf (gethash bot-num bot-table) (make-hash-set)))
    
    (loop
      (loop for bot-num being each hash-key in bot-table
              using (hash-value set)
            for config = (gethash bot-num bot-config-table)
            ;;do (format t "set ~a of bot ~a" set bot-num)
            when (and set (= 2 (set-count set)) config)
              if (equal (sort (set-to-list set) #'<) '(17 61))
                do (format t "part1: ~a~%" bot-num)
                and do (return-from part1)
            else do (handler-line config bot-table output-table)))))

(defun part2 (&optional (input *input*))
  (let ((bot-table (make-hash-table :test 'equal))
        (output-table (make-hash-table :test 'equal))
        (bot-config-table (make-hash-table :test 'equal)))
    
    (loop for line in input
          do (handler-init line bot-table bot-config-table))
    
    ;; add missing bot to table
    (loop for bot-num being each hash-key in bot-config-table
          unless (gethash bot-num bot-table)
            do (setf (gethash bot-num bot-table) (make-hash-set)))
    
    (loop with flag = t
          if flag
            do (setf flag nil)
            and do (loop for bot-num being each hash-key in bot-table
                           using (hash-value set)
                         for config = (gethash bot-num bot-config-table)
                         when (and set (= 2 (set-count set)) config)
                           do (handler-line config bot-table output-table)
                           and do (setf flag t))
          else
            return (apply #'*
                          (alexandria:flatten
                           (list (set-to-list (gethash "0" output-table))
                                 (set-to-list (gethash "1" output-table))
                                 (set-to-list (gethash "2" output-table))))))))
