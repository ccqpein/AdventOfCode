(load "../../tools/tools.lisp")

;;(defparameter *input* (read-file-by-line "../inputs/day20.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day20_demo.input"))

(defun parse-input (input)
  (let ((table (make-hash-table :test 'equal)))
    (loop for line in input
          for (name targets) = (str:split " -> " line)
          do (let (real-name type status)
               (cond ((str:starts-with-p "&" name)
                      (setf real-name (subseq name 1)
                            type (str:substring 0 1 name)))
                     ((str:starts-with-p "%" name)
                      (setf real-name (subseq name 1)
                            type (str:substring 0 1 name)
                            status nil))
                     (t (setf real-name name
                              type name)))
               (setf (gethash real-name table) (list type status (str:split ", " targets)))))
    table))

(defun gen-rev-table (table)
  (let ((rev-table (make-hash-table :test 'equal)))
    (loop for values being the hash-values of table
            using (hash-key key)
          do (loop for v in (nth 2 values)
                   ;;do (print v)
                   do (cond ((string= (nth 0 (gethash v table)) "&")
                             (push (cons key 0) (gethash v rev-table nil)))
                            (t (push key (gethash v rev-table nil))))))
    rev-table))

(defun one-button-run (table rev-table step)
  (do* ((sig '(("broadcaster" "" 0))) ;; this, from, low/high
        ;;(step 0 (1+ step))
        (low 0)
        (high 0))
       ((not sig) (list low high))
    ;;(format t "sig: ~a~%" sig)
    (let* ((s (pop sig))
           (values (gethash (car s) table)))

      (if (= 0 (nth 2 s)) (incf low) (incf high))
      (if (and (string= (car s) "qb") (= 1 (nth 2 s)))
          (format t "s: ~a, step: ~a~%" s step))
      
      (cond ((string= (car s) "broadcaster")
             (setf sig (append sig (mapcar (lambda (n) (list n (car s) (nth 2 s)))
                                           (nth 2 values)))))
               
            ((string= (car values) "&")
             (setf (cdr (assoc (nth 1 s) ;; from
                               (gethash (car s) rev-table)
                               :test #'string=))
                   (nth 2 s))

             (if (every (lambda (x) (= 1 (cdr x))) (gethash (car s) rev-table))
                 (setf sig (append sig
                                   (mapcar (lambda (n) (list n (car s) 0))
                                           (nth 2 values))))
                 (setf sig (append sig
                                   (mapcar (lambda (n) (list n (car s) 1))
                                           (nth 2 values))))))
               
            ((string= (car values) "%")
             (when (= (nth 2 s) 0)
               (if (nth 1 values)
                   (progn (setf (nth 1 (gethash (car s) table)) nil)
                          (setf sig (append sig (mapcar (lambda (n) (list n (car s) 0))
                                                        (nth 2 values)))))
                   (progn (setf (nth 1 (gethash (car s) table)) t)
                          (setf sig (append sig (mapcar (lambda (n) (list n (car s) 1))
                                                        (nth 2 values))))))
               ))))))

(defun day20 (input &optional part2)
  (let* ((table (parse-input input))
         (rev-table (gen-rev-table table)))
    (loop for s from 1 to (if part2 10000 1000)
          for (l h) = (one-button-run table rev-table s)
          sum l into ll
          sum h into hh
          finally (return (* ll hh)))
    ))

;;(lcm 3739 3911 4003 4073)
