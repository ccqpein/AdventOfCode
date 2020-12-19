(load "../tools.lisp")

(defun parse-rule (rules)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (rule rules table)
      (let* ((a (str:split ": " rule))
             (key (car a))
             (vs (cadr a))
             (cut-v (str:split "|" vs)))

        (loop for v in cut-v
              collect (handler-case
                          (mapcar #'parse-integer (str:split " " v :omit-nulls t))
                        (error ()
                          (str:split "\"" v :omit-nulls t)))
              into parsed-vs
              finally (setf (gethash (parse-integer key) table) parsed-vs)
              )))
    ))

(defun across-str-l (l1 l2)
  (alexandria:flatten
   (loop
     for a in l1
     collect (loop
               for b in l2
               collect (str:join "" (list a b))))))

(defun across-merge (ll)
  (do ((a (car ll) result)
       (b (cadr ll) (car rest))
       (rest (cddr ll) (cdr rest))
       result)
      ((not b) a)
    (setf result
          (across-str-l a b))))

(defun rec-find-rule (r table)
  (let ((v (gethash r table)))
    ;;(unless v (format t "kidding me? ~a~%" r))
    (if (stringp (caar v))
        (car v)
        (alexandria:flatten
         (mapcar (lambda (ru) ;; ru => (2 3)
                   (across-merge (mapcar (lambda (x)
                                           (rec-find-rule x table))
                                         ru)))
                 v)))))

(defparameter *table* (make-hash-table))

(defun day19-p1 ()
  (let ((input (read-file-by-line "./day19.input"))
        rule data)
    (let ((split-it (split-sequence:split-sequence-if
                     (lambda (s) (equal s "")) input)))
      (setf rule (car split-it)
            data (cadr split-it))
      (let* ((table (parse-rule rule))
             (all-0 (rec-find-rule 0 table)))
        (setf *table* table)
        (count-if (lambda (e) (member e all-0 :test 'string=)) data)
        ;;(values rule data)
        )
      )))

(defvar *rule-table*)
(defvar *f-table*)
(defvar *s-table*)

(defun parse-rule-p2 (rules)
  (let ((rule-table (make-hash-table :test 'equal))
        (f-table (make-hash-table :test 'equal))
        (s-table (make-hash-table :test 'equal)))
    (dolist (rule rules)
      (let* ((a (str:split ": " rule))
             (key (car a))
             (vs (cadr a))
             (cut-v (str:split "|" vs)))

        (loop for v in cut-v
              do  (let ((nums (handler-case
                                  (mapcar #'parse-integer (str:split " " v :omit-nulls t))
                                (error ()
                                  ;;(str:split "\"" v :omit-nulls t)
                                  )))
                        (_key (parse-integer key)))
                    ;;(print nums)
                    (if nums
                        (push nums (gethash _key rule-table)))
                    (if (car nums)
                        (push _key
                              (gethash (car nums) f-table)))
                    (if (cadr nums)
                        (push _key (gethash (cadr nums) s-table)))))))
    ;;(pprint (gethash 1 f-table))
    (values rule-table f-table s-table)
    ))

(multiple-value-setq (*rule-table* *f-table* *s-table*)
  (let ((input (read-file-by-line "./day19.input"))
        rule data)
    (let ((split-it (split-sequence:split-sequence-if
                     (lambda (s) (equal s "")) input)))
      (setf rule (car split-it)
            data (cadr split-it))
      (parse-rule-p2 rule))))

(defun find-path (this target)
  (let ((cache (gethash this *f-table*)))
    (if cache
        (if (member target cache)
            (list (find-if (lambda (x) (= (car x) this))
                           (gethash target *rule-table*)))
            (mapcar (lambda (d)
                      (alexandria:flatten
                       (mapcar (lambda (x)
                                 (append
                                  (car (find-path this d))
                                  (cdr x))
                                 x)
                               (find-path d target))))
                    cache))
        '())))


;; (defun day19-p2 ()
;;   (let ((input (read-file-by-line "./day19.input"))
;;         rule data)
;;     (let ((split-it (split-sequence:split-sequence-if
;;                      (lambda (s) (equal s "")) input)))
;;       (setf rule (car split-it)
;;             data (cadr split-it))
;;       (let* ((table (parse-rule rule))
;;              all-0)
;;         (setf (gethash 8 table) '((42) (42 8))
;;               (gethash 11 table) '((42 31) (42 11 31)))
;;         (setf *table* table)
;;         ;;(setf all-0 (rec-find-rule 0 table))
;;         ;;(count-if (lambda (e) (member e all-0 :test 'string=)) data)
;;         ;;(values rule data)
;;         )
;;       )))
