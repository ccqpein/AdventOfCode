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
        ;;(print (count-if (lambda (e) (member e all-0 :test 'string=)) data))
        (loop
          for e in data
          if (member e all-0 :test 'string=)
            collect e into cache
          finally (return cache))
        )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun part2 ()
  (let* ((input (read-file-by-line "./day19.input"))
         (split-it (split-sequence:split-sequence-if
                    (lambda (s) (equal s "")) input))
         (rules (parse-rule (car split-it)))
         (data (cadr split-it)))
    (loop
      for l in data
      if (rec-match (str:split "" l :omit-nulls t) '(0) rules)
        collect l
      )))

(defun rec-match (ss group table)
  (if (and (not ss) (not group)) (return-from rec-match t))
  (let ((gg (gethash (car group) table)))
    (cond ((stringp (caar gg))
           (if (string/= (caar gg) (car ss))
               nil
               (rec-match (cdr ss) (cdr group) table)))
          (t
           (dolist (g gg)
             (if (rec-match ss (append g (cdr group)) table)
                 (return-from rec-match t))))
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun if-match-groups (groups ss table)
;;   ;;(format t "groups: ~a~%" groups)
;;   (loop
;;     with re = '()
;;     for g in groups
;;     do (multiple-value-bind (m? left)
;;            (if-match g ss table)
;;          (if m?
;;              (push left re)))
;;     finally (if (not re)
;;                 (return (values nil ss))
;;                 (return (values t
;;                                 (car (sort re #'< :key #'length)))))))

;; (defun if-match (this-group ss table)
;;   "this-group is (4 5), table is all rules"
;;   ;;(format t "group: ~a, ss: ~a~%" this-group ss)
;;   (unless ss (return-from if-match (values t nil)))
;;   (if (stringp (car this-group)) ;; if it is ("a")
;;       (if (string= (car this-group) (car ss))
;;           (values t (cdr ss))
;;           (values nil ss))
      
;;       (do ((flag t)
;;            (group this-group (cdr group))
;;            (re-ss ss))
          
;;           ((or (not flag) (not group))
;;            (values flag (if flag re-ss ss))
;;            )
        
;;         (multiple-value-setq (flag re-ss)
;;           (if-match-groups (gethash (car group) table) re-ss table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *rule-table*)
;; (defvar *f-table*)
;; (defvar *s-table*)

;; (defun parse-rule-p2 (rules)
;;   (let ((rule-table (make-hash-table :test 'equal))
;;         (f-table (make-hash-table :test 'equal))
;;         (s-table (make-hash-table :test 'equal)))
;;     (dolist (rule rules)
;;       (let* ((a (str:split ": " rule))
;;              (key (car a))
;;              (vs (cadr a))
;;              (cut-v (str:split "|" vs)))

;;         (loop for v in cut-v
;;               do  (let ((nums (handler-case
;;                                   (mapcar #'parse-integer (str:split " " v :omit-nulls t))
;;                                 (error ()
;;                                   ;;(str:split "\"" v :omit-nulls t)
;;                                   )))
;;                         (_key (parse-integer key)))
;;                     ;;(print nums)
;;                     (if nums
;;                         (push nums (gethash _key rule-table)))
;;                     (if (car nums)
;;                         (push _key
;;                               (gethash (car nums) f-table)))
;;                     (if (cadr nums)
;;                         (push _key (gethash (cadr nums) s-table)))))))
;;     ;;(pprint (gethash 1 f-table))
;;     (values rule-table f-table s-table)
;;     ))

;; (multiple-value-setq (*rule-table* *f-table* *s-table*)
;;   (let ((input (read-file-by-line "./day19.input"))
;;         rule data)
;;     (let ((split-it (split-sequence:split-sequence-if
;;                      (lambda (s) (equal s "")) input)))
;;       (setf rule (car split-it)
;;             data (cadr split-it))
;;       (parse-rule-p2 rule))))

;; (defun find-path (this target)
;;   (let ((cache (gethash this *f-table*)))
;;     (if cache
;;         (if (member target cache)
;;             (list (find-if (lambda (x) (= (car x) this))
;;                            (gethash target *rule-table*)))
;;             (remove nil
;;                     (mapcar (lambda (d)
;;                               (alexandria:flatten
;;                                (mapcar (lambda (x)
;;                                          (append
;;                                           (car (find-path this d))
;;                                           (cdr x)))
;;                                        (find-path d target))))
;;                             cache)))
;;         '())))

;; (defun update-target (this targets)
;;   "targets (1 5) -> ((5 4 2 5)) add one dimension"
;;   (if (/= this (car targets))
;;       (mapcar
;;        (lambda (d) (append d (cdr targets)))
;;        (find-path this (car targets)))
;;       (list targets)))

;; (defun iter-step (l targets)
;;   ;;(format t "~a~%~a~%" l targets)
;;   (if (cond
;;         ((and (not l) (not targets)) t)
;;         ((or (not targets) (not l)) nil)
;;         ((= (car l) (car targets))
;;          (progn
;;            (format t "this match: ~a ~a ~%" l targets)
;;            (iter-step (cdr l) (cdr targets))))
;;         (t
;;          (let ((new-targetses (update-target (car l) targets)))
;;            (if (not new-targetses)
;;                nil
;;                (find t (mapcar
;;                         (lambda (d) (iter-step l d))
;;                         new-targetses))))))
;;       (progn (format t "~a~%~a~%" l targets)
;;              t)
;;       nil))

;; (defun str-to-int (s a b)
;;   (loop
;;     with result = '()
;;     for c across s
;;     if (char= #\a c)
;;       do (push a result)
;;     else
;;       do (push b result)
;;     finally (return (reverse result))))

;; (defun day19-p1-v2 ()
;;   (let ((input (read-file-by-line "./day19.input"))
;;         rule data)
;;     (multiple-value-bind (*rule-table* *f-table* *s-table*)
;;         (let ((split-it (split-sequence:split-sequence-if
;;                          (lambda (s) (equal s "")) input)))
;;           (setf rule (car split-it)
;;                 data (cadr split-it))
;;           (parse-rule-p2 rule))
;;       ;;(count-if (lambda (e) (iter-step (str-to-int e 1 14) '(0))) data)
;;       (loop
;;         for d in data
;;         if (iter-step (str-to-int d 121 96) '(0))
;;           collect d into rere
;;         finally (return rere))
;;       )))

;;(iter-step (str-to-int e 121 96) '(0))

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
