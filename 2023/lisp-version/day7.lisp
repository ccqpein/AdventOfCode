(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day7.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day7_demo.input"))

(defun parse-input (input)
  (loop for line in input
        for ll = (str:split " " line)
        collect (list (car ll) (parse-integer (cadr ll)))
        ))

(defun find-type-list (chars)
  (let ((counts (loop for c in (remove-duplicates chars)
                      collect (count c chars))))
    (cond ((= 1 (length counts))
           6
           )
          ((and (= 2 (length counts)) (= 4 (apply #'max counts)))
           5)
          ((and (= 2 (length counts)) (= 3 (apply #'max counts)))
           4)
          ((= 3 (apply #'max counts))
           3)
          ((and (= 3 (length counts)) (= 2 (apply #'max counts)))
           2)
          ((= 2 (apply #'max counts))
           1)
          (t 0))))

(defun hand-change (hand)
  (if (find #\J hand)
      (loop
        with count-j = (count #\J hand)
        for c in '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)
        maximize (find-type-list
                  (append (remove #\J hand) (make-list count-j :initial-element c))))
      (find-type-list hand)))

(defun char-compare (c1 c2)
  (< (position c1 '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))
     (position c2 '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))))

(defun char-compare-2 (c1 c2)
  (< (position c1 '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))
     (position c2 '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))))

(defun sort-hand (h1 h2 &optional part2)
  (let* ((hc1 (concatenate 'list h1))
         (hc2 (concatenate 'list h2))
         (h1t (if part2
                  (hand-change hc1)
                  (find-type-list hc1)))
         (h2t (if part2
                  (hand-change hc2)
                  (find-type-list hc2))))
    (if (/= h1t h2t)
        (> h1t h2t)
        (loop for c1 in hc1
              for c2 in hc2
              if (char/= c1 c2)
                return (if part2
                           (char-compare-2 c1 c2)
                           (char-compare c1 c2))
              ))))

(defun day7 (input &optional part2)
  (let ((input (reverse (sort (parse-input input) (lambda (a b) (sort-hand a b part2)) :key #'car))))
    (loop for i upfrom 1
          for (hand num) in input
          sum (* i num))))

;; (day7 *input*)
;; (day7 *input* t) ;; par2
