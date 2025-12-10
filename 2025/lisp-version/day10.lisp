(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day10.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day10_demo.input"))

(defun parse-input (input)
  (loop for line in input
        for pre-res = (str:match line 
                        (("\\[" target "\\] " buttons " {" vs "}")
                         (list (concatenate 'list target)
                               (str:split " " buttons)
                               (str:split "," vs))))
        collect (list (mapcar (lambda (x) (if (char= #\# x) t nil))
                              (first pre-res))
                      (mapcar (lambda (x) (str:match x
                                            (("\\(" a "\\)")
                                             (mapcar #'parse-integer
                                                     (str:split "," a)))))
                              (second pre-res))
                      (mapcar #'parse-integer (third pre-res)))))


(defun limited-steps-handle (input target buttons limit)
  (if (zerop limit) (return-from limited-steps-handle nil))
  (loop with res = nil
        for b in buttons
        for r = (one-step-handle input target b buttons limit)
        if r
          do (if res (setf res (min res r)) (setf res r))
        finally (return res)))

(defun one-step-handle (input target button buttons limit)
  (let ((input (copy-list input)))
    (mapcar (lambda (b) (setf (nth b input) (not (nth b input))))
            button)
    (if (equal input target)
        1
        (let ((next (limited-steps-handle input target buttons (1- limit))))
          (if next
              (1+ next)
              nil)))))

(defun day10 (input)
  (let ((input (parse-input input)))
    (loop for x in input
          sum (loop for limit from 1
                    for r = (limited-steps-handle
                             (loop for xx in (first x) collect nil)
                             (first x)
                             (second x)
                             limit)
                    if r
                      return r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun p2-handle (target buttons limit cache)
;;   (if (every #'zerop target) (return-from p2-handle 0))
;;   (if (zerop limit) (return-from p2-handle nil))
;;   (if (gethash (list target buttons limit) cache)
;;       (return-from p2-handle (gethash (list target buttons limit) cache)))
;;   (when (some (lambda (n) (< n 0)) target)
;;     (setf (gethash (list target buttons limit) cache) nil)
;;     (return-from p2-handle nil))
;;   (let (res)
;;     (setf res
;;           (loop for b in buttons
;;                 for next-target = (loop with a = (copy-list target)
;;                                         for ind in b
;;                                         do (decf (nth ind a))
;;                                         finally (return a))
;;                 unless (some (lambda (n) (< n 0)) target)
;;                   ;;do (format t "next-target:~a~%" next-target)
;;                   collect (p2-handle next-target buttons (1- limit) cache)))

;;     (setf res (remove nil res))
;;     (if (zerop (length res))
;;         (setf (gethash (list target buttons limit) cache) nil)
;;         (setf (gethash (list target buttons limit) cache) (1+ (apply #'min res))))))

;; (declaim (optimize (speed 3)))

;; (deftype list-of-real ()
;;   '(and list
;;         (satisfies list-of-real-p))) ; And satisfy this predicate

;; (defun list-of-real-p (lst)
;;   (and (listp lst)
;;        (every #'realp lst)))


(defun cal-buttons-limit (button target)
  (loop for ind in button
        minimize (nth ind target)))

(defun cal-new-target (button times target)
  (loop with n-target = (copy-list target)
        for ind in button
        do (decf (nth ind n-target) times)
        finally (return n-target)))

(defun sort-buttons (buttons)
  (sort buttons (lambda (a b) (> (list-length a) (list-length b)))))

(defun has-all-button (buttons target)
  (let ((possible-buttons (remove-duplicates
                           (loop for button in buttons
                                 append button))))
    (every (lambda (tt) (member tt possible-buttons))
           (loop for ind from 0 below (length target)
                 when (> (nth ind target) 0)
                   collect ind))))

(defun p2-handle (buttons target)
  (if (zerop (length buttons)) (return-from p2-handle nil))
  (loop with res = nil
        for x from (cal-buttons-limit (car buttons) target) downto 0
        for new-target = (cal-new-target (car buttons) x target)
        when (and (has-all-button (cdr buttons) new-target)
                  (every (lambda (n) (>= n 0)) target))
          do (cond ((every #'zerop new-target)
                    (return x))
                   ((some (lambda (n) (< n 0)) target)
                    nil)
                   (t (let ((r (p2-handle (cdr buttons) new-target)))
                        (if r (return (+ x r))))))
        ))

;; (let ((cache (make-hash-table :test #'equal)))
;;   (p2-handle '(3 5 4 7) '((3) (1 3) (2) (2 3) (0 2) (0 1)) 3 cache))
;; (p2-handle '((3) (1 3) (2) (2 3) (0 2) (0 1)) '(3 5 4 7))

(defun day10-2 (input)
  (let ((input (parse-input input)))
    (loop for x in input
          for xx = (p2-handle (sort-buttons (second x)) (third x))
          do (format t "result: ~a~%" xx)
          sum xx)))
