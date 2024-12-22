(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day21.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day21_demo.input"))

;; 0 -> 9 A
(defparameter *keyboard* '((3 1) (2 0) (2 1) (2 2) (1 0) (1 1) (1 2) (0 0) (0 1) (0 2) (3 2)))

(defun move-on-kb (from to num-b)
  (let ((offset-0 (- (first to) (first from)))
        (offset-1 (- (second to) (second from)))
        result)
    (loop with start = from
          for offset in (append
                         (make-list (abs offset-1) :initial-element (if (< offset-1 0) '(0 -1) '(0 1)))
                         (make-list (abs offset-0) :initial-element (if (< offset-0 0) '(-1 0) '(1 0))))
          if (equal start (if num-b '(3 0) '(0 0)))
            return nil
          do (setf start (mapcar #'+ offset start))
          finally (push (append (make-list (abs offset-1) :initial-element (if (< offset-1 0) #\< #\>))
                                (make-list (abs offset-0) :initial-element (if (< offset-0 0) #\^ #\v))
                                (list #\A))
                        result))

    (loop with start = from
          for offset in (append
                         (make-list (abs offset-0) :initial-element (if (< offset-0 0) '(-1 0) '(1 0)))
                         (make-list (abs offset-1) :initial-element (if (< offset-1 0) '(0 -1) '(0 1))))
          if (equal start (if num-b '(3 0) '(0 0)))
            return nil
          do (setf start (mapcar #'+ offset start))
          finally (push (append (make-list (abs offset-0) :initial-element (if (< offset-0 0) #\^ #\v))
                                (make-list (abs offset-1) :initial-element (if (< offset-1 0) #\< #\>))
                                (list #\A))
                        result))
    result))

;; (defun move-on-kb-2 (from to)
;;   (let ((offset-0 (- (first to) (first from)))
;;         (offset-1 (- (second to) (second from))))
;;     (list (append (make-list (abs offset-0) :initial-element (if (< offset-0 0) #\^ #\v))
;;                   (make-list (abs offset-1) :initial-element (if (< offset-1 0) #\< #\>))
;;                   (list #\A))
;;           (append (make-list (abs offset-1) :initial-element (if (< offset-1 0) #\< #\>))
;;                   (make-list (abs offset-0) :initial-element (if (< offset-0 0) #\^ #\v))
;;                   (list #\A)))))

;; (defun one-code-move (s)
;;   (let ((nums (mapcar #'parse-integer (mapcar #'string (butlast (concatenate 'list s))))))
;;     (let ((all-buttons (append (list (nth 10 *keyboard*))
;;                                (loop for n in nums collect (nth n *keyboard*))
;;                                (list (nth 10 *keyboard*)))))
;;       (loop for (from to) on all-buttons by #'cdr
;;             while to
;;             collect (move-on-kb-2 from to)))))

(defparameter *keyboard-1* '((#\< . (1 0)) (#\> . (1 2)) (#\v . (1 1)) (#\^ . (0 1)) (#\A . (0 2))))

;; (defun one-bot-move (sl)
;;   (let ((all-buttons (mapcar (lambda (s) (cdr (assoc s *keyboard-1*))) (append (list #\A) sl))))
;;     ;;(format t "~a~%" all-buttons)
;;     (loop for (from to) on all-buttons by #'cdr
;;           while to
;;           collect (move-on-kb from to nil))))

;; (loop for line in *input-demo*
;;       for kb-move = (one-code-move line)
;;       for len = (length (one-bot-move (one-bot-move kb-move)))
;;       do (format t "~a: ~a, ~a~%" line len (parse-integer (subseq line 0 3)))
;;       sum (* len (parse-integer (subseq line 0 3))))


(defun num-board (last this)
  (let ((last-c (nth (if (string= "A" last) 10 (parse-integer last)) *keyboard*))
        (this-c (nth (if (string= "A" this) 10 (parse-integer this)) *keyboard*)))
    (move-on-kb last-c this-c t)))

;; (defun bot-board (last this)
;;   (move-on-kb (cdr (assoc last *keyboard-1*)) (cdr (assoc this *keyboard-1*)) nil))

;; (defun rec-bot-move (sl level)
;;   (if (= level 0) (return-from rec-bot-move sl))
;;   (loop for (a b) on sl by #'cdr
;;         while b
;;         collect (loop for x in (bot-board a b)
;;                       collect (rec-bot-move x (1- level)))))

;; (defun shortest-command (l)
;;   (loop for (ind n) in (sort (loop for e from 0 below (length l)
;;                                   collect (list e (length (nth e l))))
;;                             #'< :key #'second)
;;         collect (nth ind l)))

;; (do* ((input (list (first *input-demo*)) (cdr input))
;;       (line (cons "A" (mapcar #'string (concatenate 'list (car input))))
;;             (cons "A" (mapcar #'string (concatenate 'list (car input)))))
;;       result)
;;      ((not input) result)
;;   (format t "~a~%" line)
;;   (push (loop for (a b) on line by #'cdr
;;               while b
;;               append (loop for c in (remove-duplicates (num-board a b) :test 'equal)
;;                            do (format t "c: ~a~%" c)
;;                            append (loop for (d e) on (cons #\A c) by #'cdr
;;                                         while e
;;                                         append (loop for f in (bot-board d e)
;;                                                      ;;do (format t "f: ~a~%" f)
;;                                                      for next = (loop for (g h) on (cons #\A f) by #'cdr
;;                                                                       while h
;;                                                                       for last = (shortest-command
;;                                                                                   (remove-duplicates (bot-board g h)
;;                                                                                                      :test 'equal))
;;                                                                       do (format t "all last: ~a~%" (bot-board g h))
                                                           
;;                                                                       append (first last))
;;                                                      do (format t "next ~a~%" next)
;;                                                      append next))))
;;         result))

(defun num-move (nums bot-nums cache)
  (let ((nums (cons "A" (mapcar #'string (concatenate 'list nums)))))
    (loop for (a b) on nums by #'cdr
          while b
          sum (loop for x in (remove-duplicates (num-board a b) :test 'equal)
                    ;;do (format t "x: ~A~%" x)
                    minimize (bot-move (cons #\A x) bot-nums cache)))))

(defun bot-move (sl deep cache)
  ;;(format t "sl: ~a ~a~%" sl deep)
  (if (= 0 deep) (return-from bot-move (1- (length sl))))
  (loop for (a b) on sl by #'cdr
        while b
        sum (next-bot-move a b deep cache)))

(defun next-bot-move (last this deep cache)
  (if (gethash (list last this deep) cache) (return-from next-bot-move (gethash (list last this deep) cache)))
  (setf (gethash (list last this deep) cache)
        (loop for a in (move-on-kb (cdr (assoc last *keyboard-1*)) (cdr (assoc this *keyboard-1*)) nil)
              ;;do (format t "a: ~a ~a~%" a deep)
              minimize (bot-move (cons #\A a) (1- deep) cache)))
  (gethash (list last this deep) cache))

;;(loop for line in *input* sum (* (parse-integer (subseq line 0 3)) (num-move line 2)))

(loop with cache = (make-hash-table :test 'equal)
      for line in *input*
      do (format t "~a~%" line)
      sum (* (parse-integer (subseq line 0 3)) (num-move line 25 cache)))
