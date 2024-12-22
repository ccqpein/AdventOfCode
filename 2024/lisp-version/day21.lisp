(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day21.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day21_demo.input"))

;; 0 -> 9 A
(defparameter *keyboard* '((3 1) (2 0) (2 1) (2 2) (1 0) (1 1) (1 2) (0 0) (0 1) (0 2) (3 2)))

(defparameter *keyboard-1* '((#\< . (1 0)) (#\> . (1 2)) (#\v . (1 1)) (#\^ . (0 1)) (#\A . (0 2))))

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

(defun num-board (last this)
  (let ((last-c (nth (if (string= "A" last) 10 (parse-integer last)) *keyboard*))
        (this-c (nth (if (string= "A" this) 10 (parse-integer this)) *keyboard*)))
    (move-on-kb last-c this-c t)))

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

(defun day21 (input &optional part2)
  (loop with cache = (make-hash-table :test 'equal)
        for line in input
        do (format t "~a~%" line)
        sum (* (parse-integer (subseq line 0 3)) (num-move line (if part2 25 2) cache))))
