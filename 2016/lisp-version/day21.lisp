(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day21.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day21_demo.input"))

(defun swap-position (x y input)
  (let ((a (nth x input)))
    (setf (nth x input) (nth y input)
          (nth y input) a)
    input))

(defun swap-letter (x y input)
  (let ((xx (position x input :test #'string=))
        (yy (position y input :test #'string=)))
    (swap-position xx yy input)))

(defun rot-left (n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rot-right (n l)
  (rot-left (- (length l) (mod n (length l))) l))

(defun rotate (dir x input)
  (cond ((string= dir "left") (rot-left x input))
        ((string= dir "right") (rot-right x input))))

(defun rotate-base-on (x input)
  (let ((xx (position x input :test #'string=)))
    (rotate "right" (if (>= xx 4)
                        (+ xx 2)
                        (+ xx 1))
            input)))

(defun reverse-position (x y input)
  (append (subseq input 0 x)
          (reverse (subseq input x (1+ y)))
          (subseq input (1+ y))))

(defun move-position (x y input)
  (let ((xx (nth x input))
        (input (append (subseq input 0 x)
                       (subseq input (1+ x)))))
    (append (subseq input 0 y)
            (list xx)
            (subseq input y))))

(defun parse-line (line)
  (str:match line
    (("swap position " x " with position " y)
     (list #'swap-position (parse-integer x) (parse-integer y)))
    (("swap letter " x " with letter " y)
     (list #'swap-letter x y))
    (("rotate based on position of letter " x)
     (list #'rotate-base-on x))
    (("rotate " dir " " x " step" _)
     (list #'rotate dir (parse-integer x)))
    (("reverse positions " x " through " y)
     (list #'reverse-position (parse-integer x) (parse-integer y)))
    (("move position " x " to position " y)
     (list #'move-position (parse-integer x) (parse-integer y)))))

(defun day21 (init &optional (input *input*))
  (loop with init = (mapcar #'string (concatenate 'list init))
        for line in input
        for comm = (parse-line line)
        ;;do (format t "~a~%" comm)
        do (setf init (apply (car comm) (append (cdr comm) (list init))))
        finally (return (apply #'str:concat init))))

;; (day21 "abcdefgh")
