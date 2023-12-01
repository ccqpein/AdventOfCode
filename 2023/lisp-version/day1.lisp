(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day1.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day1_demo.input"))

(defun char-to-num (cc)
  (if (member (char-code cc) (loop for i from 48 to 57 collect i))
      (- (char-code cc) 48)
      (error "e")))

(defun num-2-english (n)
  (format nil "~R" n))

(defun day1 ()
  (loop
    for line in *input*
    sum (loop
          with first = nil
          and last = nil
          for c across line
          do (ignore-errors
              (if (not first) (setf first (char-to-num c)))
              (setf last (char-to-num c)))
          finally (return (+ (* first 10) last)))
    ))

(defun day1-2 ()
  (loop
    for line in *input*
    sum (do (first
             last
             (inner-line line (str:s-rest inner-line)))
            ((string= "" inner-line) (+ (* 10 first) last))
          (handler-case
              (let ((c (elt inner-line 0)))
                (if (not first) (setf first (char-to-num c)))
                (setf last (char-to-num c)))
            (error ()
              (loop for i from 0 to 9
                    if (str:starts-with-p (num-2-english i) inner-line)
                      do (if (not first) (setf first i))
                      and do (setf last i)
                      and return nil)))
          )))
