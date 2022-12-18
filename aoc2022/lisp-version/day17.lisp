(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day17.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day17_demo.input"))

;;(defparameter rocks ())

(defstruct rock
  num
  max
  min)

(defun shift (rock move)
  (loop for r in rock
		for new-r = (cond ((char= #\>) (ash r 1))
						  ((char= #\<) (ash r -1)))))

(defun one-rock-fall (rock tower inputs)
  (let ((first-4 (subseq inputs 0 4)))
	(dolist (m first-4)
	  (setf rock (shift rock))))
  )
