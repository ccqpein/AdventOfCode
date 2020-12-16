;;; use this one
(ql:quickload "str")

(defpackage aoc-tools
  (:use cl)
  (:export read-file-by-line))

(in-package aoc-tools)

(defun read-file-by-line (filepath)
  "read file line by line, return a list of file"
  (with-open-file (s filepath :direction :input)
    (do ((l (read-line s) (read-line s nil 'eof))
         (result '()))
        ((eq l 'eof) (reverse result))
      (push l result))))

(defstruct (hash-set (:conc-name set-))
  (inner (make-hash-table :test 'equal) :type hash-table :read-only t))

(defun set-insert (set &rest eles)
  (declare (hash-set set))
  (dolist (ele eles)
    (setf (gethash ele (set-inner set)) t)))

(defun set-get (set ele)
  (declare (hash-set set))
  (if (gethash ele (set-inner set)) ele nil))

(defun list-of-sum-rest (l)
  (loop
    with sum = 0
    for n in (reverse l)
    collect (incf sum n) into result
    finally (return (reverse result))))
