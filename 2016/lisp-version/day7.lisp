(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day7.input"))
;;(defparameter *input-demo* (read-file-by-line "../inputs/day7_demo.input"))

(defun if-abba (str)
  (multiple-value-bind (matched chars)
      (cl-ppcre:scan-to-strings "(.)(.)\\2\\1" str)
    (and matched (string/= (aref chars 0)
                           (aref chars 1)))))

(defun groups (line)
  (alexandria:flatten
   (mapcar (lambda (l) (str:split "]" l))
           (str:split "[" line))))

(defun all-even-element (l)
  (loop for i from 1 by 2
        when (>= i (length l))
          return res
        collect (nth i l) into res))

(defun all-odd-element (l)
  (loop for i from 0 by 2
        when (>= i (length l))
          return res
        collect (nth i l) into res))

(defun part1 (&optional (input *input*))
  (loop for line in input
        for gg = (groups line)
        if (and (every (lambda (s) (not (if-abba s)))
                       (all-even-element gg))
                (some #'if-abba (all-odd-element gg)))
          count 1))

(defun if-aba (str)
  (do ((s str (str:s-rest s))
       res)
      ((str:empty? s) (remove nil res))
    (push (multiple-value-bind (matched chars)
                (cl-ppcre:scan-to-strings "^(.)(.)\\1" s)
              (declare (ignore matched))
            (coerce chars 'list))
          res)
    ))

(defun if-aba-outside (chars str)
  (let ((g (str:concat (second chars) (first chars) (second chars))))
    ;;(format t "try to match: ~a~%" g)
    (unless chars (return-from if-aba-outside nil))
    (str:contains? g str)))

(defun part2 (&optional (input *input*))
  (loop for line in input
        for gg = (groups line)
        if (loop for chars in (loop for a in (all-even-element gg)
                                    append (if-aba a))
                 when (loop for outside in (all-odd-element gg)
                            when (if-aba-outside chars outside)
                              return t
                            finally (return nil))
                   return t
                 finally (return nil))
          ;;do (format t "~a~%" line)
          ;;and 
          count 1))

