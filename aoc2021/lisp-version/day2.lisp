(load "../../2020/tools.lisp")

(defun part1 (path)
  (loop
    for line in (read-file-by-line path)
    for (command n) = (str::split #\  line)
    if (string= command "forward")
      sum (str::parse-integer n) into forward
    else
      if (string= command "down")
        sum (str::parse-integer n) into depth
    else
      sum (- (str::parse-integer n)) into depth
    finally (return (* forward depth))
    ))

(defun part2 (path)
  (loop
    for line in (read-file-by-line path)
    for (command n) = (str::split #\  line)
    if (string= command "forward")
      sum (str::parse-integer n) into forward
    and sum (* (str::parse-integer n) aim) into depth
    else
      if (string= command "down")
        sum (str::parse-integer n) into aim
    else
      sum (- (str::parse-integer n)) into aim
    finally (return (* forward depth))
    ))

;;(part1 "../src/bin/day2/day2.input")
