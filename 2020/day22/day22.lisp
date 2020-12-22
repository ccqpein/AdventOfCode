(load "../tools.lisp")

(defun parser ()
  (loop
    with result = '()
    and cache = '()
    for line in (read-file-by-line "./day22.input")
    if (string= "" line)
      do (push (reverse cache) result)
      and do (setf cache '())
    else
      do (push line cache)
    finally (progn (push (reverse cache) result)
                   (return (reverse result)))))

(defun play (input)
  (let ((p1 (mapcar #'parse-integer (cdar input)))
        (p2 (mapcar #'parse-integer (cdadr input))))
    
    (do ((a (car p1) (car p1))
         (b (car p2) (car p2))
         (count 0))
        ((not (and a b)) (values p1 p2))
      ;;(if (> count 29) (return-from paly (values a b)))
      ;;(format t "~a ~a ~%" a b)
      ;;(format t "~a ~a ~%" p1 p2)
      (if (> a b)
          (setf p1 (append (cdr p1) (list a b))
                p2 (cdr p2))
          (setf p2 (append (cdr p2) (list b a))
                p1 (cdr p1)))
      ;;(incf count)
      )
    ;;(values p1 p2)
    ))

(defun count-it (a b)
  (let ((x (or a b)))
    (loop
      for i from (length x) downto 1
      for ind from 0
      sum (* i (nth ind x)))))

(defun part1 ()
  (multiple-value-bind (a b)
      (paly (parser))
    (count-it a b)))

(defun game (p1 p2)
  (do ((a (car p1) (car p1))
       (b (car p2) (car p2))
       (record '())
       (count 0))
      ((not (and a b)) (values p1 p2))

    (if (member (list p1 p2) record :test #'equal)
        (return-from game (values '(1) nil))
        (push (list p1 p2) record))
    
    (if (and (<= a (1- (length p1)))
             (<= b (1- (length p2)))
             )
        (multiple-value-bind (l1 l2) ;; length bind to a and b
            (game (subseq (cdr p1) 0 a)
                  (subseq (cdr p2) 0 b))
          (setf a (length l1)
                b (length l2))))
    
    (if (> a b)
        (setf p1 (append (cdr p1) (list (car p1) (car p2)))
              p2 (cdr p2))
        (setf p2 (append (cdr p2) (list (car p2) (car p1)))
              p1 (cdr p1)))
    ))

(defun part2 ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((input (parser))
         (p1 (mapcar #'parse-integer (cdar input)))
         (p2 (mapcar #'parse-integer (cdadr input))))
    (multiple-value-bind (x y)
        (game p1 p2)
      (count-it x y))
    ))
