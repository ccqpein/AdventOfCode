(defun part1 (pos1 pos2)
  (let ((die (loop for i from 1 to 100 collect i)))
    ;; infinity loop
    (setf (cdr (last die)) die)
    (loop
      with p1 = pos1 and p2 = pos2 and die-time = 0
      for move = (apply #'+ (subseq die 0 3))
      do (setf die (cdddr die))
         
      sum (progn (setf p1 (if (zerop (mod (+ move p1) 10)) 10 (mod (+ move p1) 10)))
                 p1)
        into p1-score
      do (incf die-time 3)
      when (>= p1-score 1000)
        return (* die-time p2-score)

      do (setf move (apply #'+ (subseq die 0 3))
               die (cdddr die))
      sum (progn (setf p2 (if (zerop (mod (+ move p2) 10)) 10 (mod (+ move p2) 10)))
                 p2)
        into p2-score

      do (incf die-time 3)
      when (>= p2-score 1000)
        return (* die-time p2-score)
      ) 
    ))

(defun part2 (record p1 p2 score1 score2 player1)
  (if (not record) (setf record (make-hash-table :test 'equal)))
  
  (cond ((>= score1 21) (return-from part2 '(1 . 0)))
        ((>= score2 21) (return-from part2 '(0 . 1))))

  (let (score)
    (setf score (gethash (list p1 p2 score1 score2 player1) record))
    (if score (return-from part2 score))

    (setf score (gethash (list p2 p1 score2 score1 (not player1)) record))
    (if score (return-from part2 (cons (cdr score) (car score))))
    )
  
  (loop
    for (d1 d2 d3) in (iter-product '(1 2 3) '(1 2 3) '(1 2 3))
    for move = (+ d1 d2 d3)
    for this-score = (if player1
                         (let ((next-p1 (if (zerop (mod (+ move p1) 10))
                                            10
                                            (mod (+ move p1) 10))))
                           (part2 record next-p1 p2 (+ next-p1 score1) score2 (not player1)))
                         (let ((next-p2 (if (zerop (mod (+ move p2) 10))
                                            10
                                            (mod (+ move p2) 10))))
                           (part2 record p1 next-p2 score1 (+ next-p2 score2) (not player1))))
    sum (car this-score) into s1
    sum (cdr this-score) into s2
    finally (return
              (progn (setf (gethash (list p1 p2 score1 score2 player1) record) (cons s1 s2))
                     (cons s1 s2)))
    ))

;; length has to be same
(defun iter-product (&rest lst)
  (setf lst (reverse lst))
  (do ((first (car lst))
       (second (cadr lst) (cadr lst)))
      ((not second) first)
    (setf first
          (loop for x in second
                nconc (loop for y in first
                            if (eq 'cons (type-of y))
                              collect (cons x y)
                            else
                              collect (list x y)))
          lst (cdr lst))))
