(load "../../2020/tools.lisp")

(defstruct (square (:conc-name sq-))
  (is-win nil :type boolean)
  (all-elements nil :type hash-set)
  (all-row-and-col-set nil))

(defun parse-square (chunk) 
  "chunk like ((1 2) (3 4) ...)"
  (let ((all-row-and-col-set
          (append (loop
                    for row in chunk
                    collect (make-hash-set-from-list row))
                  (loop
                    for ind from 0 to 4
                    collect (make-hash-set-from-list (mapcar (lambda (l) (nth ind l)) chunk)))
                  ))
        (all-elements (make-hash-set-from-list (loop for r in chunk appending r)))
        )
    (make-square :all-elements all-elements :all-row-and-col-set all-row-and-col-set)))

(defun parse-input (content)
  (let* ((command (mapcar #'parse-integer (str:split #\, (car content))))
         (raw-squares (do* ((rest (cdr content) (subseq rest 6))
                            chunk 
                            result)
                           ((not rest) result)
                        (setf chunk (subseq rest 1 6))
                        (push (mapcar (lambda (l)
                                        (mapcar #'parse-integer
                                                (remove-if (lambda (s) (string= s ""))
                                                           (str:split #\  l))))
                                      chunk)
                              result)
                        ))
         (squares (mapcar (lambda (sq) (parse-square sq)) raw-squares)))
    (values command squares))) 

(defun is-win? (square command-set)
  (loop for l in (sq-all-row-and-col-set square)
        if (not (hash-set-difference l command-set))
          do (return t)))

(defun unmark-values (square command-set)
  (hash-set-difference (sq-all-elements square) command-set))

(defun part1 (path)
  (let ((content (read-file-by-line path)))
    (multiple-value-bind (command squares)
        (parse-input content)
      (let ((command-set (make-hash-set)))
        (loop
          with win-one
          for c in command
          do (set-insert command-set c)
          do (setf win-one (find-if (lambda (sq) (is-win? sq command-set)) squares))
          when win-one
            do (return (* (apply #'+ (unmark-values win-one command-set))
                          c))))
        )))

(defun part2 (path)
  (let ((content (read-file-by-line path)))
    (multiple-value-bind (command squares)
        (parse-input content)
      (let ((command-set (make-hash-set)))
        (loop
          with result = 0
          for c in command
          do (set-insert command-set c)
          do (loop for sq in squares
                   if (and (not (sq-is-win sq)) (is-win? sq command-set))
                     do (setf (sq-is-win sq) t
                              result (* (apply #'+ (unmark-values sq command-set))
                                        c)))
          finally (return result)
          ))))
  )
