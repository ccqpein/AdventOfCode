(load "../../2020/tools.lisp")

(defmacro set-nth-nest (l coorp new-value)
  `(setf ,(loop
            for i in coorp
            do (setf l `(nth ,i ,l))
            finally (return l))
         ,new-value))

(defun part1 (path step)
  (let ((content (mapcar (lambda (l) (mapcar #'digit-char-p
                                             (concatenate 'list l)))
                         (read-file-by-line path))))
    (labels ((add-1 (content)
               (loop for l in content collect (mapcar #'1+ l)))
             (adjust-energy (content flag)
               (when flag
                 (setf flag nil)
                 (loop for i from 0 below (length content)
                       do (loop for j from 0 below (length (car content))
                                if (>= (nth-nest content (list i j)) 10)
                                  do (set-nth-nest content (i j) 0)
                                  and do (setf flag t)
                                  and do (loop for (x y) in (list (list (1- i) (1- j))
                                                                  (list (1- i) j)
                                                                  (list (1- i) (1+ j))
                                                                  (list i (1- j))
                                                                  (list i (1+ j))
                                                                  (list (1+ i) (1- j))
                                                                  (list (1+ i) j)
                                                                  (list (1+ i) (1+ j)))
                                               unless (or (< x 0)
                                                          (< y 0)
                                                          (= x (length content))
                                                          (= y (length (car content))))
                                                 do (if (/= 0 (nth-nest content (list x y)))
                                                        (set-nth-nest content (x y)
                                                                      (1+ (nth-nest content (list x y))))))))
                 (adjust-energy content flag))
               content)
             (all-flash (content)
               (apply #'+ (mapcar (lambda (l) (length (remove-if-not #'zerop l))) content))))
      (loop
        with result = 0
        with this-step-flash = 0
        for s from 1 to step 
        do (setf content (add-1 content))
        do (setf content (adjust-energy content t))
        do (setf this-step-flash (all-flash content))
        do (incf result this-step-flash)
        if (= 100 this-step-flash)
          return (values result s)
        finally (return (values result nil))
        ))))
