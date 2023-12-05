(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day5.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day5_demo.input"))

(defun parse-input (input)
  (let (inner-input
        seeds)

    (setf inner-input
          (loop
            with result = nil
            and cache = nil
            for line in input
            if (string= "" line)
              do (push (reverse cache) result)
              and do (setf cache nil)
            else
              do (push line cache)
            finally (progn (push (reverse cache) result)
                           (return (reverse result)))))
    ;;(format t "~a~%" inner-input)
    (setf seeds (mapcar #'parse-integer
                        (cl-ppcre:all-matches-as-strings "\\d+" (caar inner-input))))
    
    (append (list seeds)
            (loop for x in (cdr inner-input)
                  collect (loop for values in (cdr x)
                           for (ta s len) = (mapcar #'parse-integer
                                                    (cl-ppcre:all-matches-as-strings "\\d+" values))
                           collect (list ta s len)
                           )))))

(defun day5 (input)
  (let* ((input (parse-input input))
         (seeds (car input))
         )
    (loop for seed in seeds
          minimize (loop for map in (cdr input)
                         do  (setf seed
                                   (loop for (ta s len) in map
                                         if (<= s seed (+ s len))
                                           return (+ ta (- seed s))
                                         finally (return seed)))
                         finally (return seed)))))

(defun day5-2 (input)
  (let* ((input (parse-input input))
         seeds 
         )
    (setf seeds
          (remove-duplicates
           (loop for (start len) on (car input) by #'cddr
                 append (loop for x from 0 below len collect (+ x start)))))
    seeds
    
    ;; (loop for seed in seeds
    ;;       minimize (loop for map in (cdr input)
    ;;                      do  (setf seed
    ;;                                (loop for (ta s len) in map
    ;;                                      if (<= s seed (+ s len))
    ;;                                        return (+ ta (- seed s))
    ;;                                      finally (return seed)))
    ;;                      finally (return seed)))
  ))

