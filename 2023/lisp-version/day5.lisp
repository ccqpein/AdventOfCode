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
    (setf seeds
          (mapcar #'parse-integer
                  (cl-ppcre:all-matches-as-strings "\\d+" (caar inner-input))))
    
    (append (list seeds)
            (loop for x in (cdr inner-input)
                  collect (sort ;; sort for part2
                           (loop for values in (cdr x)
                                 for (ta s len)
                                   = (mapcar #'parse-integer
                                             (cl-ppcre:all-matches-as-strings "\\d+" values))
                                 collect (list ta s len)
                                 )
                           #'< :key #'cadr)))))

(defun day5 (input)
  (let* ((input (parse-input input))
         (seeds (car input))
         )
    (loop for seed in seeds
          minimize (loop
                     for map in (cdr input)
                     do (setf seed
                              (loop for (ta s len) in map
                                    if (<= s seed (+ s len))
                                      return (+ ta (- seed s))
                                    finally (return seed)))
                     finally (return seed)))))

;; steal from friend
(defun get-range (range map)
  (let* ((start (car range))
         (end (cadr range))
         (range '()))
    (loop
      for (tar s-start len) in map
      do (let ((s-end (+ s-start len)) ;; not including
               )
           (when (< start s-end)
             (if (< start s-start)
                 (push (list start (min s-start end)) range))
           
             (setf start (max start s-start))
             (if (> (min s-end end) start)
                 (push (list (+ start (- s-start) tar)
                             (+ (min s-end end) (- s-start) tar))
                       range))
           
             (setf start s-end)
             (if (>= start end)
                 (return)))
           ))    
    (if (< start end) (push (list start end) range))
    (reverse range)))

(defun day5-2 (input)
  (let* ((input (parse-input input))
         (seeds (loop for (start range) on (car input) by #'cddr
                      collect (list start (+ start range))))
         ranges-result)
    
    (setf ranges-result
          (loop for seed in seeds
                append (loop
                         with cache = (list seed)
                         for map in (cdr input)
                         do (setf cache
                                  (loop for seed in cache
                                        append (get-range seed map) into result
                                        finally (return result)))
                         finally (return cache))))
    (caar (sort ranges-result #'< :key #'car))))
