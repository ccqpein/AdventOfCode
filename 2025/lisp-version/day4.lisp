(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day4.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day4_demo.input"))

(defun parse-input (input)
  (gen-aoc-map
   (loop for line in input
         collect (concatenate 'list line))
   :ele-frequency t :ele-coops t :coop-ele t))

(defun ele-count (l)
  (loop for e in l
        when e
          if (char= e #\@)
            count 1))

(defun clean-map (m coops)
  (loop for coop in coops
        do (set-aoc-map-ele m coop #\.))
  m)

(defun day4 (input &optional part2)
  (do ((m (parse-input input) (clean-map m clean-stack))
       (clean-stack '(1))
       all-cleaned
       end)
      ((or (null clean-stack) end) (length all-cleaned))
    (setf clean-stack
          (loop for r from 0 below (get-aoc-map-rows-len m)
                append (loop for c from 0 below (get-aoc-map-cols-len m)
                             when (char= #\@ (get-aoc-map-ele m (list r c)))
                               if (< (ele-count (mapcar (lambda (x) (get-aoc-map-ele m x))
                                                        (aoc-map-around-coop m (list r c)
                                                                             :dir 'around
                                                                             :can-beyond-range t)))
                                     4)
                                 collect (list r c)))

          all-cleaned (append all-cleaned clean-stack))

    (if (not part2) (setf end t))))
