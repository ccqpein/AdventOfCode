(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day25.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day25_demo.input"))

(defun parse-input (input)
  (let ((input (split-sequence:split-sequence-if (lambda (s) (string= s "")) input)))
    (loop for chunk in input
          collect (gen-aoc-map (loop for line in chunk
                                     collect (concatenate 'list line))
                               :ele-coops t :coop-ele t))))

(defun if-lock (map)
  (every (lambda (c) (char= c #\#)) (get-aoc-map-row map 0)))

(defun if-key (map)
  (every (lambda (c) (char= c #\#)) (get-aoc-map-row map (1- (get-aoc-map-rows-len map)))))

(defun lock-seq (m)
  (loop for c in (get-aoc-map-cols m (loop for i from 0 below (get-aoc-map-cols-len m)
                                           collect i))
        collect (1- (loop for v in c
                          while (char= v #\#)
                          count 1))))

(defun key-seq (m)
  (loop for c in (get-aoc-map-cols m (loop for i from 0 below (get-aoc-map-cols-len m)
                                           collect i))
        collect (1- (loop for v in (reverse c)
                          while (char= v #\#)
                          count 1))))


(defun the-last-dance (input)
  (let ((ms (parse-input input)))
    (let (keys
          locks
          (row-len (get-aoc-map-rows-len (car ms))))
      (loop for m in ms
            if (if-key m)
              do (push (key-seq m) keys)
            else
              do (push (lock-seq m) locks))
      (loop for l in locks
            sum (loop for k in keys
                      when (every (lambda (n) (<= n (- row-len 2))) (mapcar #'+ l k))
                        count 1)))))
