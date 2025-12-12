(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day12.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day12_demo.input"))

(defun parse-input (input)
  (let (shapes
        regions
        (split-input (split-sequence:split-sequence "" input :test #'string=)))
    (setf split-input (append (butlast split-input)
                              (loop for i in (car (last split-input))
                                    collect (list i))))
    (loop with input = split-input
          for line in input
          collect (str:match (first line)
                    ((_ ":")
                     (push (gen-aoc-map
                            (mapcar (lambda (x)
                                      (mapcar #'string (concatenate 'list x)))
                                    (cdr line))
                            :ele-frequency t :ele-coops t :coop-ele t)
                           shapes))
                    ((c "x" r ": " rest)
                     (push (list (gen-aoc-map
                                  (loop for rr from 0 below (parse-integer r)
                                        collect (loop for cc from 0 below (parse-integer c)
                                                      collect "."))
                                  :ele-frequency t :ele-coops t :coop-ele t)
                                 (mapcar #'parse-integer (str:split " " rest)))
                           regions))))

    (values (reverse shapes) (reverse regions))))

;;(parse-input *input-demo*)
;;(parse-input *input*)

(defun area-of-map (map)
  (* (get-aoc-map-cols-len map) (get-aoc-map-rows-len map)))

(defun area-of-shape (shape)
  (length (get-aoc-map-coops-of-ele shape "#")))

(defun day12 (input)
  (multiple-value-bind (shapes regions)
      (parse-input input)
    (loop for region in regions
          ;;do (format t "~a~%" (second region))
          count (< (loop with shape-list = (second region)
                         with shape-len = (length shape-list)
                       
                         for ind from 0 below shape-len
                         unless (zerop (nth ind shape-list))
                           ;;do (format t "ind: ~a, num: ~a, shape: ~a~%" ind (nth ind shape-list) (nth ind shapes))
                           sum (* (nth ind shape-list) (area-of-shape (nth ind shapes))))
                   (area-of-map (first region))))))

(defun parse-input-2 (input)
  (let (shapes
        regions
        (split-input (split-sequence:split-sequence "" input :test #'string=)))
    (setf split-input (append (butlast split-input)
                              (loop for i in (car (last split-input))
                                    collect (list i))))
    (loop with input = split-input
          for line in input
          collect (str:match (first line)
                    ((_ ":")
                     (push (loop for x in (cdr line)
                                 for xx = (concatenate 'list x)
                                 sum (count #\# xx))
                           shapes))
                    ((c "x" r ": " rest)
                     (push (list (* (parse-integer c) (parse-integer r))
                                 (mapcar #'parse-integer (str:split " " rest)))
                           regions))))
    (values (reverse shapes) (reverse regions))))

;;(parse-input-2 *input-demo*)

(defun day12 (input)
  (multiple-value-bind (shapes regions)
      (parse-input-2 input)
    (loop for region in regions
          count (< (loop with shape-list = (second region)
                         with shape-len = (length shape-list)
                       
                         for ind from 0 below shape-len
                         unless (zerop (nth ind shape-list))
                           sum (* (nth ind shape-list) (nth ind shapes)))
                   (first region)))))
