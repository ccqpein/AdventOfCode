(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day11.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day11_demo.input"))

(defun parse-input (input)
  (let ((g (make-graph)))
    (loop for line in input
          do (str:match line
               ((start ": " rest)
                (loop for target in (str:split " " rest)
                      do (insert-graph-node g start target 1)))))
    g))

;;(get-all-nodes (parse-input *input-demo*))

(defun find-out (g this-id visited)
  (set-insert visited this-id)
  (loop for (next . weight) in (get-all-nodes-of-id g this-id)
        if (string= next "out")
          sum 1 into res
        else if (not (set-get visited next))
               sum (find-out g next visited) into res
        finally (progn (set-delete visited this-id) (return res))))

(defun day11 (input)
  (find-out (parse-input input) "you" (make-hash-set)))

;; (defun find-out-2 (g this-id visited)
;;   (set-insert visited this-id)
;;   (loop for (next . weight) in (get-all-nodes-of-id g this-id)
;;         if (string= next "out")
;;           sum (if (and (set-get visited "dac") (set-get visited "fft")) 1 0) into res
;;         else if (not (set-get visited next))
;;                sum (find-out-2 g next visited) into res
;;         finally (progn (set-delete visited this-id) (return res))))

;; ;;(find-out-2 (parse-input *input*) "svr" (make-hash-set))

;; (defun find-out-3 (g this-id target dont-visited visited)
;;   (set-insert visited this-id)
;;   (loop for (next . weight) in (get-all-nodes-of-id g this-id)
;;         if (string= next target)
;;           collect (copy-hash-set visited) into res
;;         else if (and (not (set-get visited next))
;;                      (not (member next dont-visited :test #'string=)))
;;                append  (find-out-3 g next target dont-visited visited) into res
;;         finally (progn (set-delete visited this-id) (return res))))

;; (find-out-3 (parse-input *input-demo*) "svr" "fft" '("dac" "out") (make-hash-set))

(lru (this-id has-dac has-fft)
     (defun find-out-4 (g this-id visited has-dac has-fft)
       (set-insert visited this-id)
       (loop for (next . weight) in (get-all-nodes-of-id g this-id)
             if (string= next "out")
               sum (if (and has-dac has-fft) 1 0) into res
             else if (not (set-get visited next))
                    sum (find-out-4 g next visited
                                    (set-get visited "dac")
                                    (set-get visited "fft"))
                      into res
             finally (progn (set-delete visited this-id) (return res)))))

(defun day11-2 (input)
  (find-out-4 (parse-input input) "svr" (make-hash-set) nil nil))
