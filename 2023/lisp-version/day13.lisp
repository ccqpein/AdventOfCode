(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day13.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day13_demo.input"))

(defun parse-input (input)
  (mapcar (lambda (i)
            (gen-aoc-map i :line-op (lambda (l) (concatenate 'list l))))
          (split-sequence:split-sequence-if (lambda (s) (string= s "")) input)))

(defun ref-vertical (map &optional (offset-limit 0))
  (loop for col-n from 0 below (1- (get-aoc-map-cols-len map))
        for lefts = (loop for c from col-n downto 0
                          collect c)
        for rights = (loop for c from (1+ col-n) below (get-aoc-map-cols-len map)
                           collect c)
        when (compare-2 (get-aoc-map-cols map lefts) (get-aoc-map-cols map rights) offset-limit)
          collect col-n into result 
        finally (return (reverse result))))

(defun ref-horizontal (map &optional (offset-limit 0))
  (loop for row-n from 0 below (1- (get-aoc-map-rows-len map))
        for uppers = (loop for r from row-n downto 0
                           collect r)
        for downs = (loop for r from (1+ row-n) below (get-aoc-map-rows-len map)
                          collect r)
        when (compare-2 (get-aoc-map-rows map uppers) (get-aoc-map-rows map downs) offset-limit)
          collect row-n into result 
        finally (return (reverse result))))

(defun compare-2 (l1 l2 limit)
  (if (or (not l1) (not l2)) (error "cannot empty"))
  (do ((l1 l1 (cdr l1))
       (l2 l2 (cdr l2))
       (offset 0))
      ((or (not l1) (not l2) (> offset limit)) (if (> offset limit) nil t))
    (loop for i in (car l1)
          for j in (car l2)
          if (not (equal i j))
            do (incf offset))))

(defun day13 (input)
  (let ((maps (parse-input input))
        (funcs (list #'ref-vertical #'ref-horizontal)))
    (loop for i from 0 below (length maps)
          sum (loop for fi from 0 to 1
                    for this-func = (nth fi funcs)
                    for v = (car (funcall this-func (nth i maps) 0))
                    when v
                      sum (if (zerop fi) (1+ v) (* 100 (1+ v)))))))

(defun day13-2 (input)
  (let ((maps (parse-input input))
        (funcs (list #'ref-vertical #'ref-horizontal)))
    (loop for i from 0 below (length maps)
          for original = (loop for fi from 0 to 1
                               for this-func = (nth fi funcs)
                               for v = (car (funcall this-func (nth i maps) 0))
                               when v
                                 return (list fi v))
          for (fi nv) = (loop for fi from 0 to 1
                              for this-func = (nth fi funcs)
                              for v = (funcall this-func (nth i maps) 1)
                              append (loop for vv in v
                                           collect (list fi vv))
                                into all-vv
                              finally (return (find-if-not (lambda (vvv) (equal vvv original)) all-vv)))
          sum (if (zerop fi) (1+ nv) (* 100 (1+ nv))))))

;;(= (day13 *input*) 37561)
;;(= (day13-2 *input*) 31108)


;; (defun changed-map (map)
;;   (loop
;;     with orig-map = (amap-raw-map map)
;;     for r from 0 below (get-aoc-map-rows-len map)
;;     append (loop
;;              for c from 0 below (get-aoc-map-cols-len map)
;;              collect (let ((a (loop for l in orig-map collect (copy-list l))))
;;                        (if (char= #\# (nth c (nth r a)))
;;                            (setf (nth c (nth r a)) #\.)
;;                            (setf (nth c (nth r a)) #\#))
;;                        (gen-aoc-map a)))))

;; (defun day13-2 (input)
;;   (let ((maps (parse-input input))
;;         (funcs (list #'ref-vertical #'ref-horizontal)))
;;     (loop for i from 0 below (length maps)
;;           for original-v = (loop for fi from 0 to 1
;;                                  for this-func = (nth fi funcs)
;;                                  for v = (funcall this-func (nth i maps))
;;                                  when v
;;                                    return (list fi v))
;;           sum (loop for m in (changed-map (nth i maps))
;;                     for new-v = (loop for fi from 0 to 1
;;                                       for this-func = (nth fi funcs)
;;                                       for v = (funcall this-func m)
;;                                       if (and v (not (equal (list fi v) original-v)))
;;                                         return (list fi v)
;;                                       finally (return nil))
;;                     when new-v
;;                       return (if (zerop (car new-v)) (1+ (cadr new-v)) (* 100 (1+ (cadr new-v))))
;;                     finally (return 0)))))
