(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day9.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day9_demo.input"))

(defun parse-input (input)
  (let ((input (mapcar #'parse-integer (mapcar #'string (concatenate 'list input)))))
    (pprint input)
    (do* ((rest input (cdr rest))
          (this (car rest) (car rest))
          (flag t)
          (file-id 0)
          result)
         ((not this) result)
      (if flag
          (setf flag nil
                result (append result (loop repeat this collect file-id))
                file-id (1+ file-id))
          (setf flag t
                result (append result (loop repeat this collect nil))))
      )))

(defun merge-it (l)
  (do ((ind0 0)
       (ind1 (1- (length l)))
       result)
      ((> ind0 ind1) result)
    ;;(pprint result)
    (if (nth ind0 l)
        (setf result (append result (list (nth ind0 l)))
              ind0 (1+ ind0))
        (if (nth ind1 l)
            (setf result (append result (list (nth ind1 l)))
                  ind0 (1+ ind0)
                  ind1 (1- ind1))
            (setf ind1 (1- ind1))))))

(defun sum (l)
  (loop for ind from 0 below (length l)
        sum (* ind (nth ind l))))

(defun day9 (input)
  (sum (merge-it (parse-input (car input)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-input-2 (input)
  (let ((input (mapcar #'parse-integer (mapcar #'string (concatenate 'list input)))))
    (do* ((rest input (cdr rest))
          (this (car rest) (car rest))
          (flag t)
          (file-id 0)
          (table (make-hash-table :test 'equal))
          result)
         ((not this) (values (reverse (remove-if (lambda (c) (= 0 (first c))) result))
                             table))
      (if flag
          (setf flag nil
                result (push (list this file-id) result)
                (gethash file-id table) this
                file-id (1+ file-id))
          (setf flag t
                result (push (list this #\.) result)))
      )))

(defun new-placehold (empties file)
  (list file
        (if (> (- (first empties) (first file)) 0)
            (list (- (first empties) (first file)) #\.)
            nil)))

(defun clean (l)
  (do ((start 0)
       (end 0)
       (flag nil)
       result)
      ((>= end (length l)) result)
    (if flag
        (if (equal #\. (second (nth end l)))
            (incf end)
            (setf result (append result (list (list (loop for v in (subseq l start end)
                                                          sum (first v))
                                                    #\.)))
                  start end
                  flag nil))
        (if (equal #\. (second (nth start l)))
            (setf flag t
                  end start)
            (setf result (append result (list (nth start l)))
                  start (1+ start)
                  end start)))))

(defun remove-l (l id ind)
  (loop for v in l
        for i upfrom 0
        when v
        if (and (equal (second v) id) (> i ind))
          collect (list (first v) #\.)
        else
          collect v))

(defun sum-2 (l)
  (loop with ind = 0
        for (ph id) in l
        unless (equal id #\.)
          sum (loop for i from 0 below ph
                    sum (* (+ ind i) id))
        do (incf ind ph)))

(defun day9-2 (input)
  (multiple-value-bind (l table)
      (parse-input-2 (car input))
    (let ((largest-id (second (find-if-not (lambda (x) (equal (second x) #\.)) l :from-end t))))
      (loop for id downfrom largest-id to 0
            for file-detail = (gethash id table)
            do (loop for x from 0 below (length l)
                     for (spaces v) = (nth x l)
                     while (not (equal v id)) 
                     when (and (equal v #\.) (>= spaces file-detail))
                       do (let ((new-slots (new-placehold (nth x l) (list file-detail id))))
                            (setf l (append (subseq l 0 x)
                                            new-slots
                                            (subseq l (1+ x))))
                            (setf l (clean (remove-l l id x))))
                       and return nil)
               (format t "id: ~a~%" id))
      (sum-2 l))))

;; 12730899074790 X
;; 12781586807198 X
;; 6287317016845

;;(car (multiple-value-list (merge-it-2 (merge-it-2 (parse-input-2 "2333133121414131402")))))

