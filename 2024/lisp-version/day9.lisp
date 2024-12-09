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

;;(declaim (optimize (speed 3)))

(defun parse-input-2 (input)
  (let ((input (mapcar #'parse-integer (mapcar #'string (concatenate 'list input)))))
    (do* ((rest input (cdr rest))
          (this (car rest) (car rest))
          (flag t)
          (file-id 0)          
          result)
         ((not this) (reverse (remove-if (lambda (c) (= 0 (first c))) result)))
      (if flag
          (setf flag nil
                result (push (list this file-id) result)
                file-id (1+ file-id))
          (setf flag t
                result (push (list this #\.) result))))))

(defun new-placehold (empties file)
  (let ((n (- (first empties)
              (first file))))
    (if (> n 0)
        (list file (list n #\.))
        (list file))))

(defun sum-2 (l)
  (loop with ind = 0
        for (ph id) in l
        unless (equal id #\.)
          sum (loop for i from 0 below ph
                    sum (* (+ ind i) id))
        do (incf ind ph)))

(defun day9-2 (input)
  (let* ((l (parse-input-2 (car input)))
         (largest-id (second (find-if-not (lambda (x) (equal (second x) #\.)) l :from-end t))))
    (loop with end-ind = (1- (list-length l))
          for id downfrom largest-id to 0
          do (loop for rev-ind from end-ind downto 0
                   for (file-space file-id) = (nth rev-ind l)
                   when (equal file-id id)
                     do (loop for ind from 0 below rev-ind
                              for (spaces v) = (nth ind l)
                              when (and (equal v #\.) (>= spaces file-space))
                                do (let ((new-slots (new-placehold (nth ind l) (nth rev-ind l))))
                                     (setf l (append (subseq l 0 ind)
                                                     new-slots
                                                     (subseq l (1+ ind) rev-ind)
                                                     (list `(,file-space #\.))
                                                     (subseq l (1+ rev-ind)))))
                                and return nil)
                        (setf end-ind rev-ind)
                     and return nil)
             ;;(format t "id: ~a~%" id)
          )
    (sum-2 l)))

;; 12730899074790 X
;; 12781586807198 X
;; 6287317016845
