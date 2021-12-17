(load "../../2020/tools.lisp")

(defun day17 (x-range y-range)
  (let ((bucket '())
        (all-init (make-hash-set)))
    (loop for xx from 1 to (cadr x-range)
          do (loop for yy from -1000 to 1000
                   for step = 0
                   and flag = nil
                   and highest-y = 0

                   do (loop
                        with point = '(0 0)
                        do (setf point (next-point point xx yy step))
                        do (setf highest-y (max highest-y (cadr point)))
                        do (when (and (and (<= (car point) (cadr x-range))
                                           (>= (car point) (car x-range)))
                                      (and (<= (cadr point) (cadr y-range))
                                           (>= (cadr point) (car y-range))))
                             (setf flag t)
                             (set-insert all-init (list xx yy)))
                        if (or (> (car point) (cadr x-range))
                               (< (cadr point) (car y-range)))
                          return nil
                        else
                          do (incf step))
                   when flag
                     do (push highest-y bucket)))
    
    (values (apply #'max bucket)
            (set-count all-init))))

(defun next-point (point xx yy step)
  (let ((x (car point))
        (y (cadr point)))
    (list (if (<= xx step) x (+ x xx (- step)))
          (+ y yy (- step)))))
