(load "../../2020/tools.lisp")

(ql:quickload "cl-heap")

(defun day15 (path part2p)
  (let ((content (loop
                   for l in (read-file-by-line path)
                   collect (mapcar #'digit-char-p (concatenate 'list l))))
        end)

    (when part2p
      (setf content (dup-content content)))

    (setf end (list (1- (length content)) (1- (length (car content)))))

    (let ((queue (make-instance 'cl-heap:priority-queue))
          (already (make-hash-set)))
      (set-insert already '(0 0))
      (cl-heap:enqueue queue '(0 (0 0)) 0) ;; (risk (x y))
      (loop
        for (risk (x y)) = (cl-heap:dequeue queue)
        when (equal (list x y) end)
          return risk
        do (loop
             for (offsetx offsety) in '((1 0) (-1 0) (0 1) (0 -1))
             when (and (>= (+ x offsetx) 0)
                       (< (+ x offsetx) (length content))
                       (>= (+ y offsety) 0)
                       (< (+ y offsety) (length (car content))))
               do (when (not (set-get already (list (+ x offsetx) (+ y offsety))))
                    (let* ((nx (+ x offsetx))
                           (ny (+ y offsety))
                           (v (nth-nest content (list nx ny))))
                      (set-insert already (list nx ny))
                      (cl-heap:enqueue queue
                                       (list (+ risk v)
                                             (list nx ny))
                                       (+ risk v)))))))))

(defun dup-content (content)
  (loop
    with chunk = (loop
                   for line in content
                   collect (append
                            line
                            (mapcar (lambda (e) (let ((ee (+ 1 e))) (if (> ee 9) (- ee 9) ee))) line)
                            (mapcar (lambda (e) (let ((ee (+ 2 e))) (if (> ee 9) (- ee 9) ee))) line)
                            (mapcar (lambda (e) (let ((ee (+ 3 e))) (if (> ee 9) (- ee 9) ee))) line)
                            (mapcar (lambda (e) (let ((ee (+ 4 e))) (if (> ee 9) (- ee 9) ee))) line)))
    for offset from 0 to 4
    append (loop
             for line in chunk
             collect (mapcar (lambda (e)
                               (let ((ee (+ offset e)))
                                 (if (> ee 9) (- ee 9) ee)))
                             line))))
