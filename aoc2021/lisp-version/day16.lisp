(load "../../2020/tools.lisp")

(defstruct packet
  version
  type-id
  literal-value
  sub-packets)

(defun make-packet-from-input (input)
  (let ((version (str:substring 0 3 input))
        (type-id (str:substring 3 6 input))
        literal-value
        (sub-packets '())
        )
    (setf input (subseq input 6))
    (setf literal-value
          (if (string= type-id "100")
              (loop
                for group = (str:substring 0 5 input)
                do (setf input (subseq input 5))
                   ;;do (print input)
                if (char= #\1 (elt group 0))
                  collect (subseq group 1) into literal-v
                else
                  collect (subseq group 1) into literal-v
                  and return (parse-integer (apply #'str:concat literal-v) :radix 2)
                )
              (let ((length-type-id (str:substring 0 1 input)))
                (setf input (subseq input 1))
                
                (if (string= "0" length-type-id)
                    (let* ((next-p-length (parse-integer (str:substring 0 15 input) :radix 2))
                           target-len)
                      (setf input (subseq input 15)
                            target-len (- (length input) next-p-length))
                      (loop
                        for this-len = (length input)
                        while (/= this-len target-len)
                        do (multiple-value-bind (pkg new-input)
                               (make-packet-from-input input)
                             (push pkg sub-packets)
                             (setf input new-input)
                             )
                        ))
                    (let ((next-p-num (parse-integer (str:substring 0 11 input) :radix 2)))
                      (setf input (subseq input 11))
                      (loop repeat next-p-num
                            do (multiple-value-bind (pkg new-input)
                                   (make-packet-from-input input)
                                 (push pkg sub-packets)
                                 (setf input new-input))))
                    ))))
    (values (make-packet
             :version version
             :type-id type-id
             :literal-value literal-value
             :sub-packets (reverse sub-packets)
             )
            input)))

(defun hex-parse (l)
  (apply #'str:concat
         (loop
           for c across l
           collect (str:pad 4 (write-to-string (parse-integer (string c)
                                                              :radix 16)
                                               :base 2)
                            :pad-side :left
                            :pad-char #\0))))

(defun day16 (path)
  (let ((content (read-file-by-line path)))
    (loop for l in content
          do (format t "part1: ~a~%" (part1 (make-packet-from-input (hex-parse l))))
          do (format t "part2: ~a~%" (part2 (make-packet-from-input (hex-parse l)))))
    ))

(defun part1 (pa)
  (+ (parse-integer (packet-version pa) :radix 2)
     (loop for p in (packet-sub-packets pa)
           sum (part1 p)))
  )

(defun part2 (pa)
  (cond 
    ((string= (packet-type-id pa) "000")
     (apply #'+ (mapcar #'part2 (packet-sub-packets pa))))
    ((string= "001" (packet-type-id pa))
     (apply #'* (mapcar #'part2 (packet-sub-packets pa))))
    ((string= "010" (packet-type-id pa))
     (apply #'min (mapcar #'part2 (packet-sub-packets pa))))
    ((string= "011" (packet-type-id pa))
     (apply #'max (mapcar #'part2 (packet-sub-packets pa))))
    ((string= "100" (packet-type-id pa))
     (packet-literal-value pa))
    ((string= "101" (packet-type-id pa))
     (apply (lambda (a b) (if (> (part2 a) (part2 b)) 1 0)) (packet-sub-packets pa)))
    ((string= "110" (packet-type-id pa))
     (apply (lambda (a b) (if (< (part2 a) (part2 b)) 1 0)) (packet-sub-packets pa)))
    ((string= "111" (packet-type-id pa))
     (apply (lambda (a b) (if (= (part2 a) (part2 b)) 1 0)) (packet-sub-packets pa)))))
  
