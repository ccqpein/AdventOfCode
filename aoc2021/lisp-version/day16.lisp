(load "../../2020/tools.lisp")

(defstruct packet
  version
  type-id
  literal-value
  sub-packets)

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
          ;;do (print (make-packet-from-input (hex-parse l)))
          ;;do (format t "this input ~a~%" l)
          ;;do (format t "~a~%" (make-packet-from-input (hex-parse l)))
          do (format t "part1: ~a~%" (part1 (make-packet-from-input (hex-parse l)))))
    ))


(defun make-packet-from-input (input)
  (let ((version (str:substring 0 3 input))
        (type-id (str:substring 3 6 input))
        literal-value
        (sub-packets '())
        )
    (setf input (subseq input 6))
    ;;(format t "version: ~a, type-id: ~a, rest input: ~a~%" version type-id input)
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
                      (format t "need ~a length packet~%" next-p-length)
                      ;;(format t "length now ~a~%" (length input))
                      ;;(format t "target length ~a~%" target-len)
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
                      (format t "need ~a packet~%" next-p-num)
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
             :sub-packets sub-packets
             )
            input)))

(defun part1 (pa)
  (+ (parse-integer (packet-version pa) :radix 2)
     (loop for p in (packet-sub-packets pa)
           sum (part1 p)))
  )

;;:= TODO
(defun part2 (pa)
  (case (packet-type-id pa)
    ("000")
    ("001")
    ("010")
    ("011")
    ("100")
    ("101")
    ("110")
    ("111")))
  
