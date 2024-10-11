;;; use this one
(ql:quickload '("str" "alexandria" "split-sequence" "cl-ppcre" "cl-heap" "trivia"))

(defun read-file-by-line (filepath)
  "read file line by line, return a list of file"
  (uiop:read-file-lines filepath)
  )

;;;;;;;;;;;;;;
;; Hash set below
;;;;;;;;;;;;;;

(defstruct (hash-set (:conc-name set-)
                     (:copier nil))
  "hash set"
  (inner (make-hash-table :test 'equal) :type hash-table :read-only t))

(defun copy-hash-set (set)
  (make-hash-set :inner (alexandria:copy-hash-table (set-inner set) :test 'equal)))

(defun set-insert (set &rest eles)
  "insert elements inside hashset, side effect: change the set"
  (declare (hash-set set))
  (dolist (ele eles)
    (setf (gethash ele (set-inner set)) t)))

(defun set-delete (set &rest eles)
  "delete the elements from set"
  (declare (hash-set set))
  (loop for ele in eles
        do (remhash ele (set-inner set))))

(defun set-get (set ele)
  "Get element from hash set, return nil if there is nothing.
And don't change the set inside"
  (declare (hash-set set))
  (if (gethash ele (set-inner set)) ele nil))

(defun hash-set-difference (set other)
  "return set - other list"
  (loop for k being the hash-keys of (set-inner set)
        if (not (gethash k (set-inner other)))
          collect k))

(defun make-hash-set-from-list (l)
  "as the function name says"
  (let ((s (make-hash-set)))
    (apply #'set-insert s l)
    s))

(defun set-to-list (set)
  "as the function name say"
  (loop for k being the hash-keys of (set-inner set)
        collect k))

(defun list-to-set (eles)
  "make new set with eles (list of ele)"
  (let ((s (make-hash-set)))
    (dolist (ele eles)
      (setf (gethash ele (set-inner s)) t))
    s))

(defun set-union (set other)
  "union sets, return new set"
  (make-hash-set-from-list
   (append (loop for k being the hash-keys of (set-inner other)
                 collect k)
           (loop for k being the hash-keys of (set-inner set)
                 collect k))))

(defun set-intersection-p (s1 s2)
  "if two sets share even signle one element"
  (loop for k being the hash-keys of (set-inner s1)
        if (gethash k (set-inner s2))
          return t
        finally (return nil)))

(defun set-count (set)
  "return how many elements in set"
  (hash-table-count (set-inner set)))

(defun set-same (set other)
  "return boolen that if two sets are same"
  (and (= (set-count set)
          (set-count other))
       (loop for k being the hash-keys of (set-inner set)
             when (not (gethash k (set-inner other)))
               return nil
             finally (return t))))

;;;;;;;;;;;;;;;;;
;; some list helper function
;;;;;;;;;;;;;;;;;

(defun list-of-sum-rest (l)
  "give list and make a list which every elements are the
all rest original elements sum"
  (loop
    with sum = 0
    for n in (reverse l)
    collect (incf sum n) into result
    finally (return (reverse result))))

(defun nth-nest (l coorp)
  "nth list by indexes in coorp one by one"
  (loop for i in coorp
        when (or (< i 0) (> i (length l)))
          return nil
        do (setf l (nth i l))
        finally (return l)))

(defun chunk-list (list n)
  "chunk list to a list of serveral n elements list"
  (loop
	with x = 0
	for end = (+ x n)
	if (< end (length list))
	  collect (subseq list x end) into new
	  and do (setf x end)
	else
	  collect (subseq list x) into new
	  and do (return new)
	end))

(defun rotate-list (l offset)
  "rotate list to right (positive offset) or left (negative offset)"
  (let ((module-offset (mod (abs offset) (length l)))
        (l (copy-list l)))
    (if (<= offset 0)
        (append (nthcdr module-offset l)
                (subseq l 0 module-offset))
        (append (subseq l (- (length l) module-offset))
                (subseq l 0 (- (length l) module-offset))))))

;;;;;;;;;;;;;;;;
;; some str helper function below
;;;;;;;;;;;;;;;;

(defun str-split-all (ls str &rest arg)
  "split str with k in ls
&key omit-nulls limit start end"
  (let ((result (list str)))
    (dolist (s ls result)
      (setf result
            (alexandria:flatten
             (mapcar (lambda (str)
                       (apply #'str:split s str arg))
                     result)))
      )))

;;;;;;;;;;;;;;;;;;
;; AOC stuff
;;;;;;;;;;;;;;;;;;

;; (defvar *aoc-session* nil "aoc session")
;; (defvar *aoc-year* (nth 5 (multiple-value-list (get-decoded-time))) "aoc year")

;; (defun download-input (day-num &key (session *aoc-session*) input-file-path)
;;   "Get the {day-num} input. Maybe write to input-file-path.
;; Need the session in cookie for authorizing."
;;   (let ((out (make-string-output-stream))
;; 		content)
;; 	(sb-ext:run-program "curl"
;; 						(list "-sL"
;; 							  "-H"
;; 							  (format nil "cookie: session=~a" session)
;; 							  (format nil "https://adventofcode.com/~a/day/~a/input" *aoc-year* day-num)
;; 							  )
;; 						:search t
;; 						:output out)
	
;; 	(setf content (get-output-stream-string out))
	
;; 	(if input-file-path
;; 		(with-open-file (s input-file-path :direction :output :if-does-not-exist :create)
;; 		  (format s content)))
	
;; 	content
;; 	))

;; (download-input 1 :session "lalalalalal" :input-file-path "../aoc2022/inputs/day1.input")

;;;;;;;;;;;;;;;;;
;; AOC map below
;;;;;;;;;;;;;;;;;

(defstruct (aoc-map (:conc-name amap-))
  "the map that generated by aoc input, 2D"
  raw-map ;; row map
  row-num
  col-num
  ele-frequency ;; element frequency of all elements in map
  ele-coops ;; pair map that (row col) => element
  coop-ele ;; pair map that element => (row col)
  ;;:= meybe need infinity here
  )

(defun gen-aoc-map (input &key is-cols ele-frequency ele-coops coop-ele line-op)
  "gen aoc map from input (two dimensions list)

--> col number from 0 to col length
|
V  row number from 0 to row length
"
  (let ((input (if line-op (mapcar line-op input) input)))
    (labels ((cols-to-rows (cols)
               (loop for r from 0 below (length (car cols))
                     collect (loop for col in cols collect (nth r col)))))
      (do* ((rest (if is-cols (cols-to-rows input) input) (cdr rest))
            (line (car rest) (car rest))
            (row-n 0 (1+ row-n))
            raw-map
            (ele-frequency (if ele-frequency (make-hash-table :test 'equal)))
            (ele-coops (if ele-coops (make-hash-table :test 'equal)))
            (coop-ele (if coop-ele (make-hash-table :test 'equal))))

           ((not rest) (make-aoc-map :raw-map raw-map
                                     :row-num (length raw-map)
                                     :col-num (length (car raw-map))
                                     :ele-frequency ele-frequency
                                     :ele-coops ele-coops
                                     :coop-ele coop-ele))
        
        (loop
          for col-n upfrom 0
          for ele in line
          collect ele into result
          if ele-frequency
            do (incf (gethash ele ele-frequency 0))

          if ele-coops
            do (setf (gethash ele ele-coops)
                     (append (gethash ele ele-coops) (list (list row-n col-n))))

          if coop-ele
            do (setf (gethash (list row-n col-n) coop-ele)
                     ele)

          finally (setf raw-map (append raw-map (list result)))
          )))))

(defun get-aoc-map-rows-len (map)
  (amap-row-num map))

(defun get-aoc-map-cols-len (map)
  (amap-col-num map))

(defun get-aoc-map-row (map row)
  (if (<= 0 row (1- (get-aoc-map-rows-len map)))
      (nth row (amap-raw-map map))
      nil))

(defun get-aoc-map-row-with-coops (map row)
  "return (ele row col)"
  (if (<= 0 row (1- (get-aoc-map-rows-len map)))
      (loop for e in (nth row (amap-raw-map map))
            for col-n upfrom 0
            collect (list e row col-n))
      nil))

(defun get-aoc-map-rows (map rows)
  (loop for row in rows
        collect (get-aoc-map-row map row)))

(defun get-aoc-map-rows-with-coops (map rows)
  (loop for row in rows
        collect (get-aoc-map-row-with-coops map row)))

(defun get-aoc-map-col (map col)
  (if (<= 0 col (1- (get-aoc-map-cols-len map)))
      (loop for row in (amap-raw-map map)
            collect (nth col row))
      nil))

(defun get-aoc-map-col-with-coops (map col)
  "return (ele row col)"
  (if (<= 0 col (1- (get-aoc-map-cols-len map)))
      (loop for row in (amap-raw-map map)
            for rn upfrom 0
            collect (list (nth col row) rn col))
      nil))

(defun get-aoc-map-cols (map cols)
  (loop for col in cols
        collect (get-aoc-map-col map col)))

(defun get-aoc-map-cols-with-coops (map cols)
  (loop for col in cols
        collect (get-aoc-map-col-with-coops map col)))

(defun get-aoc-map-ele (map coop)
  (if (amap-coop-ele map)
      (gethash coop (amap-coop-ele map))
      (nth-nest (amap-raw-map map) coop)))

(defun print-raw-map (map)
  (format t "~{~{~a~}~%~}~%" (amap-raw-map map)))

(defun aoc-map-beyond-the-range (map coop)
  (not (and (<= 0 (car coop) (1- (get-aoc-map-rows-len map)))
            (<= 0 (cadr coop) (1- (get-aoc-map-cols-len map))))))

(defun transpose-aoc-map (map)
  (let ((raw-map (amap-raw-map map))
        (ele-frequency (if (amap-ele-frequency map) t))
        (ele-coops (if (amap-ele-coops map) t))
        (coop-ele (if (amap-coop-ele map) t)))
    (gen-aoc-map raw-map
                 :ele-frequency ele-frequency
                 :ele-coops ele-coops
                 :coop-ele coop-ele
                 :is-cols t)))

(defun set-aoc-map-ele (map coop ele)
  "use set-aoc-map-ele set the ele because this will also change some table records 
in map"
  (let ((old-ele (nth (cadr coop) (nth (car coop) (amap-raw-map map)))))
    (setf (nth (cadr coop) (nth (car coop) (amap-raw-map map))) ele)
    
    (if (amap-coop-ele map)
        (setf (gethash coop (amap-coop-ele map)) ele))

    (if (amap-ele-coops map)
        (progn (setf (gethash old-ele (amap-ele-coops map))
                     (remove coop (gethash old-ele (amap-ele-coops map)) :test #'equal))
               (push coop (gethash ele (amap-ele-coops map)))))

    (if (amap-ele-frequency map)
        (progn (decf (gethash old-ele (amap-ele-frequency map)))
               (incf (gethash ele (amap-ele-frequency map) 0))))
    ))

;;; test
;; (let ((m (gen-aoc-map '((0 1 2) (3 4 5))
;;                       :ele-coops t
;;                       :coop-ele t
;;                       :ele-frequency t)))
;;   (set-aoc-map-ele m '(0 1) 10)
;;   (print-raw-map m)
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-ele-coops m)))
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-coop-ele m)))
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-ele-frequency m)))

;;   (set-aoc-map-eles m '(((0 1) 1) ((0 0) 10)))
;;   (print-raw-map m)
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-ele-coops m)))
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-coop-ele m)))
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-ele-frequency m)))

;;   (set-aoc-map-eles m '(((0 0) 1) ((0 2) 1)))
;;   (print-raw-map m)
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-ele-coops m)))
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-coop-ele m)))
;;   (format t "~a~%" (alexandria:hash-table-alist (amap-ele-frequency m))))

(defun set-aoc-map-eles (map coop-eles)
  "coop-ele => (((r c) ele) ...)"
  (loop for (coop ele) in coop-eles
        do (set-aoc-map-ele map coop ele)))

(defun set-aoc-map-row (map row-num new-row)
  "set the whole row-num row with new new-row. index from 0"
  (assert (= (length new-row) (get-aoc-map-cols-len map))
          ()
          "new row has to be the same count of elements as col number ~a"
          (get-aoc-map-cols-len map))
  (loop for c from 0 below (get-aoc-map-cols-len map)
        ;; set-aoc-map-ele will change some other record table inside
        do (set-aoc-map-ele map (list row-num c) (nth c new-row))))

(defun set-aoc-map-col (map col-num new-col)
  "set the whole col-num col with new new-col. index from 0"
  (assert (= (length new-col) (get-aoc-map-rows-len map))
          ()
          "new col has to be the same count of elements as row number ~a"
          (get-aoc-map-rows-len map))
  (loop for r from 0 below (get-aoc-map-rows-len map)
        do (set-aoc-map-ele map (list r col-num) (nth r new-col))))

;;;;;;;;;;;;;;;;;
;; AOC graph below
;;;;;;;;;;;;;;;;;

(defstruct (aoc-graph (:conc-name agraph-))
  "the graph"
  ;; 'directed or 'undirected
  graph-type 

  ;; id -> (list (list other-id weight) *)
  table)

(defun make-graph (&key (graph-type 'directed))
  (let ((table (make-hash-table :test 'equal)))
    (make-aoc-graph :graph-type graph-type
                    :table table)))

(defun insert-graph-node (graph id other-id weight)
  "id --weight-> other-id. other-id isn't ensure unique"
  (cond ((eq 'directed (agraph-graph-type graph))
         (progn
           (unless (gethash id (agraph-table graph))
             (setf (gethash id (agraph-table graph))
                   '()))
           (push (list other-id weight) (gethash id (agraph-table graph)))))
        ((eq 'undirected (agraph-graph-type graph))
         (progn
           (unless (gethash id (agraph-table graph))
             (setf (gethash id (agraph-table graph))
                   '()))
           (unless (gethash other-id (agraph-table graph))
             (setf (gethash other-id (agraph-table graph))
                   '()))
           (push (list other-id weight) (gethash id (agraph-table graph)))
           (push (list id weight) (gethash other-id (agraph-table graph)))))
        ))

(defun get-all-nodes-of-id (graph id)
  "get all nodes of id in array. unsorted"
  (gethash id (agraph-table graph)))

(defun dijkstra (graph start-id end-id &key (sort-fun #'<))
  "graph has to be id -> (id value). The id must be can get with #'first"
  (let (;; other id to start-id distance 
        (distance-table (make-hash-table :test 'equal))
        (set (make-hash-set)))

    ;; start to self is 0
    (setf (gethash start-id distance-table) 0)
    
    (do* ((this start-id)
          
          (next-round (make-instance 'cl-heap:binary-heap
                                     :sort-fun sort-fun
                                     :key #'second))
          
          (this-connected-nodes (get-all-nodes-of-id graph this)
                                (get-all-nodes-of-id graph this))
          
          (this-to-start-value (gethash this distance-table)
                               (gethash this distance-table)))
         
         ((equal this end-id)
          this-to-start-value)
      
      ;;(format t "this is ~a, heap is ~a~%" this (slot-value next-round 'cl-heap::data))
      (if (set-get set this)
          (progn ;;(format t "this ~a has already visited, pass~%" this)
                 nil)
          (loop for (id v) in this-connected-nodes
                unless (set-get set id)
                  if (or (not (gethash id distance-table))
                         (> (gethash id distance-table) (+ this-to-start-value v)))
                    do (setf (gethash id distance-table)
                             (+ this-to-start-value v))
                    and do (cl-heap:add-to-heap next-round
                                                (list id (+ this-to-start-value v)))))
      
      ;;(format t "new heap is ~a~%" (slot-value next-round 'cl-heap::data))
      ;; visited
      (set-insert set this)
      
      ;; update the id
      (setf this (first (cl-heap:pop-heap next-round)))
      )))

;; example from wiki
;; (let ((g (make-graph :graph-type 'undirected)))
;;   (insert-graph-node g 1 2 7)
;;   (insert-graph-node g 1 6 14)
;;   (insert-graph-node g 1 3 9)
;;   (insert-graph-node g 2 3 10)
;;   (insert-graph-node g 2 4 15)
;;   (insert-graph-node g 3 6 2)
;;   (insert-graph-node g 3 4 11)
;;   (insert-graph-node g 6 5 9)
;;   (insert-graph-node g 4 5 6)
;;   (dijkstra g 1 5))


;;;;;;;;;;;;;;;
;; some macros
;;;;;;;;;;;;;;;

(defun lambda-list-to-argument (ll)
  "ll is the lambda-list, transf to arguments"
  (do* ((rest ll)
        flag
        arguments)
       ((not rest) (reverse arguments))
    (cond ((eq '&optional (car rest))
           (setf flag 'optional
                 rest (cdr rest)))
          ((eq '&key (car rest))
           (setf flag 'key
                 rest (cdr rest)))
          ((eq '&rest (car rest))
           (setf flag 'rest
                 rest (cdr rest)))
          )

    (case flag
      (optional (push (car rest) arguments))
      (key
       (push (intern (symbol-name (car rest)) "KEYWORD") arguments)
       (push (car rest) arguments))
      (rest nil)
      (t (push (car rest) arguments)))
    
    (setf rest (cdr rest))
    ))

(defmacro lru (cache-syms func-declare)
  (if (not (eq 'defun (car func-declare))) (error "only for function"))
  (let ((table-name (gensym))
        (shadow-func-name (gensym))
        (cache-v (gensym)))
    `(let ((,table-name (make-hash-table :test 'equal)))
       (defun ,shadow-func-name ,@(cddr func-declare))
       ,(let ((cut-pos 3))
          (loop while
                (cond ((stringp (nth cut-pos func-declare))
                       (incf cut-pos)
                       t)
                      ((eq 'declare (car (nth cut-pos func-declare)))
                       (incf cut-pos)
                       t)
                      (t nil)))
          (append (subseq func-declare 0 cut-pos)
                  (list `(let ((,cache-v (gethash (list ,@cache-syms) ,table-name)))
                           (if ,cache-v (return-from ,(nth 1 func-declare) ,cache-v))
                           (setf ,cache-v (apply #',shadow-func-name
                                                 (list ,@(lambda-list-to-argument (nth 2 func-declare))))
                                 (gethash (list ,@cache-syms) ,table-name) ,cache-v)
                           ,cache-v)))
          ))))

;; (defun expand-match-branch (str block patterns forms)
;;   (case patterns
;;     ((t 'otherwise) `(return-from ,block (progn ,@forms)))
;;     (t (loop with regex = '("^")
;;              and vars = '()
;;              and ind = 0
;;              for x in patterns
;;              do (cond ((stringp x)
;;                        (push x regex))
;;                       ((symbolp x)
;;                        (push "(.*)" regex)
;;                        (push (list x ind) vars)
;;                        (incf ind))
;;                       (t (error "only symbol and string allowed in patterns")))
;;              finally (push "$" regex)
;;              finally (setf vars (reverse vars))
;;              finally (return (let ((whole-str (gensym))
;;                                    (regs (gensym)))
;;                                `(multiple-value-bind (,whole-str ,regs)
;;                                     (cl-ppcre:scan-to-strings
;;                                      ,(apply #'str:concat (reverse regex))
;;                                      ,str)
;;                                   (declare (ignore ,whole-str))
;;                                   (when ,regs
;;                                     (let ,(loop for (v ind) in vars
;;                                                 unless (string= (symbol-name v) "_")
;;                                                   collect v)
;;                                       ,@(loop for (v ind) in vars
;;                                               unless (string= (symbol-name v) "_")
;;                                                 collect `(setf ,v (elt ,regs ,ind)))
;;                                       (return-from ,block
;;                                         (progn ,@forms)))))))))))

;; (defmacro string-match (str &body match-branches)
;;   (let ((block-sym (gensym)))
;;     `(block ,block-sym
;;        ,@(loop for statement in match-branches
;;                collect (expand-match-branch
;;                         str
;;                         block-sym
;;                         (nth 0 statement)
;;                         (cdr statement))))))

;; (str-match "a1c5b"
;;   (("a" b "c") (parse-integer b))
;;   (("a" x "c" y "b") (print (parse-integer x)) (print (parse-integer y)) (list (parse-integer x) (parse-integer y)))
;;   (t (print "aa")))

;; (trivia:match "a1c5b"
;;   ((string "a" b "c") (parse-integer b))
;;   ((string "a" x "c" y "b") (print (parse-integer x)) (print (parse-integer y)) (list (parse-integer x) (parse-integer y)))
;;   (t (print "aa")))

;; (trivia:match #(1 2 3)
;;   ((vector _ x _)
;;    x))

;; (trivia:match '(1 2 3)
;;   ((list* 1 x _)
;;    x)
;;   ((list* _ x)
;;    x)) ;; => 2

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shoelace (all-points)
  "need the same start and end points in all-points"
  (labels ((matrix-op (a b)
             (- (* (car a) (cadr b))
                (* (cadr a) (car b)))))
    (/ (loop for (a b) on all-points
             while b
             sum (matrix-op a b))
       2)))
