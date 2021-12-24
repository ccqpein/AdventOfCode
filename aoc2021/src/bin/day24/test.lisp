(+ (* last-z 1 (+ (* 25 (if (= (+ (mod last-z 26) 10) (nth 0 number)) 0 1))
                  1)
      )
   (* (nth 0 number)
      (if (= (+ (mod last-z 26) 10) (nth 0 number)) 0 1))
   )
;; = (nth 0 number) => 123456789

(+ (* last-z (+ (* 25 (if (= (+ (mod last-z 26) 12) (nth 1 number)) ;; (nth x number) always in [1,9], so (+ (mod last-z 26) 12) will always failed
                          0
                          1))
                1)
      )
   (* (+ (nth 1 number) 6)
      (if (= (+ (mod last-z 26) 12) (nth 1 number))
          0
          1)))

(+ (* last-z (+ (* 25 (if (= (+ (mod last-z 26) 13) (nth 2 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 2 number)) 4)
      (if (= (+ (mod last-z 26) 13) (nth 2 number)) 0 1)))

(+ (* last-z (+ (* 25 (if (= (+ (mod last-z 26) 13) (nth 3 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 3 number)) 2) (if (= (+ (mod last-z 26) 13) (nth 3 number)) 0 1)))

(+ (* last-z 1 (+ (* 25 (if (= (if (= (+ (mod last-z 26) 14) (nth 4 number)) 1 0) 0) 1 0)) 1))
   (* (+ (+ 0 (nth 4 number)) 9) (if (= (+ (mod last-z 26) 14) (nth 4 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -2) (nth 5 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 5 number)) 1) (if (= (+ (mod last-z 26) -2) (nth 5 number)) 0 1)))

(+ (* last-z (+ (* 25 (if (= (+ (mod last-z 26) 11) (nth 6 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 6 number)) 10) (if (= (+ (mod last-z 26) 11) (nth 6 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -15) (nth 7 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 7 number)) 6) (if (= (+ (mod last-z 26) -15) (nth 7 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -10) (nth 8 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 8 number)) 4) (if (= (+ (mod last-z 26) -10) (nth 8 number)) 0 1)))

(+ (* last-z  (+ (* 25 (if (= (+ (mod last-z 26) 10) (nth 9 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 9 number)) 6) (if (= (+ (mod last-z 26) 10) (nth 9 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -10) (nth 10 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 10 number)) 3) (if (= (+ (mod last-z 26) -10) (nth 10 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -4) (nth 11 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 11 number)) 9) (if (= (+ (mod last-z 26) -4) (nth 11 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -1) (nth 12 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 12 number)) 15) (if (= (+ (mod last-z 26) -1) (nth 12 number)) 0 1)))

(+ (* (/ last-z 26) (+ (* 25 (if (= (+ (mod last-z 26) -1) (nth 13 number)) 0 1)) 1))
   (* (+ (+ 0 (nth 13 number)) 5) (if (= (+ (mod last-z 26) -1) (nth 13 number)) 0 1)))
