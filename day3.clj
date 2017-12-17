(ns adventofcode.day3
  )

(defn solution [num]
  (let [circle-num (which-circle num)]
    (+ (int (/ circle-num 2))
       (left-steps num circle-num))))

(defn which-circle [num]
  (first (filter #(and (odd? %) (>= (* % %) num)) (range))))

(defn left-steps [nums circle-num]
  (let [step-num (/ (dec circle-num) 2)]
    (apply min (map #(Math/abs (- % nums))
              (range (- (* circle-num circle-num) step-num)
                     (* (- circle-num 2) (- circle-num 2))
                     (* -2 step-num))))))
