(ns adventofcode.day15)

(def lazy-dna-a (iterate #(next-dna 16807) 65))
(def lazy-dna-b (iterate #(next-dna 48271) 8921))

(defn next-dna [factor]
  (partial (fn [factor num] (mod (* factor num) 2147483647)) factor))

(defn match? [str1 str2]
  (loop [lis1 (reverse (str "0000000000000000" str1))
         lis2 (reverse (str "0000000000000000" str2))
         ind 1
         flag true]
    (cond (not flag)
          false
          (= 17 ind)
          true
          (= (first lis1) (first lis2))
          (recur (rest lis1)
                 (rest lis2)
                 (+ 1 ind)
                 flag)
          :else
          (recur (rest lis1) (rest lis2) (+ 1 ind) false))))

(defn solution [dna1 dna2 num]
  (let [total (atom 0)]
    (doseq [a (take (inc num) dna1)
            b (take (inc num) dna2)]
      (print a b)
      (if (match? (Integer/toString a 2)
                  (Integer/toString b 2))
        (swap! total inc)))
    @total))
