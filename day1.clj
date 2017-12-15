(require '[clojure.string :as str])

(def str-int-hashmap {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "0" 0})

(defn test1 [strChain]
  (let [origin-chars (str/split strChain #"")]
    (loop [chars (conj origin-chars (first origin-chars))
           result 0]
      (cond (empty? (rest chars))
            (print result)
            (= (first chars) (second chars))
            (recur (rest chars)
                   (+ result (get str-int-hashmap (first chars))))
            :else
            (recur (rest chars) result)))))
