(ns adventofcode.day15
  )

(def lazy-dna-a (iterate #(next-dna 16807) 699))
(def lazy-dna-b (iterate #(next-dna 48271) 124))

(defn next-dna [factor]
  (partial (fn [factor num] (mod (* factor num) 2147483647)) factor))

(defn match? [str1 str2]
  )

(defn solution [])
