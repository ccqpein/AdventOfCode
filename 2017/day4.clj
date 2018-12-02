(ns adventofcode.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn solution [path]
  (with-open [file (io/reader path)]
    (->> file 
         (line-seq)
         (map #(str/split % #" "))
         (map #(= (count %) (count (distinct %))))
         (frequencies)
         )))

(solution "./day4.input")
