(ns adventofcode.day2)

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])


(defn str-to-int [str]
  (map #(Integer/parseInt %) (str/split str #"\t+")))

(defn find-different [num-seq]
  (#(- (last %) (first %)) (sort num-seq)))

(defn solution-demo [path]
  (with-open [file (io/reader path)]
    (reduce + (map find-different (map str-to-int (line-seq file))))))

;; pretty one below
(defn solution [path]
  (with-open [file (io/reader path)]
    (reduce + (->> file
                   (line-seq)
                   (map str-to-int)
                   (map find-different)))))


(solution "./day2.input")
