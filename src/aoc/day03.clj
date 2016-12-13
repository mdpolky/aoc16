(ns aoc.day03
  (:require [clojure.string :as cs]))

(def input (slurp "resources/day03.txt"))

;; ([3 4 5] ...)
(def puzzle
  (let [fix #(->> (cs/split % #"\s+")
                 (mapv (fn[x] (Integer/parseInt x))))]
    (->> (cs/split input #"\n")
         (map cs/trim)
         (map fix))))

(defn tri-maybe
  "is it a triangle? give me any 3 sides [3 4 5] and we will find out"
  [sides]
  (let [sort (sort sides)]
    (if (< (last sort) (+ (first sort) (second sort)))
      "triangle!")))

(defn solve
  "let's count em' up and return how many triangles we got."
  []
  (->> (map tri-maybe puzzle)
       (filter #(= "triangle!" %))
       (count)))
