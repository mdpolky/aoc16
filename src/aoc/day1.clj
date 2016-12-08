(ns aoc.day1
  (:require [clojure.string :as cs]))

(def input (slurp "resources/day01.txt"))

;; ([:R 2] [:L 3] ... [:L 2])
;; ([turn amount] ...)
(def puzzle (->> (cs/split input #",")
                 (map cs/trim)
                 (map #(vector
                        (-> % first str keyword)
                        (-> % (.substring 1) Integer/parseInt)))))

(def start [:N [0 0]])

;; orientation + turn -> new orientation
(def ot->o
  {[:N :R] :E
   [:N :L] :W
   [:S :R] :W
   [:S :L] :E
   [:E :R] :S
   [:E :L] :N
   [:W :R] :N
   [:W :L] :S})

(defn move-xy
  "Based on current orientation and [x,y] position, move n number of spaces in
  that direction."
  [o [x y] n]
  (case o
    :N [x (+ y n)]
    :S [x (- y n)]
    :E [(+ x n) y]
    :W [(- x n) y]))

(defn move
  "From a state, turn t and move n"
  [[o pos] t n]
  (let [no (ot->o [o t])]
    [no (move-xy no pos n)]))

(defn solve1
  "Return [orientation position] (state) from start state and set of puzzle moves."
  []
  (reduce (fn [s [t n]] (move s t n))
          start
          puzzle))

;; Only tracking turn points but now need to record all points traversed

(defn first-dup
  "Returns the first duplicate value of a sequence."
  [s]
  (loop [[f & r] s seen #{}]
    (if (and f (seen f))
      f
      (if r
        (recur r (conj seen f))))))

(defn positions
  "all positions that (mf p n) visits."
  [mxy o p n]
  (for [i (range 1 (inc n))]
    (mxy o p i)))

(defn trail
  "return all states in the move from state with turn t, position pos."
  [[o pos] t n]
  (let [no (ot->o [o t])]
    (map vector (repeat no) (positions move-xy no pos n))))

(defn solve2
  []
  (let [ps (map second (reduce (fn [ss [t n]] (concat ss (trail (last ss) t n)))
                               [start]
                               puzzle))]
    (first-dup ps)))
