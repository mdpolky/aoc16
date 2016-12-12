(ns aoc.day02
  (:require [clojure.string :as cs]))

"--- Day 2: Bathroom Security ---

You arrive at Easter Bunny Headquarters under cover of darkness. However, you
left in such a rush that you forgot to use the bathroom! Fancy office buildings
like this one usually have keypad locks on their bathrooms, so you search the
front desk for the code.

\"In order to improve security,\" the document you find says,
\"bathroom codes will no longer be written down. Instead, please memorize and
follow the procedure below to access the bathrooms.\"

The document goes on to explain that each button to be pressed can be found by
starting on the previous button and moving to adjacent buttons on the keypad:
U moves up, D moves down, L moves left, and R moves right.

Each line of instructions corresponds to one button, starting at the previous
button (or, for the first line, the \"5\" button); press whatever button you're
on at the end of each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk
to the bathroom. You picture a keypad like this:

1 2 3
4 5 6
7 8 9"

(def input (slurp "resources/day02.txt"))

;; ([:L :U :D ...] [:U :U :D :R ...] ...)
(def puzzle
  (let [fix #(keyword (str %))]
    (->> (cs/split input #"\n")
         (map cs/trim)
         (map (fn[x] (mapv fix x))))))

;;start position
(def start [0 0])

;;[x,y] pos->keypad key
(def pos->key
  {[-1 1]  :1
   [0 1]   :2
   [1 1]   :3
   [-1 0]  :4
   [0 0]   :5
   [1 0]   :6
   [-1 -1] :7
   [0 -1]  :8
   [1 -1]  :9})

(defn valid-move
  "return a valid move from coordinate1 by transform1"
  [c1 t1]
  (let [res (+ c1 t1)]
    (cond
      (<= 1 res) 1
      (>= -1 res) -1
      :else res)))

;; apply direction to cartesian plane.
(def ins->trans
  {:U [0 1]
   :D [0 -1]
   :L [-1 0]
   :R [1 0]})

(defn move
  "takes a start position [x,y] and a single instruction :U and returns the
  next valid position [x,y]"
  [pos i]
  (let [move (ins->trans i)
        next-x (valid-move (first pos) (first move))
        next-y (valid-move (second pos) (second move))]
    [next-x next-y]))

(defn make-moves
  [pos ins]
  (if (empty? ins)
    pos
    (recur (move pos (first ins)) (rest ins))))
