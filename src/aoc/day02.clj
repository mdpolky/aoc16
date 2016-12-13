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
7 8 9

part 2 keypad looks like this, use the same instructions as before:

    1
  2 3 4
5 6 7 8 9
  A B C
    D"

(def input (slurp "resources/day02.txt"))

;; ([:L :U :D ...] [:U :U :D :R ...] ...)
(def puzzle
  (let [fix #(keyword (str %))]
    (->> (cs/split input #"\n")
         (map cs/trim)
         (map (fn[x] (mapv fix x))))))

;;start position
(def start [0 0])

(def start-2 [-2 0])

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
  "given a start position pos [x,y], follow a line of instructions ins, applying
  the instructions recursively until none remain. When done, return pos and the
  keyword which goes along with it."
  [pos ins]
  (if (empty? ins)
    [pos (pos->key pos)]
    (recur (move pos (first ins)) (rest ins))))

;;solved using repl
;; (make-moves start (nth puzzle 0))
;; (make-moves new-pos (nth puzzle 1)) etc.

;; let's lay out the positions for our keys on the updated keypad.
(def pos->key-2
  {[-1 -1] :A
   [0 -1] :B
   [1 -1] :C
   [0 -2] :D
   [0 2] :1
   [-1 1] :2
   [0 1] :3
   [1 1] :4
   [-2 0] :5
   [-1 0] :6
   [0 0] :7
   [1 0] :8
   [2 0] :9})

;; From position :A, valid moves are up and right, etc.
(def valid-move-2
  {:A [:U :R]
   :B [:U :D :R :L]
   :C [:L :U]
   :D [:U]
   :1 [:D]
   :2 [:D :R]
   :3 [:U :D :R :L]
   :4 [:D :L]
   :5 [:R]
   :6 [:U :D :R :L]
   :7 [:U :D :R :L]
   :8 [:U :D :R :L]
   :9 [:L]})

(defn move-2
  "check what moves are possible from current position, if possible, make the
  move, if not return where you are."
  [pos i]
  (let [pos-moves (valid-move-2 (pos->key-2 pos))
        move (ins->trans i)]
    (if (some #{i} pos-moves)
      [(+ (first pos) (first move)) (+ (second pos) (second move))]
      pos)))

(defn make-moves-2
  "similar to make-moves, positions, keys, and move function are updated."
  [pos ins]
  (if (empty? ins)
    [pos (pos->key-2 pos)]
    (recur (move-2 pos (first ins)) (rest ins))))

;;solved using repl as in first make-moves
