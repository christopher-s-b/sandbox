(ns sicp.ch2)

(defn count-leaves [tree]
  (if (empty? tree)
    0
    (let [left (first tree)
          right (rest tree)]
      (if (not (pair? left))
        1
        (+ (count-leaves left) (count-leaves right))))))
(assert (= 3 (length [1 2 3])))

;; how can we make this tail recursive? use the iter pattern

(defn length-2 [items]
  (defn iter [count items]
    (if (empty? items)
      count
      (recur (inc count) (rest items)))) ; tail recursive
  (iter 0 items))
(assert (= 3 (length-2 [1 2 3])))

;; clojure provides a special form to make iteration more idiomatic

(defn length-3 [items]
  (loop [count 0 items items]
    (if (empty? items)
      count
      (recur (inc count) (rest items))))) ; tail recursive
(assert (= 3 (length-3 [1 2 3])))



(def x [[1 2] [3 4]])

(defn fringe [x]
  (if (empty? (rest x))
    (first x)
    (recur (rest x))))