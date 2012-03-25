(ns joy.ch5) ;; Fogus & Houser, Joy of Clojure

;; provided by book
(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

;; book gives this as idiomatic code
(defn pos [needle coll]
  (for [[i val] (index coll)
        :when (= needle val)] i))

(let [coll [:0 :1 :2 :3 :4 :5 :4]]
  (pos :4 coll))

;; how can i make this better, and why provide :when, given filter?
(defn pos2 [needle coll]
  (let [pairs (filter (fn [[i val]] (= val needle))
                      (index coll))]
    (for [[i val] pairs] i)))

(defn pos3 [needle coll]
  (let [pairs (filter (fn [[i val]] (= val needle))
                      (index coll))]
    (for [[i val] pairs] i)))

(let [coll [:0 :1 :2 :3 :4 :5 :4]]
  (pos3 :4 coll))

(defn pos4 [pred coll]
  (for [[i val] (index coll) :when (pred val)] i))

(let [coll [:0 :1 :2 :3 :4 :5 :4]]
  (pos4 (fn [val] ( = val :4)) coll))
