(ns joy.ch5)

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos [needle coll]
  (for [[i val] (index coll)
        :when (= needle val)] i))

(let [coll [:0 :1 :2 :3 :4 :5 :4]]
  (pos :4 coll))

(defn pos2 [needle coll]
  (let [pairs (filter (fn [[i val]] (= val needle))
                      (index coll))]
    (for [[i val] pairs] i)))

(let [coll [:0 :1 :2 :3 :4 :5 :4]]
  (pos2 :4 coll))
