(ns fizzbuzz.day2)

(->> [:lather :rinse :repeat] (cycle) (drop 2) (take 5))
(take 5 (drop 2 (cycle [:later :rinse :repeat])))

(defn fib-pair [[a b]] [b (+ a b)])
(assert (= [5 8] (fib-pair [3 5])))

(defn fib-gen [] (map first (iterate fib-pair [1 1])))
(assert (= [1 1 2 3 5] (take 5 (fib-gen))))

(defn fib [n]
  (nth (take n (fib-gen)) (dec n)))

;;(fib 5)