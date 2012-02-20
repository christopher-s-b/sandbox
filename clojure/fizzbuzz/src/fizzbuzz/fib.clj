(ns fizzbuzz.fib)

;; or with lazy seqs
(defn fib-pair [[a b]] [b (+ a b)])
(assert (= [5 8] (fib-pair [3 5])))
(->> [3 5] (fib-pair) (= [5 8]) (assert))
(assert (->> (fib-pair [3 5]) (= [5 8])))

;;(defn fib-gen [] (map first (iterate fib-pair [1 1])))
(defn fib-gen []
  (map first
       (iterate
        (fn [[a b]] [b (+ a b)])
        [1 1])))
(assert (= [1 1 2 3 5] (take 5 (fib-gen))))

(defn fib [n] (nth (take n (fib-gen)) (dec n)))
(assert (= 55 (fib 10)))

