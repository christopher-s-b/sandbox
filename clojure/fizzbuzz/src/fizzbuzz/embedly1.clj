(ns embedly1)

;; n! means n * (n - 1) * ... * 3 * 2 * 1
;; For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800
;; Let R (n) equal the sum of the digits in the number n!
;; For example, R (10) is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
;; Find the lowest value for n where R (n) is 8001.           

(defn oneTo [n]
  (range 1 (+ n 1)))
(assert (= [1 2 3] (oneTo 3)))

(defn fac [n]
  (reduce *' (oneTo n))) ;; auto-promoting mul, overflows expected
(assert (= 3628800 (fac 10)))

(defn parseInt [s]
  (Integer/parseInt (str s))) ;; transform char to string

(defn digits [n]
  (map parseInt (str n))) ;; map over string gives chars
(assert (= [1 2 3] (digits 123)))

(defn R [n]
  (reduce + (digits (fac n))))
(assert (= 27 (R 10)))

(defn zip [a b]
  (map vector a b))
(assert (= [[1 4] [2 5] [3 6]]) (zip [1 2 3] [4 5 6]))

(prn
 (let [r (oneTo 1000)]
   (filter
    (fn [[x _]] (= x 8001))
    (zip (map R r) r))))



;; or with lazy seqs
(defn fib-pair [[a b]] [b (+ a b)])
(assert (= [5 8] (fib-pair [3 5])))
(->> [3 5] (fib-pair) (= [5 8]) (assert))
(assert (->> (fib-pair [3 5]) (= [5 8])))

;;(defn fib-gen (iterate (let [[a b]] [b a+b])))
;;(defn fib-gen [] (iterate fib-pair [1 1]))

(defn fib-gen [] (map first (iterate fib-pair [1 1])))
(assert (= [1 1 2 3 5] (take 5 (fib-gen))))

(defn fib [n] (nth (take n (fib-gen)) (dec n)))
(assert (= 55 (fib 10)))


;; (defn fib [n]
;;   (nth (map first (take n (fib-gen))) (dec n)))
;; (assert (= 5 (fib 5)))


;;(assert (= (->> (fib-gen) (take 1)) [1 1]) )
