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
 (let [r (oneTo 800)
       pairs (zip (map R r) r)]
   (filter
    (fn [[x _]] (= x 8001))
    pairs)))


