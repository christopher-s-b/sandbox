(ns fizzbuzz.core
  (:use [clojure.contrib.string :only (join)]))

(defn multiple-of? [a b]
  (zero? (mod a b)))

(defn fizzbuzz [n]
  (cond (multiple-of? n 15) "fizzbuzz"
        (multiple-of? n 3) "fizz"
        (multiple-of? n 5) "buzz"
        :else (str n)))

(defn -main [& args]
  (println 
    (join ", " (map fizzbuzz (range 1 20)))))

