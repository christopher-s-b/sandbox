(ns fizzbuzz.core
  (:use [clojure.contrib.string :only (join)]))



(def targets (sorted-map 3 "fizz" 5 "buzz"))

;; match any applicable factors
(defn match [n, factors]
  (filter #(= 0 (mod n %)) factors))


(defn fizzbuzz [n]
  (let [factors (keys targets)
        matches (match n factors)]
    (if (= 0 (count matches))
      (str n)
      (reduce str (map targets matches)))))

(join ", " (map fizzbuzz (range 20)))
