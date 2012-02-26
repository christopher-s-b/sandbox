(ns fizzbuzz.core)

;; from SICP 2.1.3
;; complex data objects can be built from
;; pairs, no need for cons/car/cdr to be special forms.
;; can we use this to be more confident about absense of
;; bugs, since we can easily test decomposed pieces?
;; turtles all the way down.

(defn cons [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(let [z (cons 1 2)]
          (assert (= 1 (car z)))
          (assert (= 2 (cdr z))))