(ns fizzbuzz.monads4
  (:use [clojure.algo.monads :as m]))


;; for loop
;; for loop with errors (how is this different than exceptions?)
;; for loop with retries (cont-m)
;; lazy or parallel for loop (does cont-m help?)

(defn incrange
  ([end] (incrange 1 end))
  ([start end] (range start (+ 1 end))))

(defn safe-div [x y]
  (m/domonad m/maybe-m
           [a x
            b y
            :when (not (zero? b))]
                (/ a b)))

(m/domonad (m/sequence-t m/maybe-m)
           [y (map #(if (odd? %) % nil) (incrange -5 5))]
  (+ 1 y))
