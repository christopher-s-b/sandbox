(ns fizzbuzz.core)

;; SICP 2.1.4 - church numerals
;; using substitution we can define `one` directly, not in terms of
;; (inc (zero)). I don't think we can define dec. I don't know how to
;; implement `zero?` or do anything useful, though SICP says we can
;; implement `+` directly, no repeated incs. Once we have a number
;; defined I don't understand how we can print it or inspect its value
;; in a useful way.

(defn zero []
  (fn [f] (fn [x] x)))

(defn inc [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(defn zero? [n]
  (= n (zero)))

;;(assert (zero? (zero))) ;; fails

(inc (zero))