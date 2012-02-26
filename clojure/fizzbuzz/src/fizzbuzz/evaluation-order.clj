(ns fizzbuzz.core)

(defn new-if [pred then else]
  (cond pred then
        :else else))

(assert (new-if true true false))
(assert (new-if false false true))

(defn square [x] (* x x))

(defn abs [x]
  (if (> x 0)
    x
    (- x)))

(defn avg [x y]
  (/ (+ x y) 2))
(assert (= (avg 7 9) 8))

(defn improve [guess x]
  (avg guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x] (sqrt-iter 1.0 x))

(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt2 [x] (sqrt-iter 1.0 x))