(ns fizzbuzz.monads
  (:use [clojure.algo.monads :only [domonad
                                    defmonad
                                    identity-m]]))

;; based on < http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/ >

;; lets implement the let form!

(let [a 1
      b (inc a)]
  (* a b))
;; very nice to work with, the computations are ordered, later computations
;; can access values from earlier computations.

;; we can express this dependency of ordering with function composition:

((fn [a]
  ((fn [b]
    (* a b)) (inc a))
  ) 1)
;; equivalent expression, with the dependencies expressed by function composition
;; instead of imperative statement order. messy to work with, because
;; statement order is backwards, difficult to read, too much going on.

(defn bind [val f]
  (f val))
;; helper function that means we can invoke a function with the argument
;; to the left and the function to the right.
;; (assert (= (range 4) (m-bind 4 range)))

(bind 1        (fn [a]
(bind (inc a)  (fn [b]
        (* a b)))))
;; now dependencies are expressed by composition, but the code reads a bit
;; more like imperative statements. clojure provides macros for this:

;; (defn identity [x] x) ;provided by clojure
(defmonad identity-monad [m-result identity
                          m-bind bind])

(domonad identity-monad
         [a  1
          b  (inc a)]
         (* a b))

;; macroexpand this, it expands to the bind expression above.
;; monads are a generalization of the let form, which allow more complex behaviors
;; by providing more complex implementations of bind


;; maybe monad gards against NPEs
(defmonad maybe-monad
  [m-result identity
   m-bind (fn [val f]
            (if (nil? val) ;; only call function if val non-nil
              nil
              (f val)))])

(domonad maybe-monad
         [a 1
          b nil]
         (+ a b)) ;; business logic doesn't need null checks

;; safe division - express a sequence of divisions without caring for error control
;; (defmonad division-monad
;;   [m-result identity
;;    m-bind (fn [val f]
;;             )])

;; (domonad division-monad
;;          [a 1, b 0]
;;          (/ a b))
