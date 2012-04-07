(ns fizzbuzz.monads
  (:use [clojure.algo.monads :only [domonad
                                    defmonad
                                    monad
                                    identity-m]]))

;; based on < http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/ >
;; http://www.learningclojure.com/2009/09/how-it-works-monad-im-currently-reading.html

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
;;
;;    (assert (= (range 4) (m-bind 4 range)))
;;
;; this bind is just function application, just makes code look different!
;; bind in python:
;;
;;    def bind(val, f): return f(val)
;;    assert bind(4, range) == range(4)
;;
;; so lets start with our let statement
;;
;;    (let [a 1
;;          b (inc a)]
;;      (* a b))
;;
;; expressed as function composition in python:
;;
;;    def first(a):
;;      def second(b):
;;        return a*b
;;      return second(a+1)
;;    first(1)
;;
;; we can refactor this using bind:
;;
;;    def first(a):
;;      def second(b):
;;        return a*b
;;      return bind(a+1, second)
;;    bind(1, first)
;;
;; we could use lambdas instead of named functions and express more concisely:
;;
;;    bind(1, fna: bind(a+1, fnb: a*b))
;;
;; a little bit of formatting:
;;
;;    bind(1, fna:
;;    bind(a+1, fnb:
;;      a*b))
;;
;; this is pretty close to our let statement in clojure:
;;
;;    (let [a 1
;;          b (inc a)]
;;      (* a b))
;;
;; if we didn't have `bind` to turn the order of function application inside out,
;; so instead of `f(val)` we have something closer to `(val)f`, our nested functions
;; would have their argument spread away from their function definition, like
;;
;;    apply( fna:
;;    apply( fnb:
;;      a*b)(a+1)(1)
;;
;; or without named functions,
;;
;;    (fna: (fnb: a*b)(a+1))(1)
;;
;; note how the argument `1` becomes `a`, which sandwiches the inner function.


;; now we are ready to do this in clojure:
(bind 1
      (fn [a] (bind (inc a)
                   (fn [b]
                     (* a b)))))

;; reformatted
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

;; < http://onclojure.com/2009/03/06/a-monad-tutorial-for-clojure-programmers-part-2/ >

;; now lets implement the for form - nested loops

(for [x (range 5)
      y (range x)]
  [x y (* x y)])    ; => ([1 0 0] [2 0 0] [2 1 2] [3 0 0] [3 1 3] [3 2 6] [4 0 0] [4 1 4] [4 2 8] [4 3 12])

(for [x (range 5)
      y (range x)]
  (* x y))          ; => (0 0 2 0 3 6 0 4 8 12)


;; attempt 1
(domonad
 (monad
  [m-result identity
   m-bind (fn [seq f] (map f seq))])
 [x (range 5), y (range x)]
 (* x y))

;; => (() (0) (0 2) (0 3 6) (0 4 8 12))
;; bind added a level of nesting.
;; Technically is a malformed monad. if `bind` adds a level of nesting,
;; `result` must remove it.

(domonad
 (monad
  [m-result (fn [val] (list val))
   m-bind (fn [seq f] (apply concat (map f seq)))])
 [x (range 5), y (range x)]
 (* x y))

;; => (() (0) (0 2) (0 3 6) (0 4 8 12))





;; safe division - express a sequence of divisions without caring for error control
;; (defmonad division-monad
;;   [m-result identity
;;    m-bind (fn [val f]
;;             )])

;; (domonad division-monad
;;          [a 1, b 0]
;;          (/ a b))
