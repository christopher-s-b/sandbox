(ns fizzbuzz.monads-cps
  (:use [clojure.algo.monads :only [domonad cont-m run-cont call-cc m-when]]))


; A simple computation performed in continuation-passing style.
; (m-result 1) returns a function that, when called with a single
; argument f, calls (f 1). The result of the domonad-computation is
; a function that behaves in the same way, passing 3 to its function
; argument. run-cont executes a continuation by calling it on identity.
(run-cont
  (domonad cont-m
    [x (m-result 1)
     y (m-result 2)]
    (+ x y)))

; Let's capture a continuation using call-cc. We store it in a global
; variable so that we can do with it whatever we want. The computation
; is the same one as in the first example, but it has the side effect
; of storing the continuation at (m-result 2).
(def continuation nil)

(run-cont
  (domonad cont-m
    [x (m-result 1)
     y (call-cc (fn [c] (def continuation c) (c 2)))]
    (+ x y)))

; Now we can call the continuation with whatever argument we want. The
; supplied argument takes the place of 2 in the above computation:
(run-cont (continuation 5))
(run-cont (continuation 42))
(run-cont (continuation -1))

; Next, a function that illustrates how a captured continuation can be
; used as an "emergency exit" out of a computation:
(defn sqrt-as-str [x]
  (call-cc
   (fn [k]
     (domonad cont-m
       [_ (m-when (< x 0) (k (str "negative argument " x)))]
       (str (. Math sqrt x))))))

(run-cont (sqrt-as-str 2))
(run-cont (sqrt-as-str -2))
