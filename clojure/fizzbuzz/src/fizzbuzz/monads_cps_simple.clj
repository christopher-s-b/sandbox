(ns fizzbuzz.monads-cps-simple
  (:use [clojure.algo.monads :only [domonad cont-m run-cont call-cc
                                    m-when with-monad maybe-t]]))


;; biz logic

(defn square [x] (* x x))

;; traditional composition
(defn bizlogic [x]
  (let [a (square x)
        b (square a)
        c (square b)]
    c))

;; threaded composition - this is what i want my monadic driver logic to look like
(defn bizlogic-2 [x]
  (-> x (square) (square) (square)))


;; cps

(with-monad cont-m
  (def mk-cont m-result))

(defn cps-bizlogic [x]
  (with-monad cont-m
    (domonad [a (mk-cont (square x))
              b (mk-cont (square a))
              c (mk-cont (square b))]
             c)))
;; (run-cont (cps-bizlogic 3))


(defn cps-square [x] (mk-cont (* x x)))

(defn cps-bizlogic-2 [x]
  (with-monad cont-m
    (domonad [a (cps-square x)
              b (cps-square a)
              c (cps-square b)]
             c)))
;; (run-cont (cps-bizlogic-2 3))


;; does it matter that callback happens in another thread?
(defn sleep-async [ms callback]
  (future (Thread/sleep ms) (callback)))


;; build a callback that will invoke the current continuation.
(defn cps-square-async
  ""
  [x]
  (call-cc (fn [c-resume] ; invoke c to 'resume'
             (let [callback (fn []
                              (prn (str "resume:" x))
                              (run-cont (c-resume (square x))))] ; callback resumes continuation
               (sleep-async 1000 callback) ; resume after sleep
               (mk-cont nil) ; signal not to continue
               ))))

;; once you invoke this, you're done, you gave up your call stack and
;; can't return anything meaningful, just "response pending". the final operation
;; in the CPS monad must be a side effect.
;; can't skip the remaining biz logic while waiting on a value - have to return
;; something so we don't block - use maybe-m to bail out. each subsequent time
;; the async callback is called, we will get a step further.
(defn cps-bizlogic-3 [x]
  (with-monad ;cont-m
    (maybe-t cont-m)
    (domonad [a (cps-square-async x)
              _ (do (prn (str "_a:" a)) (mk-cont a))
              b (cps-square-async a)
              _ (do (prn (str "_b:" b)) (mk-cont b))
              c (cps-square-async b)
              _ (do (prn (str "_c:" c)) (mk-cont c))]
             (prn (str "end:" c))
             ;; (if c c "pending")
             )))
;; (run-cont (cps-bizlogic-3 3))



(defn cps-sleep-async [ms]
  )
