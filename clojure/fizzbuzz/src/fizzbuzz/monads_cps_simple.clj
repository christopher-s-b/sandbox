(ns fizzbuzz.monads-cps-simple
  (:use [clojure.algo.monads :only [domonad cont-m run-cont call-cc
                                    m-when with-monad maybe-t m-chain]]))


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






(defn query-async [query callback]
  (future (Thread/sleep 1000) (callback (str "response: " query))))


(defn cps-query-async [query]
  (call-cc (fn [c-resume] ; invoke c to 'resume'
             (let [callback (fn [response]
                              (run-cont (c-resume response)))] ; callback resumes continuation
               (query-async query callback) ; resume after sleep
               (mk-cont nil) ; signal not to continue
               ))))

(defn mysquare [n]
  (cps-query-async n))

(defn cps-bizlogic-4 [x finished]
  (with-monad (maybe-t cont-m)
    (domonad [a (mysquare x)
              b (mysquare a)
              c (mysquare b)]
             (finished c))))
;; (run-cont (cps-bizlogic-4 "query1" prn))






(defn cps-bizlogic-5 [x]
  (with-monad (maybe-t cont-m)
    (m-chain [mysquare mysquare mysquare])))
;; (run-cont (cps-bizlogic-5 "query1"))





;; (defn query-async [query callback]
;;   (future (Thread/sleep 1000) (callback (str "response: " query))))


(defn mk-cps
  "wrap a function `f-async` taking only a callback, suitable for use inside cont-m"
  [f-async]
  (call-cc (fn [c-resume] ;`c-resume` is a "callback" to the business logic
             (let [query-callback (fn [response] ;build an internal callback for the query function
                                                ;that will invoke `c-resume` to return to biz logic
                              (run-cont (c-resume response)))] ;when we have a response,
                                                               ;invoke `c-resume` with the response
               (f-async callback) ;execute the query, it will resume the business logic via above
                                  ;internal callback which invokes `c-resume` with the response.
               (mk-cont nil) ;signal to bail out of business logic, we don't have a response yet.
                             ;we will resume the business logic when we have a response via `c-resume`
               ))))

(defn query-something [n]
  (let [doquery (partial query-async (str "query:" n))]
    (mk-cps doquery)))

(defn cps-bizlogic-6 [x finished-callback]
  (with-monad (maybe-t cont-m)
    (domonad [a (query-something x)
              b (query-something a)
              c (query-something b)]
             (finished-callback c))))
;; (run-cont (cps-bizlogic-6 "query1" prn))
