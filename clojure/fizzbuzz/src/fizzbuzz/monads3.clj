(ns fizzbuzz.monads3
  (:require [clojure.set :only [intersection union] :as set])
  (:use [clojure.algo.monads :only [domonad cont-m run-cont]]))


;; error monad
;; customized error monad in for- which just continues if you got anything at all
;; futures - wrap everything in a future
;; async - wrap everything in a continuation that knows about a reactor
;; state - which db to query against?


(defn q-likes [user]
  (continue
   (cond
     (= :user1 user) [:track1 :track2 :track3 :track4]
     (= :user2 user) [:track1 :track2 :track5 :track6]
     :else nil)))

(defn q-likers [track]
  (cond
     (= :track1 track) [:user1 :user2]
     (= :track2 track) [:user1 :user2]
     (= :track3 track) [:user1]
     (= :track4 track) [:user1]
     (= :track5 track) [:user2]
     (= :track6 track) [:user2]
     :else nil))





;; if we want to do async queries with imperative-looking business logic
;; via an eventloop, we need to convert to CPS because we don't have a useful stack.

(defn continue
  "simplest continuation - always invoke the computation.
   could also do nil checks or other error handling and abort."
  [val]
  (fn [kont] (kont val)))


(defn weight-user
  "what percent of their likes intersect my likes?"
  [his-likes my-likes]
  (continue
   (if (empty? my-likes) 0
      (/ (set/intersection his-likes my-likes)
         (count (set/union his-likes my-likes))))))

(defn weight-users-by-likes
  [similar-users-with-likes my-likes]
  (continue
   (let [users (keys similar-users-with-likes)
        weights (map #(let [user-likes (similar-users-with-likes %)]
                        (weight-user user-likes my-likes))
                     users)]
    (zipmap users weights))))

(defn weight-recs
  "produce list of recs, weighted by number of likes and weight of user who liked"
  [recs-by-users weighted-users]
  (continue
   (merge-with + (for [[user recs] recs-by-users
                       track recs]
                   (let [weight (weighted-users user)]
                     [track weight])))))


(defn recommendations [user]
  (domonad cont-m
           [my-likes (q-likes user)
            similar-users (for [like my-likes] (q-likers like))
            recs-by-users (zipmap similar-users (for [user similar-users] (q-likes (run-cont  user))))
            weighted-users (weight-users-by-likes recs-by-users my-likes)
            weighted-recs (weight-recs recs-by-users weighted-users)]
           weighted-recs))

;; (run-cont (recommendations :user1))

(defn recommendations-2 [user]
  (run-cont
   (domonad cont-m
           [my-likes (q-likes user)
            similar-users (continue (for [like (run-cont my-likes)] (q-likers like)))]
           similar-users)))

;; (run-cont (recommendations-2 :user1))
;; (map run-cont (recommendations-2 :user1))
