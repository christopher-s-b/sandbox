(ns fizzbuzz.monads2
  (:require [clojure.set :only [intersection union] :as set]
            [clojure.algo.monads :as monads]))


(defn pmapcat [f xs] (mapcat f xs) (apply concat (apply pmap f xs)))

(defn weight-user
  "what percent of their likes intersect my likes?"
  [his-likes my-likes]
  (if (empty? my-likes) 0
      (/ (set/intersection his-likes my-likes)
         (count (set/union his-likes my-likes)))))

(defn weight-users-by-likes
  [similar-users-with-likes my-likes]
  (let [users (keys similar-users-with-likes)
        weights (map #(let [user-likes (similar-users-with-likes %)]
                        (weight-user user-likes my-likes))
                     users)]
    (zipmap users weights)))

(defn weight-recs
  "produce list of recs, weighted by number of likes and weight of user who liked"
  [recs-by-users weighted-users]
  (merge-with + (for [[user recs] recs-by-users
                      track recs]
                  (let [weight (weighted-users user)]
                    [track weight]))))

(let [me (get-current-user)
      my-likes (q-likes me)
      similar-users (mapcat q-likers my-likes)
      recs-by-users (zipmap similar-users (map q-likes similar-users))
      weighted-users (weight-users-by-likes recs-by-users my-likes)
      weighted-recs (weight-recs recs-by-users weighted-users)]
  weighted-recs)

(let [me (get-current-user)
      my-likes (q-likes me)
      similar-users (for [like my-likes] (q-likers like))
      recs-by-users (zipmap similar-users (for [user similar-users] (q-likes user)))
      weighted-users (weight-users-by-likes recs-by-users my-likes)
      weighted-recs (weight-recs recs-by-users weighted-users)]
  weighted-recs)
