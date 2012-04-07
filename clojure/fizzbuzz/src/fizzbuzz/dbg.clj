
;; < http://stackoverflow.com/questions/2352020/debugging-in-clojure/2352280#2352280 >

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; Examples of dbg
(println (+ (* 2 3) (dbg (* 8 9))))
(println (dbg (println "yo")))
(defn factorial[n] (if (= n 0) 1 (* n (dbg (factorial (dec n))))))
(factorial 8)

(def integers (iterate inc 0))
(def squares  (map #(dbg(* % %))   integers))
(def cubes    (map #(dbg(* %1 %2)) integers squares))
(take 5 cubes)
(take 5 cubes)
