(ns duey-monads-unsession.core)


(def f (comp (partial * 2) inc))
(f 9)


(defn f [x] [(* 2 x) (* 5 x)])
(f 4)


(defn g [x] [(+ 2 x) (+ 5 x)])
(g 4)

(def h (comp f g))
(h 9) ;; clojure.lang.PersistentVector cannot be cast to java.lang.Number

;; need plumbing
(defn bind1 [mv f]
  (mapcat f mv))

(defn bind [mv f]
  (apply concat (map f mv)))

(bind (vector 4) f)
(f 4)

(= (f 4) (bind (vector 4) f)) ;; first monad law

(= (vector 4) (bind (vector 4) vector)) ;; second
;; bind undoes what unit does

(= (bind (bind (vector 4) f) g)
   (bind (vector 4)
         (fn [x]
           (bind (f x) g)))) ;; third

(hash-set 9)
(defn set-bind [mv f]
  (apply clojure.set/union
         (map f mv)))

((constantly 3))
((constantly 3) 1 2 3 4 5)

;; mv is fn of no args
(defn bind [mv f]
  (fn []
    (let [v (mv)
          new-mv (f v)]
      (new-mv))))

(defn f [x] (constantly (inc x)))
((f 4))

(bind (constantly 2) f)
((bind (constantly 2) f))


(defn result [x]
  (fn [state]
    [x state]))

((result 3) :state)

(def h (result 2))

(h [])
(h #{})
(h nil)

(defn bind [mv f]
  (fn [state]
    (let [[v new-state] (mv state)
          new-mv (f v)]
      (new-mv new-state))))

(defn f [x]
  (fn [state]
    [(inc x) (conj state :inced)]))

(def h (f 8))
(h [])

(def h (bind (result 3) f))
(h [])
(h nil)

;; back to vector monad
(defn bind [mv f] (mapcat f mv))
(def v1 [:a :b :c])
(def v2 [1 2 3])

(bind v1 (fn [x]
           (bind v2 (fn [y]
                      (vector [x y])))))

(for [x v1 y v2] [x y])

(defmacro m-do [result
                [sym1 mv1
                 sym2 mv2]
                expression]
  `(bind ~mv1 (fn [~sym1]
                (bind ~mv2 (fn [~sym2]
                             (~result ~expression))))))

(m-do vector [x v1 y v2] [x y])

;; m-do is a generalization of for

;; universal query language. linq (eric meyer)
;; paper: comprehending queries -proves that query languages are set comprehension
;; query language: = go get a list of results, filter them out
;; for loop is a join, also selects and project (p=roject is select in sql)

;; back to state
(defn bind [mv f]
  (fn [state]
    (let [[v new-state] (mv state)
          new-mv (f v)]
      (new-mv new-state))))

(defn result [x] (fn [state] [x state]))

(defn poke [k v]
  (fn [state]
    [nil (assoc state k v)]))

(def h (poke :a 4))
(h {})

(defn peek [k]
  (fn [state]
    [(get state k) state]))

(def h (peek :a))
(h {:a 6})

(def h (m-do result [x (peek :a)
              y (peek :b)]
      [x y]))

(h {})
(h {:a 4})

(def h (m-do result
             [_ (poke :a 3)
              x (peek :a)]
             (inc x)))

(h {})
