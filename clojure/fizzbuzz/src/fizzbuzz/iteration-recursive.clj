(ns sicp.ch2)

(defn length [items]
  (if (empty? items)
    0
    (inc (length (rest items))))) ; not tail recursive
(assert (= 3 (length [1 2 3])))

;; how can we make this tail recursive? use the iter pattern

(defn length-2 [items]
  (defn iter [count items]
    (if (empty? items)
      count
      (recur (inc count) (rest items)))) ; tail recursive
  (iter 0 items))
(assert (= 3 (length-2 [1 2 3])))

;; clojure provides a special form to make iteration more idiomatic

(defn length-3 [items]
  (loop [count 0 items items]
    (if (empty? items)
      count
      (recur (inc count) (rest items))))) ; tail recursive
(assert (= 3 (length-3 [1 2 3])))

;; so, loop is a construct for helping turn algorithms which aren't naturally
;; tail recursive into iterative items with tail-recursion


(defn square [x] (* x x))

(defn square-list [items]
  (if (empty? items)
    []
    (cons (square (first items))
          (square-list (rest items))))) ; note, not a tail call
(assert (= [1 4 9 16] (square-list [1 2 3 4])))

(defn map-2 [proc, items]
  (if (empty? items)
    nil
    (cons (proc (first items))
          (map-2 proc (rest items))))) ;; not a tail-call
(assert (= [1 4 9 16] (map-2 square [1 2 3 4])))

(defn map-3 [proc, items] ; using tail call
  (reverse (reduce (fn [x y] (cons (proc y) x)) [] items)))
(assert (= [1 4 9 16] (map-3 square [1 2 3 4])))


(defn square-list-2 [items] (map square items))
(assert (= [1 4 9 16] (square-list-2 [1 2 3 4])))

(defn square-list-3 [items]
  (defn iter [things answer] ; would be more idiomatic to use `loop`
    (if (empty? things)
      answer
      (recur (rest things) ; tail call
            (cons
             (square (first things)) ; reversed because cons is prepend ?
             answer))))
  (iter items []))
(assert (= [16 9 4 1] (square-list-3 [1 2 3 4]))) ;; reversed

(defn square-list-4 [items]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (recur (rest things) ; tail call
            (cons ; arg order swapped - still reversed !!
             answer
             (square (first things))))))
  (iter items []))
(assert (= [16 9 4 1] (square-list-3 [1 2 3 4]))) ;; still reversed

;; i think the moral here is that tail recursion means the list
;; will always be reversed, due to the nature of stack algorithms.
;; this is still a bit murky for me.
;; this implies that if we implement `map` in tail recursive
;; fashion, we'll need to reverse the result.

(defn for-each [proc, items]
  (if (empty? items) nil
      (do
        (proc (first items))
        (recur proc (rest items)))))
;; (for-each prn [1 2 3 4])

;; note it is really hard to test side effects! we'd want to pass a
;; stateful environment in so we can inspect the environment after

;; can we do it without the `do` special form?
;; this version is broken.
(defn for-each-2 [proc, items]
  (let [f (first items)
        r (rest items)]
    (if (empty? r)
      (proc f)
      (recur proc r))))

