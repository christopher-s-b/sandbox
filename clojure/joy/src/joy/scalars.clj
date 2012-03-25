

(def population {:zombies 2700, :humans 9})
(println (/ (:zombies population)
            (:humans population))
         "zombies per capita")

(defn pour [lower upper]
  (cond
    (= upper :toujours) (iterate inc lower)
    :else (range lower upper)))

(pour 1 10)
