

(for [x (range 2) y (range 2)] [x y (bit-xor x y)])

(defn xors [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y (rem (bit-xor x y) 256)]))


(def gfx (.getGraphics frame))
(.fillRect gfx 100 100 50 75)
(.setColor gfx (java.awt.Color. 255 128 0))
(.fillRect gfx 100 150 75 50)

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(doseq [[x y xor] (xors 500 500)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(defn clear [g] (.clearRect g 0 0 200 200))

(defn f-values [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y v v)))

(draw-values bit-and 256 256)
(draw-values + 256 256)
(draw-values * 256 256)
