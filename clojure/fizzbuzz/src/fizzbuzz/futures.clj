;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(defn get-user-favorites [user]
  (Thread/sleep 10000) (prn 1)
  1)

(defn get-track-favoriters [user track]
  (Thread/sleep 1000) (prn 2)
  2)

(defn get-favoriters-favorites [favoriters]
  (let [results []]
    (dotimes [n 10]
      (conj results (future (get-user-favorites nil))))
    (map deref results))
  (prn 3)
  3)

(defn sort-users-by-similarity [user favoriters]
  (Thread/sleep 1000) (prn 4)
  4)


(defn recommendations [user track]
  (let [user-favorites (future (get-user-favorites user))
        track-favoriters (future (get-track-favoriters user track))
        favoriters-favorites (future (get-favoriters-favorites @track-favoriters))
        sorted-favoriters (future (sort-users-by-similarity user @track-favoriters))
        top-favoriter (future @sorted-favoriters)]
    (future (get-user-favorites @top-favoriter))))
