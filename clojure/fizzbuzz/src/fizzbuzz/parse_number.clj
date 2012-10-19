(ns fizzbuzz.parse-number
  "reconstructed from http://brehaut.net/blog/2010/ghosts_in_the_machine (Andrew Brehaut)"
  (:use [clojure.algo.monads]
        [fizzbuzz.parser-m]))


;; private API - parse-number internals
(def ^:private parse-digit (one-of "0123456789"))
(def ^:private parse-digits (many1 parse-digit))
(def ^:private parse-sign (one-of "+-"))
(def ^:private parse-dot (one-of "."))

(comment
  (parse-digit "1")          ; [\1 ())]
  (parse-digit "123")        ; [\1 (\2 \3)]
  (parse-digit "abc123")     ; nil

  (parse-digits "123 456")   ; [(\1 \2 \3) (\space \4 \5 \6)]
  (parse-digits " 123 456")  ; nil
  (parse-digits "123bar")    ; [(\1 \2 \3) (\b \a \r)]
  (parse-digits "foo123bar") ; nil

  (parse-sign "+")           ; [\+ ()]
  (parse-sign "-")           ; [\- ()]
  (parse-sign "+123.45")     ; [\+ (\1 \2 \3 \. \4 \5)]
  (parse-sign "+ +")         ; [\+ (\space \+)]

  (parse-dot ".3")           ; [\. (\3)]
  (parse-dot "3.3")          ; nil
  )


;; public API
(with-monad parser-m
  (def parse-integer
    (domonad [digits parse-digits]
             [:int digits]))

  (def parse-float
    (domonad [whole-digits parse-digits
              _ parse-dot
              decimal-digits parse-digits]
             [:float (concat whole-digits "." decimal-digits)]))

  (def parse-number
    (domonad [sign (optional parse-sign "+")
              [type digits] (choice (domonad [int parse-integer
                                              _ eof]
                                             int)
                                    (domonad [float parse-float
                                              _ eof]
                                             float))]
                          [type (apply str (cons sign digits))])))


(comment
  (parse-number "123")       ; [[:int "+123"] ()]
  (parse-number "")          ; nil
  (parse-number "123.123")   ; [[:float "+123.123"] ()]
  (parse-number "0")         ; [[:int "+0"] ()]
  (parse-number "0.0")       ; [[:float "+0.0"] ()]
  (parse-number "-1")        ; [[:int "-1"] ()]
  (parse-number "1 2")       ; nil
  )
