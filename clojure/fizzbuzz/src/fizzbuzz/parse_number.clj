(ns fizzbuzz.parse-number
  (:use [clojure.algo.monads]
        [fizzbuzz.parser-m]))

(with-monad parser-m
  (def parse-digit (one-of "0123456789"))
  (def parse-digits (many1 parse-digit))
  (def parse-sign (one-of "+-"))
  (def parse-dot (one-of "."))

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


(map #(run-parser parse-number %)
     ["123"
      ""
      "123.123"
      "0"
      "0.0"
      "-1"
      "1 2"])

;; => ([[:int "+123"] ()] nil [[:float "+123.123"] ()] [[:int "+0"] ()] [[:float "+0.0"] ()] [[:int "-1"] ()] nil)
