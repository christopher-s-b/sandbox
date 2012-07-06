(ns fizzbuzz.monads5
  (:use [clojure.algo.monads]))

;; http://brehaut.net/blog/2010/welcome_to_the_machine

;; (defmonad backtrack-state
;;   [m-result (fn [value]
;;               (fn [state]
;;                 [value state]))

;;    m-bind (fn [computation func]
;;             (fn [state]
;;               (when-let [[value new-state] (computation state)]
;;                 ((func value) new-state))))

;;    m-zero (fn [new-state] nil)

;;    m-plus (fn [left right]
;;             (fn [state]
;;               (if-let [result (left state)]
;;                 result
;;                                             (right state))))])

(def backtrack-m (state-t maybe-m))

(with-monad backtrack-m
  (defn run-parser
    "run-parser takes a top level parser and a string or seq'able
         to run the parser on"
    [parser input]
    (parser (lazy-seq input)))

  ;; primatives
  (defn get-one []
    "Gets the next item from the input and returns it"
    (domonad [input (fetch-state)
              _ (set-state (rest input))]
             (first input)))

  (def eof
    (domonad [remaining (fetch-state)
              :when (= (count remaining) 0)]
             nil))

  ;; simple parsers
  ;;
  ;; These parser's are built on top of the primatives above
  (defn one-satisfying [p]
    "This is the most basic matching parser. It tests the next
         item in the sequence against the predicate provided. If
         true, then it is returned, otherwise fails."
    (domonad [one (get-one)
              :when (p one)]
             one))

  (defn one-of [any-of]
    "Matches any one item in the provided collection or string"
    (one-satisfying (set any-of)))

  (defn not-one-of [none-of]
    "Matches any one item not in the provided collection or string"
    (let [none-of (set none-of)]
      (one-satisfying #(not (none-of %)))))

  ;; combinators
  ;;
  ;; These parsers combine other parsers into new parsers
  (defn choice [& parsers]
    "Choice takes one or more parsers (in order) and returns a new
         parser that tries each one in turn until one matches. If
         all fail, then choice fails"
    (reduce m-plus parsers))

  (defn many [p]
    "Many matches the same parser 0 or more times until it it fails,
         then it returns a sequence of the match results"
    (choice (domonad [r p
                      rs (many p)]
                     (cons r rs))
            (m-result '())))

  (defn many1 [p]
    "Many1 is like many, but must match at least 1 item"
    (domonad [r p
              rs (many p)]
             (cons r rs)))

  (defn optional
    "This parser matches 0 or 1 of some parser. Optionally may take
         a default result"
    ([p] (optional p nil))
    ([p default] (choice p (m-result default)))))



(with-monad backtrack-m
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
