(ns fizzbuzz.parser-m
  "reconstructed from http://brehaut.net/blog/2010/ghosts_in_the_machine (Andrew Brehaut)"
  (:use [clojure.algo.monads]))


(def parser-m (state-t maybe-m))


(with-monad parser-m
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
