(ns fizzbuzz.regexp)

;; MatchObject api - http://docs.python.org/library/re.html#re.MatchObject

;; c -- any literal character
;; . -- any single character
;; ^$ -- beginning, end of line
;; * - 0 or more occurences of preceeding
;; + - one or more occurences of preceeding


(defn match-star [op text] nil)

(defn match-here [regex text] nil)

(defn match
  "match a regexp, returning ?? to indicate match"
  [regexp, text]
  (if (= "^" (take 1 text))
    (match-here (rest regexp) text)
    (loop [regexp regexp, text text]
      (if (match-here regexp text) true)

      )))
