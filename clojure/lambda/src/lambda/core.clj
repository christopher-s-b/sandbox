(ns lambda.core)


(def env { 'a 1
           'b 2
           'add (fn [x y] (+ x y))
           })

;; reduce a list of terms into the value
;; take the first term
;; if it is 


(defn meval [expr]
  (cond
   (= (first expr) 'app) ((env (nth expr 1)) (env (nth expr 2)) (env (nth expr 3)))
   ))


;; data Term
;;   = Var String
;;   | App Term Term
;;   | Lam String Term


;; App (Lam x b) a
;;(def expr ['app '('lam 'x 'b) 'a])
(def expr ['app 'add 'a 'b])





;; App (Lam x (Lam y (Var x))) (Var y)
;; 'app ('lam "x" ('lam "y" ('var "x"))) ('var "y")
