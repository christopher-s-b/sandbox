(ns lambda.core)

(def env { 'a 1
           'b 2
           '+ +
           })


(defn meval
  [term] (cond
          (contains? env term) (env term)
          :else term)

  [term f & args]
    (cond
     (= term 'app) (apply (meval f) (map meval args)))
    
)

(comment
  (def expr ['app '+ 'a 'b])  
  (apply meval expr)

  (meval ['a])
  (meval ['app '+ 'a 1])

'app '+ 'a '1
'app '+ 'a '('+ 'a 'a)


  )
