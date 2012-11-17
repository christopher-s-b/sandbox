(ns compojure-bootstrap.core
  (:use compojure.core, ring.adapter.jetty)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes main-routes
  (GET "/" {params :params} (str "<h1>Hello World</h1>" (pr-str params)))
  (route/not-found "<h1>Page not found</h1>"))

(defn -main [& m]
  (run-jetty (handler/site main-routes) {:port 8080}))
