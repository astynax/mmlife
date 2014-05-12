(ns mmlife.core
  (:gen-class true)
  (:use org.httpkit.server))

(def PORT 8080)

(defn app [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "hello HTTP!"})

(defn -main
  []
  (println (str "Server started at " 8080))
  (run-server app {:port PORT}))
