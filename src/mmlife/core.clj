(ns mmlife.core
  (:gen-class true)
  (:require
   [clojure.java.io :as io]
   [org.httpkit.server :as server]
   [compojure.handler :as ch]
   [compojure.route :as cr]
   [compojure.core :refer [defroutes GET POST DELETE ANY context]]
   [carica.core :refer [config]]
   [mmlife.cgol :as cgol]))

;; реестр каналов пользователей
(defonce chans (atom {}))

;; хранилище состояния
(defonce state (atom [;; glider на пустом поле
                      (reduce #(cgol/insert %1 %2 1)
                              (cgol/empty-field 128 128)
                              [[1 0] [2 1] [0 2] [1 2] [2 2]])
                      ;; слайсов на первом ходу нет
                      {}]))

;; делитель поля на области
(def slice (partial cgol/split-field 8 8))

;; функция обновления состояния
(defn update-state!
  [state]
  (swap! state (fn [[field _]]
                 (let [f (cgol/populate field)
                       s (slice f)]
                   [f s]))))

;; цикл обновления состояния и отправки в соединения
(defn updating-loop
  [tout stop-flag chans state]
  (loop []
    (let [[_ slices] (update-state! state)]
      (doseq [[ch k] @chans]
        (server/send! ch (pr-str (slices (:pos k) '())))))
    (Thread/sleep tout)
    (when-not (realized? stop-flag)
      (recur))))

;; обработчик websocket-соединения
(defn ws-handler
  [request]
  (server/with-channel request chan
    (when (server/websocket? chan)
      (swap! chans assoc chan {:pos [0 0]})
      (server/on-close chan
                       (fn [_] (swap! chans dissoc chan)))
      (server/on-receive chan
                         (fn [data]
                           (println data))))))

(defroutes app-routes
  (GET "/" [] (slurp (io/resource "public/html/index.html")))
  (GET "/ws" [] ws-handler)
  (cr/resources "/static/")
  (cr/not-found "Not found!"))

(defn -main
  []
  (let [port (or (config :port) 8080)]
    (println (str "Server started at " port))
    (future (updating-loop 1000 (promise) chans state))
    (server/run-server
     (ch/site #'app-routes)
     {:port port})))
