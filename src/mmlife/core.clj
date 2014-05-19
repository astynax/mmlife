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
(defonce state (atom {;; glider на пустом поле
                      :field (reduce #(cgol/insert %1 %2 "#002000")
                                     (cgol/empty-field 128 128)
                                     [[1 0] [2 1] [0 2] [1 2] [2 2]])
                      ;; слайсов на первом ходу нет
                      :slices {}
                      ;; обновлений тоже пока нет
                      :updates []}))

;; делитель поля на области
(def slice (partial cgol/split-field 8 8))

;; функция обновления состояния
(defn update-state!
  [state]
  (swap! state (fn [{:keys [field tasks]}]
                 (let [field' (if tasks
                                (update-in field [:cells]
                                           #(reduce conj % tasks))
                                field)
                       f (cgol/populate field')
                       s (slice f)]
                   {:field f
                    :slices s
                    :tasks []}))))

;; цикл обновления состояния и отправки в соединения
(defn updating-loop
  [tout stop-flag chans state]
  (loop []
    (let [{:keys [slices]} (update-state! state)]
      (doseq [[ch k] @chans]
        (server/send! ch (pr-str [:field
                                  (slices (:pos k) '())]))))
    (Thread/sleep tout)
    (when-not (realized? stop-flag)
      (recur))))

;; обработчик сообщений от клиента
(defn process-message
  [chan [topic data]]
  (case topic
    :cell
    (let [[dx dy] (get-in @chans [chan :pos])
          [x y] data
          x (+ x (* 16 dx))
          y (+ y (* 16 dy))]
      (swap! state update-in [:tasks] conj [[x y] "#408000"]))

    :pos
    (do (server/send! chan (pr-str [:pos data])) ;; подтверждение
        (swap! chans
               (fn [m] (update-in m [chan]
                                 assoc :pos data))))))

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
                           (process-message chan (read-string data)))))))

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
