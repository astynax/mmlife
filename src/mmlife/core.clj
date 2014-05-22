(ns mmlife.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]

   [org.httpkit.server :as server]

   [compojure.handler :as ch]
   [compojure.route :as cr]
   [compojure.core :refer [defroutes GET POST DELETE ANY]]

   [carica.core :refer [config]]

   [ring.middleware.session.cookie :refer [cookie-store]]
   [ring.util.response :refer [response redirect-after-post]]

   [mmlife.cgol :as cgol]
   [mmlife.utils :as utils]))

;; реестр каналов пользователей
(defonce chans (atom {}))

;; хранилище состояния
(defonce state (atom {;; пустое поле
                      :field (cgol/empty-field 128 128 ;; 128x128 ячеек
                                               8 8)    ;; 8x8 секторов
                      ;; слайсов на первом ходу нет
                      :slices {}
                      ;; обновлений тоже пока нет
                      :updates []}))

;; функция обновления состояния
(defn update-state!
  [state]
  (swap! state (fn [{:keys [field tasks]}]
                 (let [f (cgol/populate field)
                       f' (if tasks
                                (update-in f [:cells]
                                           #(reduce conj % tasks))
                                f)
                       s (cgol/split-field f')]
                   {:field f'
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
    :cells
    (let [{:keys [pos color]} (get @chans chan)
          [dx dy] pos
          dx (* 16 dx)
          dy (* 16 dy)
          cells (for [[x y] data]
                  [[(+ x dx) (+ y dy)] color])]
      (swap! state update-in [:tasks] into cells))

    :pos
    (do (server/send! chan (pr-str [:pos data])) ;; подтверждение
        (swap! chans
               (fn [m] (update-in m [chan]
                                 assoc :pos data))))))

;; обработчик websocket-соединения
(defn ws-handler
  [request]
  (server/with-channel request chan
    (let [{:keys [nickname color]} (:session request)]
      (when (server/websocket? chan)
        (swap! chans assoc chan {:pos [0 0]
                                 :nickname nickname
                                 :color color})
        (server/on-close chan
                         (fn [_] (swap! chans dissoc chan)))
        (server/on-receive chan
                           (fn [data]
                             (process-message chan (read-string data))))
        (server/send! chan (pr-str [:pos [0 0]]))))))

;; response с сообщением об ошибке
(defn oops
  [text]
  (str
   "<html><head><meta charset=\"utf-8\"/>"
   "<title>MMLife: Oops!</title><head><body><center>"
   "<h3>Ой!</h3>"
   text
   "</center></body></html>"))

;; роутинг приложения
(defroutes app-routes
  (GET "/" {session :session}
       (if-not (:nickname session)
         ;; пользователь не представился - попросим об этом
         (slurp (io/resource "public/html/nameyourself.html"))
         ;; пользователь представлен, выводим основной интерфейс
         (slurp (io/resource "public/html/index.html"))))

  ;; обработка данных о пользователе
  (POST "/newuser" {{nickname "nickname"
                     color    "color"} :form-params}
        (let [nickname (clojure.string/trim nickname)
              color (clojure.string/trim color)]
          (cond (empty? nickname)
                (oops "Ник не может быть пустым!")

                (not (or (empty? color)
                         (re-matches #"#[0-9a-fA-F]{6}" color)))
                (oops "Цвет может быть либо не указан,
                       либо должен иметь формат #RRGGBB!")

                :else
                (assoc (redirect-after-post "/")
                  :session {:nickname nickname
                            :color (if (empty? color)
                                     (utils/rand-color)
                                     color)}))))

  (GET "/ws" [] ws-handler)
  (cr/resources "/static/")
  (cr/not-found "Not found!"))

;; ключ для шифрования сессии
(def SESSION-KEY "--==[MMLife]==--")

(defn -main
  []
  (let [port (or (config :port) 8080)]
    (println (str "Server started at " port))
    (future (updating-loop 500 (promise) chans state))
    (server/run-server
     (ch/site #'app-routes
              {:session {:store (cookie-store {:key SESSION-KEY})}})
     {:port port})))
