(ns ws
  (:require [cljs.reader :refer [read-string]]))

;; возвращает websocket
(defn connect
  [path & {:keys [onopen onclose onmessage]
           ;; умолчательные обработчики для websocket-соединения
           :or {onopen     #(.log js/console (str "wsocket opened: " %))
                onclose    #(.log js/console (str "wsocket closed: " %))
                onmessage (fn [token data]
                            (.log js/console (str "wsocket message: "
                                                  token ", " data)))}}]
  (let [sock (js/WebSocket. (str "ws://" js/window.location.host path))]
    (set! (.-onopen sock)    onopen)
    (set! (.-onclose sock)   onclose)
    (set! (.-onmessage sock) (fn [message]
                               (let [[topic data] (read-string (.-data message))]
                                 (onmessage topic data))))
    sock))

;; функция отправки сообщения через websocket
(defn send!
  [sock topic data]
  (.send sock (pr-str [topic data])))
