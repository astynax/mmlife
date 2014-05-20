(ns core
  (:require [dommy.core :as dommy]
            [dommy.utils :as utils]
            [clojure.set]
            [ws]
            [grid])
  (:use-macros [dommy.macros :only [sel sel1 node]]))

;; constants
(def NBSP \u00A0)

;; состояние
(def pos (atom [0 0]))
(def selected-cells (atom #{}))

;; будущий websocket
(declare sock)

;; манипуляция с DOM'ом
(defn cell [] (node [:td.cell NBSP]))
(defn slice [] (node [:td.slice NBSP]))

;; обработчик кликов в основном по полю
(defn cell-handler
  [pos]
  (swap! selected-cells
         (fn [s] (if (s pos)
                  (disj s pos)
                  (conj s pos)))))

;; ссылки на элементы DOM
(def ^:private send-btn (sel1 :#send-selected))
(dommy/set-attr! send-btn :disabled)

(dommy/listen! send-btn :click
               (fn []
                 (do (ws/send! sock :cells @selected-cells)
                     (reset! selected-cells #{}))))

;; коллекции DOM-элементов, представляющих гриды
(def field-cells
  (grid/build! (sel1 :#field)   16 16 cell-handler))

(def mmap-cells
  (grid/build! (sel1 :#minimap)  8  8 #(ws/send! sock :pos %)))

;; обновление выделения на ячейках
(add-watch selected-cells :toggle
           (fn [_ _ old new]
             (let [to-clear (clojure.set/difference old new)]
               (doseq [c to-clear]
                 (grid/setCellText! field-cells c nil))
               (doseq [c new]
                 (grid/setCellText! field-cells c "*"))
               (if (empty? new)
                 (dommy/set-attr! send-btn :disabled)
                 (dommy/remove-attr! send-btn :disabled)))))

;; реакция на изменение состояния
(add-watch pos :display
           (fn [_ _ old new]
             (grid/fillCell! mmap-cells old nil)
             (grid/fillCell! mmap-cells new "#404040")))

;; websocket-соединение
(def sock
  (ws/connect
   "/ws"

   :onclose
   (fn [] (js/alert "Соединение разорвалось. Обновите страницу!"))

   :onmessage
   (fn [topic data]
     (case topic
       :field
       (grid/drawCells! field-cells data)

       :pos
       (reset! pos data)

       (.log js/console [topic data])))))
