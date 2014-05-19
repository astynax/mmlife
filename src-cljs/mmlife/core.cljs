(ns core
  (:require [dommy.core :as dommy]
            [dommy.utils :as utils]
            [cljs.reader :refer [read-string]]
            [clojure.set])
  (:use-macros [dommy.macros :only [sel sel1 node]]))

;; constants
(def NBSP \u00A0)

;; состояние
(def pos (atom [0 0]))
(def selected-cells (atom #{}))

;; будущий websocket
(declare ws)

;; функция отправки сообщения через websocket
(defn ws-send
  [data]
  (.send ws (pr-str data)))

;; манипуляция с DOM'ом
(defn cell [] (node [:td.cell NBSP]))
(defn slice [] (node [:td.slice NBSP]))

(defn make-grid!
  [dom rows cols el-fabric handler]
  (let [array (for [y (range rows)]
                (let [row (for [x (range cols)]
                            (let [c (el-fabric)]
                              (dommy/listen! c :click
                                             #(handler [x y] %))
                              [[x y] c]))
                      row-el (->> row
                                  (map second)
                                  (cons :tr.cell)
                                  vec
                                  node)]
                  (dommy/append! dom row-el)
                  row))]
    (reduce (partial reduce (fn [m [k v]] (assoc m k v)))
            {} array)))

;; обработчик кликов в основном по полю
(defn cell-handler
  [pos]
  (swap! selected-cells
         (fn [s] (if (s pos)
                  (disj s pos)
                  (conj s pos)))))

;; ссылки на элементы DOM
(def ^:private field (sel1 :#field))
(def ^:private minimap (sel1 :#minimap))
(def ^:private send-btn (sel1 :#send-selected))
(dommy/set-attr! send-btn :disabled)

(dommy/listen! send-btn :click
               (fn []
                 (do (ws-send [:cells @selected-cells])
                     (reset! selected-cells #{}))))

;; коллекции DOM-элементов, представляющих гриды
(def field-cells (make-grid! field 16 16 cell cell-handler))
(def mmap-cells (make-grid! minimap 8 8 slice #(ws-send [:pos %])))

(defn fillCell
  [grid pos color]
  (dommy/set-attr! (grid pos)
                   :style (if (nil? color) ""
                              (str "background: " color ";"))))

(defn clearGrid
  [grid]
  (doseq [el (vals grid)]
    (dommy/set-attr! el :style "")))

;; выставляет текст в ячейке грида (text=nil очищает ячейку)
(defn setCellText
  [grid pos text]
  (dommy/set-text! (grid pos) (or text NBSP)))

;; обновление выделения на ячейках
(add-watch selected-cells :toggle
           (fn [_ _ old new]
             (let [to-clear (clojure.set/difference old new)]
               (doseq [c to-clear]
                 (setCellText field-cells c nil))
               (doseq [c new]
                 (setCellText field-cells c "*"))
               (if (empty? new)
                 (dommy/set-attr! send-btn :disabled)
                 (dommy/remove-attr! send-btn :disabled)))))

;; реакция на изменение состояния
(add-watch pos :display
           (fn [_ _ old new]
             (fillCell mmap-cells old nil)
             (fillCell mmap-cells new "#404040")))

;; websocket
(def ws (js/WebSocket. "ws://localhost:8080/ws"))

(set! (.-onopen ws) (fn [] (.log js/console "opened!")))
(set! (.-onclose ws) (fn [] (.log js/console "closed!")))
(set! (.-onmessage ws)
      (fn [message]
        (let [[topic data] (read-string (.-data message))]
          (case topic
            :field
            (do (clearGrid field-cells)
                (doseq [[pos color] data]
                  (fillCell field-cells pos color)))

            :pos
            (reset! pos data)

            (.log js/console [topic data])))))
