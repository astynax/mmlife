(ns core
  (:require [dommy.core :as dommy]
            [dommy.utils :as utils]
            [cljs.reader :refer [read-string]])
  (:use-macros [dommy.macros :only [sel sel1 node]]))

(def NBSP \u00A0)

;; манипуляция с DOM'ом
(defn cell-click
  [data]
  (js/alert (str data)))

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

;; работа с гридами
(def field (sel1 :#field))
(def minimap (sel1 :#minimap))

(def field-cells (make-grid! field 16 16 cell cell-click))
(def mmap-cells (make-grid! minimap 8 8 slice cell-click))

(defn fillFieldCell
  [x y col]
  (dommy/set-attr! (field-cells [x y])
                   :style (str "background: " col ";")))

(defn clearField
  []
  (doseq [el (vals field-cells)]
    (dommy/set-attr! el :style "")))

;; websocket
(def ws (js/WebSocket. "ws://localhost:8080/ws"))

(set! (.-onopen ws) (fn [] (.log js/console "opened!")))
(set! (.-onclose ws) (fn [] (.log js/console "closed!")))
(set! (.-onmessage ws)
      (fn [message]
        (do
          (clearField)
          (doseq [[[x y] _] (read-string (.-data message))]
            (fillFieldCell x y "#ff00ff")))))
