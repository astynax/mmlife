(ns grid
  (:require [dommy.core :as dommy]
            [dommy.utils :as utils])
  (:use-macros [dommy.macros :only [sel sel1 node]]))

;; наполняет dom-элемент (тэг tbody) ячейками в кол-ве rows X cols,
;; на каждую ячейку навешивается обработчик onclick
(defn build!
  [tbody rows cols onclick]
  (let [array (for [y (range rows)]
                (let [row (for [x (range cols)]
                            (let [c (node [:td.cell \u00A0])]
                              ;; конфигурируем класс
                              ;;(dommy/add-class! c td-class)
                              ;; и вешаем обработчик клика
                              (dommy/listen! c :click
                                             #(onclick [x y] %))
                              [[x y] c]))
                      row-el (->> row
                                  (map second)
                                  (cons :tr)
                                  vec
                                  node)]
                  (dommy/append! tbody row-el)
                  row))]
    ;; сборка словаря dom-элементов
    (reduce (partial reduce (fn [m [k v]] (assoc m k v)))
            {} array)))

;; устанавливает цвет фона ячейке грида
(defn fillCell!
  [grd pos color]
  (dommy/set-attr! (grd pos)
                   :style (if (nil? color) ""
                              (str "background: " color ";"))))

;; очищает грид (убирает стиль фона у всех ячеек)
(defn clearGrid!
  [grd]
  (doseq [el (vals grd)]
    (dommy/set-attr! el :style "")))

;; выставляет текст в ячейке грида (text=nil очищает ячейку)
(defn setCellText!
  [grd pos text]
  (dommy/set-text! (grd pos) (or text \u00A0)))

;; "отрисовывает" коллекцию ячеек в грид
(defn drawCells!
  [grd cells]
  (do (clearGrid! grd)
      (doseq [[pos color] cells]
        (grid/fillCell! grd pos color))))
