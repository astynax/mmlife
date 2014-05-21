(ns mmlife.utils)

(def ^:private MAX-24BIT (int (Math/pow 2 24)))

;; возвращает случайный цвет в формате #RRGGBB
(defn rand-color [] (->> (rand-int MAX-24BIT)
                        Integer/toHexString
                        (str "#")))

;; делит нацело a на b
(defn div [a b] (int (Math/floor (/ (float a) b))))
