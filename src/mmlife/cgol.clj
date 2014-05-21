(ns mmlife.cgol
  (:require [mmlife.utils :refer [div]]))

;; конструктор пустого поля
(defn empty-field
  [width height
   ;; кол-во секторов по горизонтали и вертикали
   cols rows]
  (let [row-height (div width rows)
        col-width  (div height cols)]
    {:cells {}
     :mx (dec width)
     :my (dec height)
     :cols cols
     :rows rows
     :col-width col-width
     :row-height row-height}))

(defn- wrap
  [{:keys [mx my]} [x y]]
  (letfn [(wrap' [m n]
            (if (neg? n) m (if (> n m) 0 n)))]
    [(wrap' mx x)
     (wrap' my y)]))

(defn insert
  [field pos val]
  (assoc-in field [:cells pos] val))

;;;;;;;;;;;; Статаистика по ячейкам ;;;;;;;;;;;;;

(defn neibs
  [field [x y]]
  (for [dy [-1 0 1]
        dx [-1 0 1]
        :when (not (= dx dy 0))]
    (wrap field [(+ x dx) (+ y dy)])))

;; статистика по соседям (с.) ячейки
(def ^:private empty-stats
  {:neibs {}  ;; отображение значения с. в кол-во с. с этим значением
   :total 0}) ;; общее кол-во с. у ячейки

(defn- update-stats
  [stats val]
  (-> stats
      (update-in [:total] inc)
      (update-in [:neibs val] #(inc (or % 0)))))

;; обновление transient map со значением по умолчанию
(defn- update-default!
  "update for transient"
  [tr key func default]
  (let [val (get tr key default)]
    (assoc! tr key (func val))))

;; сборщик статистики по полю
;; возвращает отображение позиции ячейки в запись статистики
;; (отображаются позиции всех ячеек с ненулевым кол-вом соседей)
(defn- collect-stats
  [field]
  (letfn [(process-cell! [res [pos val]]
            (reduce (fn [acc neib]
                      (update-default! acc neib
                                       #(update-stats % val)
                                       empty-stats))
                    res
                    (neibs field pos)))]
    (persistent!
     (reduce process-cell!
             (transient {})
             (:cells field)))))

;;;;;;;;;;;;;; Обработка поля ;;;;;;;;;;;;;

(defn- rule
  [stats val]
  (letfn [(choice [xs]
            (nth xs (rand-int (count xs))))]
    (let [{:keys [neibs total]} stats]
      (cond
       ;; три родителя у пустой ячейки
       (and (= total 3) (nil? val))
       (case (count neibs)
         ;; все три родителя одного вида - вид сохраняется
         1 (-> neibs first first)
         ;; два вида - выбираем тот, которого больше
         2 (let [[[v1 c1] [v2 c2]] (seq neibs)]
             (if (> c1 c2) v1 v2))
         ;; все три родителя разного вида - выбираем случайного
         (-> neibs vals choice))

       ;; ячейки с парой-тройкой соседей - выживают
       (and (#{2 3} total) val) val
       true nil))))

(defn- cells-seq
  [{:keys [mx my]}]
  (for [x (range (inc mx))
        y (range (inc my))]
    [x y]))

;; производит один цикл над полем
(defn populate
  [field]
  (let [stats (collect-stats field)]
    (update-in field [:cells]
               (fn [cells]
                 (persistent!
                  (reduce (fn [t p]
                            (let [old (get t p)]
                              (if-let [val (rule (stats p empty-stats) old)]
                                (assoc! t p val)
                                (if old
                                  (dissoc! t p)
                                  t))))
                          (transient cells)
                          (cells-seq field)))))))

;; делит поле на ячейки сетки cols x rows
(defn split-field
  [{:keys [col-width row-height cells]}]
  (letfn [(put [m [[x y] v]]
            (let [xx (div x col-width)
                  yy (div y row-height)
                  x (mod x col-width)
                  y (mod y row-height)]
              (update-in m [[xx yy]] conj [[x y] v])))]
    (reduce put {} (seq cells))))
