(ns mmlife.cgol)

(defn empty-field
  [width height]
  {:cells {}
   :mx (dec width)
   :my (dec height)})

(defn wrap
  [{:keys [mx my]} x y]

  (letfn [(wrap' [m n] (if (neg? n) m (if (> n m) 0 n)))]
    [(wrap' mx x)
     (wrap' my y)]))

(defn insert
  [field pos val]
  (assoc-in field [:cells pos] val))

(defn neibs
  [field [x y]]
  (for [dy [-1 0 1]
        dx [-1 0 1]
        :when (not (= dx dy 0))]
    (wrap field (+ x dx) (+ y dy))))

(defn neib-counts
  [field]
  (let [cells (:cells field)]
    (persistent!
     (reduce (fn [m p]
               (reduce (fn [m n]
                         (let [v (get m n 0)]
                           (assoc! m n (inc v))))
                       m
                       (neibs field p)))
             (transient {})
             (keys cells)))))

(defn viz
  [{:keys [cells mx my]}]
  (doseq [y (range (inc my))]
    (println (apply str (for [x (range (inc mx))]
                          (if (cells [x y]) "*" " "))))))
