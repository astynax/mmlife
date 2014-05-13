(ns mmlife.cgol)

(defn empty-field
  [width height]
  {:cells {}
   :mx (dec width)
   :my (dec height)})

(defn wrap
  [{:keys [mx my]} [x y]]
  (letfn [(wrap' [m n]
            (if (neg? n) m (if (> n m) 0 n)))]
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
    (wrap field [(+ x dx) (+ y dy)])))

(defn update!
  "update for transient"
  ([tr key func]
     (update! tr key func nil))
  ([tr key func default]
     (let [val (get tr key default)]
       (assoc! tr key (func val)))))

(defn neib-counts
  [field]
  (let [cells (:cells field)]
    (persistent!
     (reduce (fn [t p]
               (reduce #(update! %1 %2 inc 0)
                       t
                       (neibs field p)))
             (transient {})
             (keys cells)))))

(defn rule
  [n s]
  (case [n s]
    [3 nil] 1
    ([2 1] [3 1]) 1
    nil))

(defn cells-seq
  [{:keys [mx my]}]
  (for [x (range (inc mx))
        y (range (inc my))]
    [x y]))

(defn populate
  [field]
  (let [counts (neib-counts field)]
    (update-in field [:cells]
               (fn [cells]
                 (persistent!
                  (reduce (fn [t p]
                            (let [old (get t p)]
                              (if-let [val (rule (counts p 0) old)]
                                (assoc! t p val)
                                (if old
                                  (dissoc! t p)
                                  t))))
                          (transient cells)
                          (cells-seq field)))))))

(defn viz
  [{:keys [cells mx my]}]
  (doseq [y (range (inc my))]
    (println (apply str (for [x (range (inc mx))]
                          (if (cells [x y]) "*" " "))))))
