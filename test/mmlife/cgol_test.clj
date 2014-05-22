(ns mmlife.cgol-test
  (:require [clojure.test :refer :all]
            [mmlife.cgol :refer :all]))

(def f10x10 (empty-field 10 10))

(deftest insert-test
  (testing "single insert"
    (is (= (:cells (insert f10x10 [4 4] 1)) {[4 4] 1}))
    (is (= (:cells (insert f10x10 [0 0] 1)) {[0 0] 1})))

  (testing "multiple insert"
    (is (= (-> f10x10
               (insert [2 3] 10)
               (insert [3 2] 5)
               :cells)
           {[2 3] 10
            [3 2] 5})))

  (testing "comutativity"
    (is (= (-> f10x10
               (insert [2 3] 10)
               (insert [3 2] 5)
               :cells)
           (-> f10x10
               (insert [3 2] 5)
               (insert [2 3] 10)
               :cells)))))

(deftest neibs-test
  (testing "cell in center of field"
    (is (= (set (neibs f10x10 [3 3]))
           #{[2 2] [3 2] [4 2]
             [2 3]       [4 3]
             [2 4] [3 4] [4 4]})))

  (testing "cell on the edge"
    (is (= (set (neibs f10x10 [0 0]))
           #{[9 9] [0 9] [1 9]
             [9 0]       [1 0]
             [9 1] [0 1] [1 1]}))))

(deftest populate-test
  (testing "populatig of empty filed"
    (is (= f10x10
           (populate f10x10)
           (-> f10x10
               populate
               populate))))

  ;; блок, даже гетерогенный - стабилен
  (let [block-in-3x3 (-> (empty-field 3 3)
                         (insert [0 0] 1)
                         (insert [1 0] 2)
                         (insert [0 1] 3)
                         (insert [1 1] 4))]
    (testing "block on 3x3 field"
      (is (= block-in-3x3
             (populate block-in-3x3)
             (-> block-in-3x3
                 populate
                 populate)))))

  (let [blinker-in-4x4 (-> (empty-field 4 4)
                           (insert [1 0] 1)
                           (insert [1 1] 1)
                           (insert [1 2] 1))]
    (testing "blinker"
      ;; мигалка менняет состояние от хода к ходу,
      ;; возвращаясь в исходное состояние каждый второй ход
      (is (not= blinker-in-4x4
                (populate blinker-in-4x4)))
      (is (= blinker-in-4x4
             (-> blinker-in-4x4
                 populate
                 populate)
             (-> blinker-in-4x4
                 populate
                 populate
                 populate
                 populate)))))

  (let [two-color-blinker (-> (empty-field 4 4)
                              (insert [1 0] 1)
                              (insert [1 1] 2)
                              (insert [1 2] 2))
        symm-blinker (-> (empty-field 4 4)
                         (insert [1 0] 1)
                         (insert [1 1] 2)
                         (insert [1 2] 1))]
    (testing "two-color blinker"
      ;; несимметричная мигалка вида
      ;; .....
      ;; .122.
      ;; .....
      ;; после двух циклов вырождается в гомогенную
      ;; .....
      ;; .222.
      ;; .....
      (is (not= two-color-blinker (-> two-color-blinker
                                      populate
                                      populate)))
      ;; несимметричная мигалка стабилизируется через один свой цикл (2 хода)
      (is (= (-> two-color-blinker
                 populate)
             (-> two-color-blinker
                 populate
                 populate
                 populate))))
    ;; симметричная мигалка стабильна
    (testing "symmetrical blinker"
      (is (= symm-blinker
             (-> symm-blinker
                 populate
                 populate))))))
