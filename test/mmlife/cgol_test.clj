(ns mmlife.cgol-test
  (:require [clojure.test :refer :all]
            [mmlife.cgol :refer :all]))

(def f10x10 (empty-field 10 10))

(deftest wrap-test
  (testing "normal cells"
    (is (= (wrap f10x10 [0 0]) [0 0]))
    (is (= (wrap f10x10 [9 9]) [9 9])))

  (testing "out of bounds"
    (is (= (wrap f10x10 [-1  0]) [9 0]))
    (is (= (wrap f10x10 [ 0 -1]) [0 9]))
    (is (= (wrap f10x10 [-1 -1]) [9 9]))
    (is (= (wrap f10x10 [10  0]) [0 0]))
    (is (= (wrap f10x10 [ 0 10]) [0 0]))
    (is (= (wrap f10x10 [-1 10]) [9 0]))))

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

(deftest neib-counts-test
  (testing "two near cells"
    (is (= (-> f10x10
               (insert [0 0] 1)
               (insert [1 0] 1)
               neib-counts
               set)
           #{[[9 9] 1] [[0 9] 2] [[1 9] 2] [[2 9] 1]
             [[9 0] 1] [[0 0] 1] [[1 0] 1] [[2 0] 1]
             [[9 1] 1] [[0 1] 2] [[1 1] 2] [[2 1] 1]}))))

(deftest populate-test
  (testing "populatig of empty filed"
    (is (= f10x10
           (populate f10x10)
           (-> f10x10
               populate
               populate))))

  (let [block-in-3x3 (-> (empty-field 3 3)
                         (insert [0 0] 1)
                         (insert [1 0] 1)
                         (insert [0 1] 1)
                         (insert [1 1] 1))]
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
                 populate))))))
