(ns ergo.core-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [ergo.core :as sut :include-macros true]))

(deftest non-xform-iterate
  (testing "2 argument iterate works like clojure.core/iterate"
    (let [result (sut/iterate inc 1)
          first-5 (take 5 result)]
      (is (= #?(:clj clojure.lang.Iterate :cljs Iterate)
             (type result)))
      (is (= #?(:clj clojure.lang.LazySeq :cljs LazySeq)
             (type first-5)))
      (is (= [1 2 3 4 5] first-5)))))

(deftest test-iterate
  (testing "basic iterate and reduced stops iteration"
    (is (= [1 2 3 4]
           (transduce (comp (sut/iterate inc)
                            (take 4))
                      conj
                      [1]))))

  (testing "non-xf version"
    (is (= (list 1 2 3 4)
           (take 4 (sut/iterate inc 1))))))

(deftest test-reductions
  (testing "basic reductions"
    (is (= [[]
            [1]
            [1 2]
            [1 2 3]
            [1 2 3 4]]
           (transduce (sut/reductions conj []) conj [1 2 3 4]))))

  (testing "properly uses reduced"
    (is (= [[] [1] [1 2]]
           (transduce (comp (sut/reductions conj [])
                            (take 3))
                      conj [1 2 3 4])))))

(deftest test-produce
  (is (= [1 2 3 4]
         (sut/produce (comp (sut/iterate inc)
                          (take 4))
                    conj
                    []
                    1))))

(defn inc-until-5-then-nil
  [i]
  (if (not (= 5 i))
    (inc i)))

(deftest test-til-nil
  (is (= [1 2 3 4 5]
         (sut/produce (comp (sut/iterate inc-until-5-then-nil)
                            sut/til-nil)
                      conj
                      []
                      1))))

(defn inc-but-never-more-than-5
  [i]
  (min 5 (inc i)))

(deftest fixed-point
  (testing "= as eq-fn"
    (is (= [1 2 3 4 5]
           (sut/produce (comp (sut/iterate inc-but-never-more-than-5)
                              (sut/fixed-point =))
                        conj
                        []
                        1))))

  (testing "custom eq-fn"
    (is (= [5.0 2.5 1.25 0.625]
           (sut/produce (comp (sut/iterate (fn half [i] (/ i 2)))
                              (sut/fixed-point (fn close-enough [x y] (< (- x y) 0.5))))
                        conj
                        []
                        5.0))))

  (testing "nil-on-fixed-point version"
    (is (= [5.0 2.5 1.25 0.625]
           (sut/produce (comp (sut/iterate (fn half [i] (/ i 2)))
                              (sut/nil-on-fixed-point (fn close-enough [x y] (< (- x y) 0.5)))
                              sut/til-nil)
                        conj
                        []
                        5.0)))))

(deftest backtrack-test
  (is (= [1 [1 2] [2 3] [3 4] [4 5]]
         (sut/produce (comp (sut/iterate inc)
                            (sut/backtrack (fn [pi i] [pi i]))
                            (take 5))
                      conj
                      []
                      1))))

(deftest last-test
  (is (= 5
         (sut/produce (comp (sut/iterate inc-until-5-then-nil)
                            sut/til-nil)
                      sut/last
                      nil
                      1))))

(deftest thread-rf
  (is (= 42
         (reduce sut/thread-rf {:foo {:bar {:baz 42}}} [:foo :bar :baz]))))

(defn inc-to-5-then-throw
  [i]
  (let [i (inc i)]
    (if (> i 5)
      (throw (ex-info "more than 5" {:i i})))
    i))

(deftest exception-utilities
  (testing "halt-err halts on error"
    (is (= {:i 6}
           (->> (sut/produce (comp (sut/iterate (sut/catching inc-to-5-then-throw))
                                   sut/halt-err)
                             sut/last
                             nil
                             1)
                (#(if (sut/throwable? %) (ex-data %) %))))))
  (testing "catching catches"
    (is (= [1 2 3 4 5 {:i 6}]
           (->>
            (sut/produce (comp (sut/iterate (sut/catching inc-to-5-then-throw))
                               (take 6))
                         conj
                         []
                         1)
            (map #(if (sut/throwable? %) (ex-data %) %))))))
  (testing "take-until-err takes until error"
    (is (= [1 2 3 4 5]
           (sut/produce (comp (sut/iterate (sut/catching inc-to-5-then-throw))
                              sut/take-until-err)
                        conj
                        []
                        1))))
  (testing "take-until-after-err takes until after the error"
    (is (= [1 2 3 4 5 {:i 6}]
           (->>
            (sut/produce (comp (sut/iterate (sut/catching inc-to-5-then-throw))
                               sut/take-until-after-err)
                         conj
                         []
                         1)
            (map #(if (sut/throwable? %) (ex-data %) %)))))))
