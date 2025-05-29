(ns ergo.async-mixed-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer [deftest is]]
            [net.r4s6.test-async :as ta :include-macros true]
            [ergo.core :as ergo]
            [ergo.async-utils :as utils]
            [ergo.async-mixed :as sut]))

(defn async-inc [x] (a/go (inc x)))

(defn ->mixed-inc-fn
  [pred]
  (fn mixed-inc
    [x]
    (if (pred x)
      (async-inc x)
      (inc x))))

(deftest iterate-test
  (ta/async
      done
      (a/go
        (is (= [1 2 3 4 5]
               (a/<! (ergo/produce (comp (sut/iterate async-inc)
                                         (take 5))
                                   conj
                                   []
                                   1))

               (a/<! (ergo/produce (comp (sut/iterate inc)
                                         (take 5))
                                   conj
                                   []
                                   1))
               (a/<! (ergo/produce (comp (sut/iterate inc)
                                         (take 5))
                                   conj
                                   []
                                   (utils/->pchan 1)))

               (a/<! (ergo/produce (comp (sut/iterate (->mixed-inc-fn even?))
                                         (take 5))
                                   conj
                                   []
                                   1))
               (a/<! (ergo/produce (comp (sut/iterate (->mixed-inc-fn odd?))
                                         (take 5))
                                   conj
                                   []
                                   1))
               (a/<! (ergo/produce (comp (sut/iterate
                                          (->mixed-inc-fn #(= 0 (mod % 3))))
                                         (take 5))
                                   conj
                                   []
                                   1))))
        (done))))
