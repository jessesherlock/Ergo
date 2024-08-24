(ns ergo.async-test
  (:require [clojure.core.async :as a]
            [clojure.test :as t :refer [deftest testing is]]
            [net.r4s6.test-async :as ta :include-macros true]
            [ergo.async :as sut]
            [ergo.async-utils :as utils]
            [ergo.core :as ergo]))

;; useful util fns for tests

(defn async-inc [x] (a/go (inc x)))

(deftest iterate-test
  (ta/async
   done
   (a/go
     (is (= [1 2 3 4 5]
            (a/<! (transduce (comp (sut/iterate async-inc)
                                   (take 5))
                             conj
                             []
                             [(utils/->pchan 1)]))))
     (done))))

(deftest produce-test
  (ta/async
   done
   (a/go
     (is (= [1 2 3 4 5]
            (a/<! (sut/produce (comp (sut/iterate async-inc)
                                     (take 5))
                               conj
                               []
                               1))))
     (done))))

(defn async-inc-until-5-then-nil
  [i]
  (a/go (if (not (= 5 i))
          (inc i))))

(deftest iterate-composes-properly
  (ta/async
   done
   (a/go
     (testing "comp with til-nil"
       (is (= [1 2 3 4 5]
              (a/<! (sut/produce (comp (sut/iterate async-inc-until-5-then-nil)
                                       ergo/til-nil)
                                 conj
                                 []
                                 1)))))
     (testing "reduce with last"
       (is (= 5
              (a/<! (sut/produce (comp (sut/iterate async-inc-until-5-then-nil)
                                       ergo/til-nil)
                                 ergo/last
                                 nil
                                 1)))))
     (done))))


(deftest put-rf-test
  (ta/async
   done
   (a/go
     (testing "basic put-rf!"
       (is (= [1 2 3 4 5]
              (->> (a/<! (sut/produce (comp (sut/iterate async-inc-until-5-then-nil)
                                            ergo/til-nil)
                                      sut/put-rf!
                                      (a/chan)
                                      1))
                   (a/into [])
                   (a/<!)))))

     (testing "put-rf channel close/reduced behaviour"
       ;; a/into doesn't return until the channel is closed
       ;; the nil will prompt put-rf! to close the channel and mark the result as reduced
       ;; (take 10) will short circuit as a result
       (is (= [1 2 3 4 5]
              (->> (a/<! (sut/produce (comp (sut/iterate async-inc-until-5-then-nil)
                                            (take 10))
                                      sut/put-rf!
                                      (a/chan)
                                      1))
                   (a/into [])
                   (a/<!)))))
     (done))))


(deftest put-rf-puts-incrementally
  ;; we want put-rf! to put values onto the result immediately, not when the inner reduction
  ;; is complete
  (ta/async
   done
   (a/go
     (let [output-ch (a/chan)
           control-ch (a/chan)
           inc-fn (fn [x] (a/go
                            (a/<! control-ch)
                            (inc x)))
           out (sut/produce (comp (sut/iterate inc-fn)
                                  (take 5))
                            sut/put-rf!
                            output-ch
                            1)]
       ;; first value is on the output
       (is (= 1 (a/<! output-ch)))
       ;; second shouldn't be since we haven't unparked inc-fn
       (is (nil? (a/poll! output-ch)))

       ;; unpark and the next value should be present
       (a/>! control-ch :tick)
       (is (= 2 (a/<! output-ch)))

       ;; unpark the rest and get the rest
       (dotimes [n 3] (a/>! control-ch :tick))
       (is (= [3 4 5] [(a/<! output-ch) (a/<! output-ch) (a/<! output-ch)]))

       ;; since it's complete the return channel of produce has the result
       (is (identical? output-ch (a/<! out)))

       ;; result channel is closed
       (is (nil? (a/<! output-ch))))
     (done))))


(defn async-inc-to-5-then-throw
  [i]
  (utils/go-safe
   (let [i (inc i)]
     (if (> i 5)
       (throw (ex-info "more than 5" {:i i})))
     i)))

(deftest exception-utilities
  (ta/async
   done
   (a/go
     (is (= [1 2 3 4 5]
            (a/<! (sut/produce (comp (sut/iterate
                                      (ergo/catching async-inc-to-5-then-throw))
                                     ergo/take-until-err
                                     (take 10))
                               conj
                               []
                               1))))
     (done))))
