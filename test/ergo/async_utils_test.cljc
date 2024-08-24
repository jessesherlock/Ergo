(ns ergo.async-utils-test
  (:require [clojure.core.async :as a]
            [clojure.test :as t :refer [deftest is testing]]
            [net.r4s6.test-async :as ta :include-macros true]
            [ergo.async-utils :as sut]))

(deftest val->pchan-test
  (ta/async
   done
   (a/go
     (testing "single arity value argument"
       (is (sut/pchan? (sut/val->pchan :value)))
       (is (= :value
              (a/<! (sut/val->pchan :value))))
       (is (= nil
              (a/<! (sut/val->pchan nil)))))

     (testing "two arity value and result channel arguments"
       (let [pc-arg (a/promise-chan)
             pc (sut/val->pchan :value pc-arg)]
         (is (= :value
                (a/<! pc)))))

     (testing "two arity with closed output channel"
       (let [pc-arg (a/promise-chan)
             _ (a/close! pc-arg)
             pc (sut/val->pchan :value pc-arg)]
         (is (= nil
                (a/<! pc)))))
     (done))))

(deftest chan->pchan-test
  (ta/async
   done
   (a/go
     (testing "regular channel argument"
       (let [test-pc (sut/chan->pchan (a/to-chan! [:value]))]
         (is (sut/pchan? test-pc))
         (is (= :value
                (a/<! test-pc)))))

     (testing "regular channel and output promise channel arguments"
       (let [input-ch (a/to-chan! [:value])
             test-pc (a/promise-chan)
             _ (sut/chan->pchan input-ch test-pc)]
         (is (= :value
                (a/<! test-pc)))))

     (testing "promise channel argument is closed if input is empty and closed"
       (let [input-ch (a/chan)
             _ (a/close! input-ch)
             test-pc (sut/chan->pchan input-ch)]
         (is (nil? (a/<! test-pc)))))
     (done))))

(deftest ->pchan-test
  (ta/async
   done
   (a/go
     (testing "single-arity value argument"
       (let [test-pc (sut/->pchan :value)]
         (is (sut/pchan? test-pc))
         (is (= :value
                (a/<! test-pc)))))

     (testing "single-arity channel argument"
       (let [test-pc (sut/->pchan (a/to-chan! [:value]))]
         (is (sut/pchan? test-pc))
         (is (= :value
                (a/<! test-pc)))))

     (testing "already a promise channel, just return it"
       (let [in-pc (sut/val->pchan :value)
             out-pc (sut/->pchan in-pc)]
         (is (identical? in-pc out-pc))))

     (testing "with output channel argument"
       (let [out-pc (a/promise-chan)
             _ (sut/->pchan :value out-pc)
             out-pc2 (a/promise-chan)
             _ (sut/->pchan (a/to-chan! [:value]) out-pc2)]
         (is (= :value
                (a/<! out-pc)
                (a/<! out-pc2)))))
     (done))))

(deftest pchan-returning-test
  (ta/async
   done
   (a/go
     (testing "normal fn is pchan"
       (let [test-fn (sut/pchan-returning inc)
             output (test-fn 1)]
         (is (sut/pchan? output))
         (is (= 2
                (a/<! output)))))

     (testing "channel returning fn is pchan"
       (let [f (fn [x] (a/to-chan! [(inc x)]))
             test-fn (sut/pchan-returning f)
             output (test-fn 1)]
         (is (sut/pchan? output))
         (is (= 2
                (a/<! output)))))
     (done))))

(deftest apply-pch-test
  (ta/async
   done
   (a/go
     (is (= 2
            (a/<! (sut/apply-pch inc (a/to-chan! [1])))))
     (is (nil? (a/<! (sut/apply-pch inc (a/to-chan! [])))))
     (done))))

(deftest exception-utilities
  (ta/async
   done
   (a/go
     (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"more than 5"
                           (sut/<? (sut/go-safe (throw (ex-info "more than 5" {:i 5}))))))
     (done))))
