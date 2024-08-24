(ns ergo.async
  (:refer-clojure :exclude [iterate])
  (:require [clojure.core.async :as a]
            [ergo.core :as ergo]
            [ergo.async-utils :as utils]))

(defn iterate
  "An iteration transducer that iterates functions that return a channel holding the result for the next iteration"
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input-ch]
        (a/go-loop [result result
                    input-ch input-ch]
          (let [input (a/<! input-ch)]
            (let [result (rf result input)]
              (if (reduced? result)
                @result
                (recur result (f input)))))))))))


(defn produce
  "A version of produce for async steps (like ergo.async/iterate).
  For convenience the seed can be passed in without being in a channel"
  [xform f init seed]
  (ergo/produce xform f init (utils/->pchan seed)))


(defn put-rf!
  "A version of >! that can be used as a reducing fn. Useful for reducing things onto a channel.
  a closed result channel terminates the reduction, a nil input closes the channel.
  The completing arity closes the results channel"
  ([result]
   (a/go
     (let [result (a/<! result)]
       (a/close! result)
       result)))
  ([result input]
   (cond
     (nil? input) (do (a/close! result)
                      (reduced result))

     (not (a/put! result input)) (reduced result)

     :else result)))


