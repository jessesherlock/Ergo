(ns ergo.async-mixed
  (:refer-clojure :exclude [iterate])
  (:require [clojure.core.async :as a]
            [ergo.async-utils :as utils]))

(defn iterate
  "An iteration transducer that iterates functions that may return a channel holding the result for the next iteration or may return the result"
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (a/go-loop [result result
                    input input]
          (let [input (utils/?<! input)
                result (rf result input)]
            (if (reduced? result)
              @result
              (recur result (f input))))))))))
