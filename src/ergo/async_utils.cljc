(ns ergo.async-utils
  (:require [clojure.core.async :as a]
            [ergo.core :as ergo])
  #?(:cljs (:require-macros [ergo.async_utils])))


(defn chan?
  [ch]
  (instance? #?(:clj clojure.core.async.impl.channels.ManyToManyChannel
                :cljs cljs.core.async.impl.channels.ManyToManyChannel)
             ch))

(defn pchan?
  [ch]
  (and (chan? ch)
       (instance? #?(:clj clojure.core.async.impl.buffers.PromiseBuffer
                     :cljs cljs.core.async.impl.buffers.PromiseBuffer)
                  ^#?(:clj clojure.core.async.impl.protocols.Buffer
                      :cljs cljs.core.async.impl.protocols.Buffer)
                  (.-buf ^clojure.core.async.impl.channels.ManyToManyChannel ch))))

(defn val->pchan
  "wrap a normal value in a promise channel"
  ([v] (val->pchan v (a/promise-chan)))
  ([v pc]
   (if v
     (a/put! pc v (fn [v] (a/close! pc)) true)
     (a/close! pc))
   pc))

(defn chan->pchan
  ([ch] (chan->pchan ch (a/promise-chan)))
  ([ch pc]
   (a/take! ch (fn [v] (if v (a/put! pc v) (a/close! pc))))
   pc))

(defn ->pchan
  ([x]
   (if (pchan? x)
     x
     (->pchan x (a/promise-chan))))
  ([x pc]
   (if (chan? x)
     (chan->pchan x pc)
     (val->pchan x pc))))

(defn pipe-mixed
  [from to]
  (if (chan? from)
    (a/pipe from to)
    (a/put! to from)))

(defn pchan-returning
  "f is a single arity function that takes a value and *may* return a channel,
  the returned fn is one that takes a value and always returns a promise-chan"
  [f]
  (fn pchan-returning-fn
    [x]
    (->pchan (f x))))

(defn apply-pch
  [f ch]
  (let [ret (a/promise-chan)]
    (a/take! ch (fn [v] (if v (a/put! ret (f v)) (a/close! ret))) true)
    ret))

;; exception handling utilities
;; thanks to David Nolen, https://gist.github.com/vvvvalvalval/f1250cec76d3719a8343 and
;; https://blog.jakubholy.net/2019/core-async-error-handling/ for some ideas

(defmacro <?
  [c]
  `(ergo/throw-err (a/<! ~c)))

(defmacro <??
  [c]
  `(ergo/throw-err (a/<!! ~c)))

(defmacro go-safe
  [& body]
  `(a/go (ergo/err-or ~@body)))

(defn consume-rest
  "Consume all remaining items on ch, useful for cleanup after errors
  so you can avoid go blocks hanging around and live/dead locks"
  [ch]
  (a/go-loop []
    (when (a/<! ch) (recur)))
  nil)

;; note: use let for maybe-ch so it isn't mentioned twice
;; since maybe-ch may be a form dealing with a channel (which is mutable)
(defmacro ?<!
  [maybe-ch]
  `(let [x# ~maybe-ch]
     (if (chan? x#) (a/<! ~maybe-ch) x#)))

(defmacro ?<?
  [maybe-ch]
  `(let [x# ~maybe-ch]
     (if (chan? x#) (<? x#) x#)))

(defmacro pgo
  [& body]
  `(->pchan (a/go ~@body)))

(defmacro pgo-safe
  [& body]
  `(->pchan (go-safe ~@body)))
