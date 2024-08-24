(ns ergo.core
  (:refer-clojure :exclude [iterate last reductions])
  (:require [clojure.core :as c])
  #?(:cljs (:use-macros [ergo.core :only [err-or]])))

(defn iterate
  "The classic iterate function, as in core, but with a transducer arity"
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [result (rf result input)]
          (if (reduced? result)
            @result
            (recur result (f input))))))))
  ([f x] #?(:clj (clojure.lang.Iterate/create f x)
            :cljs (Iterate. nil f nil x nil))))


(defn reductions
  ([f] (reductions f (f)))
  ([f init]
   (fn [rf]
     (let [vacc (volatile! init)]
       (fn
         ([] (rf))
         ([result]
          (let [result (unreduced (rf result @vacc))]
            (rf result)))
         ([result input]
          (let [result (rf result @vacc)]
            (if (reduced? result)
              @result
              (let [vacc (vswap! vacc f input)]
                (if (reduced? vacc)
                  (reduced result)
                  result))))))))))

(defn produce
  "Like transduce but for a scalar input rather than a collection.
  you could just call `(transduce xform f init [x])` or similar of course"
  ([xform f init seed]
   (let [f (xform f)
         ret (f init seed)]
     (f ret))))


;; * useful transducers to use with iterate

(def til-nil
  (take-while (complement nil?)))

(defn backtrack
  "Transducer to replace input with (f previous-input input). Stateful xform"
  [f]
  (fn [rf]
    (let [pi (volatile! ::none)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [previous-input @pi]
           (vreset! pi input)
           (if (#?(:clj identical? :cljs keyword-identical?)
                previous-input ::none)
             (rf result input)
             (rf result (f previous-input input)))))))))

(defn nil-on-fixed-point
  "A version of the fixed-point xf that works with til-nil.
  use ergo.core/fixed-point for an xf that works on it's own"
  [eq-fn]
  (backtrack (fn [pi i] (if-not (eq-fn pi i) i))))

(defn fixed-point
  "Takes a comparison function eq-fn and
  takes values until (eq-fn previous-input input) is true. "
  ([eq-fn]
   (fn [rf]
     (let [pi (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [previous-input @pi]
            (vreset! pi input)
            (if (and (not (#?(:clj identical? :cljs keyword-identical?)
                           previous-input ::none))
                     (eq-fn previous-input input))
              (reduced result)
              (rf result input)))))))))

;; * reducing fns

(defn last
  ([] nil)
  ([x] x)
  ([_ x] x))


(defn thread-rf
  ([] nil)
  ([v] v)
  ([v f]
   (f v)))

;; * error/exception related utilities

(def throwable?
  (partial instance? #?(:clj java.lang.Throwable :cljs js/Error)))

(defn take-until
  [pred]
  (take-while (complement pred)))

(def take-until-err
  (take-until throwable?))

(defn take-until-after
  [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (pred input)
         (reduced (rf result input))
         (rf result input))))))

(def take-until-after-err
  (take-until-after throwable?))

(def halt-err
  (halt-when throwable?))

(defn throw-err
  [v]
  (if (throwable? v) (throw v) v))


(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))


(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))


(defmacro err-or
  "Catch exceptions thrown in body and return them"
  [& body]
  `(if-cljs
    (try ~@body (catch :default e# e#))
    (try ~@body (catch java.lang.Throwable e# e#))))


(defn catching
  [f]
  (fn [& args] (err-or (apply f args))))
