(ns puppetlabs.trapperkeeper.experimental.scratchpattern
  (:require [clojure.walk :refer [postwalk]]))
;
;(defn replace-pattern*
;  [pattern acc x]
;  {:pre [(vector? acc)]
;   :post [(vector? %)
;          (vector? (first %))
;          (if (seq? x)
;            (vector? (second %))
;            (not (seq? (second %))))]}
;  (println "R-p* called with:")
;  (println "\tpattern:" pattern)
;  (println "\tacc:" acc)
;  (println "\tx:" x)
;  (if-not (seq? x)
;    [acc x]
;    (reduce
;      (fn [[inner-acc inner-x] [replaced-acc replaced]]
;        (println "reducing")
;        (let [new-acc (conj inner-acc replaced-acc)
;              new-x   (conj inner-x replaced)]
;          [new-acc new-x]))
;      [acc []]
;      (map (partial replace-pattern* pattern acc) x)
;      )))
;
;(defn replace-pattern
;  [pattern x]
;  {:post [(vector? %)
;          (sequential? (first %))
;          (if (seq? x)
;            (seq? (second %))
;            (not (seq? (second %))))]}
;  (let [[matches result] (replace-pattern* pattern [] x)
;        result           (if (seq? x) (seq result) result)]
;    [matches result]))

;(defn replace-pattern*
;  [matches pattern x]
;  (if-not (seq? x)
;    x
;    (let [patsize (count pattern)]
;      (if (and (>= (count x) patsize)
;               (= (take patsize x) pattern))
;        (cons :match (nthrest x patsize))
;        x))))
;
;(defn replace-pattern
;  [pattern x]
;  (let [matches (atom {})
;        result  (postwalk
;                  (partial replace-pattern* matches pattern)
;                  x)]
;    [(keys @matches) result]))

(defn postwalk-with-accumulator
  [matches? replace accumulate form]
  (postwalk
    (fn [x]
      (if-not (matches? x)
        x
        (do
          (accumulate x)
          (replace x))))
    form))

(defn is-fn-call?
  [fns service form]
  (and (seq? form)
       (> (count form) 1)
       (= service (second form))
       (contains? fns (first form))))

(defn replace-fn-calls
  [fns service form]
  (let [replace     (fn [form] (cons (first form) (nthrest form 2)))
        acc         (atom {})
        accumulate  (fn [form] (swap! acc assoc (first form) true))
        result      (postwalk-with-accumulator
                      (partial is-fn-call? fns service)
                      replace
                      accumulate
                      form)]
    [(keys @acc) result]))

(println
  (replace-fn-calls #{:a :c} :b '(:d (:a :b) {:c :a :b :d} [:e (:a :b)])))


