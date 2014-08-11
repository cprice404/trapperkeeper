(ns service2-scratch
  (:import (clojure.lang IFn))
  (:require [clojure.test :refer :all]
            [clojure.tools.macro :refer [name-with-attributes]]
            [plumbing.core :as plumbing]
            [plumbing.graph :as graph]
            [schema.core :as schema]))

(schema/set-fn-validation! true)

(def ServiceSymbol
  "For internal use only; this schema gives us a way to differentiate between
  a symbol representing the name of a service, and a symbol representing the
  name of the *protocol* for a service.  This is necessary because the `service`
  macro accepts both as optional leading arguments when defining a service."
  {:service-symbol (schema/pred symbol?)})

(defn validate-fn-forms!
  [fns]
  (if (every? seq? fns)
    {:fns fns}
    (throw (IllegalArgumentException.
             (format
               "Invalid service definition; expected function definitions following dependency list, invalid value: '\"hi\"'"
               (pr-str (first (filter #(not (seq? %)) fns))))))))

(defn validate-deps-form!
  [forms]
  (let [f (first forms)]
    (if (vector? f)
      (merge {:binding-form f} (validate-fn-forms! (rest forms)))
      (throw (IllegalArgumentException.
               (format
                 "Invalid service definition; expected dependency list following protocol, found: '%s'"
                 (pr-str f)))))))

(defn find-prot-and-deps-forms!
  [forms]
  (let [f (first forms)]
    (cond
      (symbol? f) (merge {:protocol-sym f} (validate-deps-form! (rest forms)))
      (vector? f) (validate-deps-form! forms)
      :else (throw (IllegalArgumentException.
                     (format
                       "Invalid service definition; first form must be protocol or dependency list; found '%s'"
                       (pr-str f)))))))

(defn parse-forms
  [forms]
  (let [f (first forms)]
    (if (nil? (schema/check ServiceSymbol f))
      (merge {:service-symbol f} (find-prot-and-deps-forms! (rest forms)))
      (find-prot-and-deps-forms! forms))))

(defn build-fns-map
  [fns]
  (reduce (fn [acc fn-sym]
            (assoc acc (keyword fn-sym) fn-sym))
          {}
          (map first fns)))

(defn build-service-map
  [fns-map svc]
  (reduce (fn [acc [fn-name fn-impl]]
            (assoc acc fn-name
                       (partial fn-impl svc)))
          {}
          fns-map))

(defn build-output-schema
  [fns-map]
  (reduce (fn [acc fn-name] (assoc acc (keyword fn-name) IFn))
          {}
          (keys fns-map)))

(defmacro service
  [& forms]
  (let [{:keys [service-sym protocol-sym binding-form fns]} (parse-forms forms)
        fns-map       (build-fns-map fns)
        output-schema (build-output-schema fns-map)]
    `(plumbing/fnk service-fnk# :- ~output-schema
       ~binding-form
       (let [svc# (reify
                    ~protocol-sym
                    ~@fns)]
         (build-service-map ~fns-map svc#)))))

(defmacro defservice
  [svc-name & forms]
  (let [service-sym      (symbol (name (ns-name *ns*)) (name svc-name))
        [svc-name forms] (name-with-attributes svc-name forms)]
    `(def ~svc-name (service {:service-symbol ~service-sym} ~@forms))))

(defprotocol FooService
  (foo [this]))

(defprotocol BarService
  (bar [this]))

(defservice foo-service
  FooService
  []
  (foo [this] "foo"))

(defservice bar-service
  BarService
  [[:foo-service foo]]
  (bar [this] (str (foo) "bar")))

(deftest test-macro
  (let [m   (graph/graph :foo-service foo-service
                 :bar-service bar-service)
        g   ((graph/eager-compile m) {})
        bar (get-in g [:bar-service :bar])]
    (is (= "foobar" (bar)))))
