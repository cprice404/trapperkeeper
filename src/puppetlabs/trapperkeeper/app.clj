(ns puppetlabs.trapperkeeper.app
  (:import (clojure.lang ExceptionInfo))
  (:require [plumbing.map]
            [plumbing.graph :refer [eager-compile]]))

;  A type representing a trapperkeeper application.  This is intended to provide
;  an abstraction so that users don't need to worry about the implementation
;  details and can pass the app object to our functions in a type-safe way.
;  The internal properties are not intended to be used outside of this
;  namespace.
(defrecord TrapperKeeperApp [graph-instance])

(def ^{:doc "Alias for plumbing.map/map-leaves-and-path, which is named inconsistently
            with Clojure conventions as it doesn't behave like other `map` functions.
            Map functions typically return a `seq`, never a map, and walk functions
            are used to modify a collection in-place without altering the structure."}
  walk-leaves-and-path plumbing.map/map-leaves-and-path)

(defn service-graph?
  "Predicate that tests whether or not the argument is a valid trapperkeeper
  service graph."
  [service-graph]
  (and
    (map? service-graph)
    (every? keyword? (keys service-graph))
    (every? (some-fn ifn? service-graph?) (vals service-graph))))

(defn validate-service-graph!
  "Validates that the argument is a valid trapperkeeper service graph.  Throws
  an IllegalArgumentException if it is not."
  [service-graph]
  (if (service-graph? service-graph)
    service-graph
    (throw (IllegalArgumentException. (str "Invalid service graph; service graphs must "
                                           "be nested maps of keywords to functions.  Found: "
                                           service-graph)))))

(defn parse-missing-required-key
  ;; TODO docs
  [m]
  (let [service-name    (first (keys m))
        service-fn-name (first (keys (m service-name)))
        error           (get-in m [service-name service-fn-name])]
    (if (= error 'missing-required-key)
      {:service-name (name service-name)
       :service-fn   (name service-fn-name)})))

(defn handle-prismatic-exception!
  ;; TODO DOCS
  [e]
  (let [data  (ex-data e)]
    ;; TODO: it seems like we could clean this up using cond-> to get rid of
    ;;  the repeated calls to `throw e`.
    (condp = (:error data)
      :missing-key
      (if (empty? (:map data))
        (throw (RuntimeException. (format "Service '%s' not found" (:key data))))
        (throw (RuntimeException. (format "Service function '%s' not found"
                                          (name (:key data))))))

      :does-not-satisfy-schema
      (if-let [error-info (parse-missing-required-key (:failures data))]
        (throw (RuntimeException. (format "Service function '%s' not found in service '%s'"
                                          (:service-fn error-info)
                                          (:service-name error-info))))
        (throw e))

      :else
      (throw e))))

(defn compile-graph
  "Given the merged map of services, compile it into a function suitable for instantiation.
  Throws an exception if there is a dependency on a service that is not found in the map."
  [graph-map]
  {:pre  [(service-graph? graph-map)]
   :post [(ifn? %)]}
  (try
    (eager-compile graph-map)
    (catch ExceptionInfo e
      (handle-prismatic-exception! e))))

(defn instantiate
  "Given the compiled graph function, instantiate the application. Throws an exception
  if there is a dependency on a service function that is not found in the graph."
  [graph-fn]
  {:pre  [(ifn? graph-fn)]
   :post [(service-graph? %)]}
  (try
    (graph-fn {})
    (catch ExceptionInfo e
      (handle-prismatic-exception! e))))
