(ns puppetlabs.trapperkeeper.services
  (:require [clojure.tools.macro :refer [name-with-attributes]]
            [clojure.set :refer [difference]]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as g]
            [puppetlabs.kitchensink.core :refer [select-values keyset]]
            [puppetlabs.trapperkeeper.services-internal :as si]))

(defprotocol ServiceLifecycle
  "Lifecycle functions for a service.  All services satisfy this protocol, and
  the lifecycle functions for each service will be called at the appropriate
  phase during the application lifecycle."
  (init [this context] "Initialize the service, given a context map.
                        Must return the (possibly modified) context map.")
  (start [this context] "Start the service, given a context map.
                         Must return the (possibly modified) context map.")
  (stop [this context] "Stop the service, given a context map.
                         Must return the (possibly modified) context map."))

(defprotocol Service
  "Common functions available to all services"
  (service-context [this] "Returns the context map for this service"))

(defprotocol ServiceDefinition
  "A service definition.  This protocol is for internal use only.  The service
  is not usable until it is instantiated (via `boot!`)."
  (service-id [this] "An identifier for the service")
  (service-map [this] "The map of service functions for the graph")
  (service-constructor [this] "A constructor function to instantiate the service"))

(def lifecycle-fn-names (map :name (vals (:sigs ServiceLifecycle))))

(defmacro service
  ;; TODO DOCS
  [& forms]
  (let [{:keys [service-protocol-sym service-id service-fn-names
                dependencies fns-map]}
                      (si/parse-service-forms!
                        lifecycle-fn-names
                        forms)
        ;;; we add 'context' to the dependencies list of all of the services.  we'll
        ;;; use this to inject the service context so it's accessible from service functions
        dependencies  (conj dependencies 'context)]
    `(reify ServiceDefinition
       (service-id [this] ~service-id)

       ;; service map for prismatic graph
       (service-map [this]
         {~service-id
           ;; the main service fnk for the app graph.  we add metadata to the fnk
           ;; arguments list to specify an explicit output schema for the fnk
           (fnk ~(si/fnk-binding-form dependencies service-fn-names)
              ;; create a function that exposes the service context to the service.
              (let [~'service-context (fn [] (get ~'@context ~service-id))
                    ;; here we create an inner graph for this service.  we need
                    ;; this in order to handle deps within a single service.
                    service-map#      ~(si/prismatic-service-map
                                         (concat lifecycle-fn-names service-fn-names)
                                         fns-map)
                    s-graph-inst#     (if (empty? service-map#)
                                        {}
                                        ((g/eager-compile service-map#) {}))]
                s-graph-inst#))})

       ;; service constructor function
       (service-constructor [this]
         ;; the constructor requires the main app graph and context atom as input
         (fn [graph# context#]
           (let [~'service-fns (graph# ~service-id)]
             ;; now we instantiate the service and define all of its protocol functions
             (reify
               Service
               (service-context [this] (get @context# ~service-id {}))

               ServiceLifecycle
               ~@(si/protocol-fns lifecycle-fn-names fns-map)

               ~@(if service-protocol-sym
                  `(~service-protocol-sym
                    ~@(si/protocol-fns service-fn-names fns-map))))))))))

(defmacro defservice
  [svc-name & forms]
  (let [[svc-name forms] (name-with-attributes svc-name forms)]
    `(def ~svc-name (service ~@forms))))

