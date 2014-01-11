(ns puppetlabs.trapperkeeper.experimental.services
  (:import (java_service_example ServiceImpl))
  (:require [clojure.tools.macro :refer [name-with-attributes]]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as g]
            [puppetlabs.kitchensink.core :refer [select-values]]
            [puppetlabs.trapperkeeper.experimental.services-internal :as si]))

(defprotocol ServiceLifecycle
  "Lifecycle functions for a service.  All services satisfy this protocol, and
  the lifecycle functions for each service will be called at the appropriate
  phase during the application lifecycle."
  (init [this context] "Initialize the service, given a context map.
                        Must return the (possibly modified) context map.")
  (start [this context] "Start the service, given a context map.
                         Must return the (possibly modified) context map."))

(defprotocol Service
  "Common functions available to all services"
  (service-context [this] "Returns the context map for this service"))

(defprotocol TrapperkeeperApp
  "Functions available on a trapperkeeper application instance"
  (get-service [this service-id] "Returns the service with the given service id"))

(defprotocol ServiceDefinition
  "A service definition.  This protocol is for internal use only.  The service
  is not usable until it is instantiated (via `boot!`)."
  (service-id [this] "An identifier for the service")
  (service-map [this] "The map of service functions for the graph")
  (constructor [this] "A constructor function to instantiate the service"))

(defn check-for-required-fns!
  ;; TODO Docs preconds
  [fns-map required-fn-names protocol-name]
  (doseq [fn-name required-fn-names]
    (if-not (contains? fns-map (keyword fn-name))
      (throw (IllegalArgumentException.
               (format "Service does not implement required function '%s'" fn-name))))))


(defmacro service
  ;; TODO DOCS
  [service-protocol-sym dependencies & fns]
  (let [service-id            (keyword service-protocol-sym)
        service-protocol-var  (resolve service-protocol-sym)
        _                     (if-not service-protocol-var
                                (throw (IllegalArgumentException.
                                         (format "Unrecognized service protocol '%s'" service-protocol-sym))))
        service-protocol      (var-get (resolve service-protocol-sym))
        _                     (if-not (si/protocol? service-protocol)
                                (throw (IllegalArgumentException.
                                         (format "Specified service protocol '%s' does not appear to be a protocol!"))))
        service-fn-names      (map :name (vals (:sigs service-protocol)))
        ;; TODO: verify that the service protocol doesn't define any functions
        ;;  whose names collide with our built-ins.
        lifecycle-fn-names    (map :name (vals (:sigs ServiceLifecycle)))
        ;; TODO: verify that there are no functions in fns-map that aren't in one
        ;; of the two protocols
        fns-map               (reduce (fn [acc f] (assoc acc (keyword (first f)) f)) {} fns)
        fns-map               (si/add-default-lifecycle-fns lifecycle-fn-names fns-map)
        ;; we add 'context' to the dependencies list of all of the services.  we'll
        ;; use this to inject the service context so it's accessible from service functions
        dependencies          (conj dependencies 'context)]

    (check-for-required-fns! fns-map service-fn-names (name service-protocol-sym))
    (check-for-required-fns! fns-map lifecycle-fn-names (name (:name (meta (var ServiceLifecycle)))))

    ;; let the show begin!  The main thing the macro does is to create an
    ;; object that satisfies the ServiceDefinition protocol.
    `(reify ServiceDefinition
       ;; provide a unique identifier for the service (we just use a keyword
       ;; representation of the service protocol name
       (service-id [this] ~service-id)

       ;; service map for prismatic graph
       (service-map [this]
         {~service-id
           ;; the main service fnk for the app graph.  we add metadata to the fnk
           ;; arguments list to specify an explicit output schema for the fnk
           (fnk ~(si/fnk-binding-form dependencies service-fn-names)
              ;; create a function that exposes the service context to the service.
              ;; we use ~' to force literal symbols for this, because we must
              ;; match the name of the protocol function exactly since that
              ;; is what the service functions will be written against.
              (let [~'service-context (fn [] (get ~'@context ~service-id))
                    ;; here we create an inner graph for this service.  this is
                    ;; how we allow service functions for this service to call
                    ;; other functions from this service.
                    service-map#      (into {}
                                        ~(vec (let [fns (select-values fns-map (map keyword service-fn-names))
                                                    fns (si/protocol-fns->prismatic-fns fns service-fn-names)]
                                                (for [f fns]
                                                  [(:name f) `(fnk [~@(:deps f)] (fn [~@(:args f)] ~@(:body f)))]))))
                    ;; compile the inner graph so that we end up with a map of
                    ;; regular functions,  which is what the outer graph expects.
                    s-graph-inst#     ((g/eager-compile service-map#) {})]
                s-graph-inst#))})

       ;; protocol-based service constructor function
       (constructor [this]
         ;; the constructor requires the main app graph and context atom as input
         (fn [graph# context#]
           (let [~'service-fns (graph# ~service-id)]
             ;; now we instantiate the service and define all of its protocol functions
             (reify
               Service
               (service-context [this] (get @context# ~service-id {}))

               ;; TODO: provide no-op default implementations of lifecycle functions
               ServiceLifecycle
               ~@(for [fn-name lifecycle-fn-names]
                   ;; the lifecycle functions don't require any munging, so
                   ;; we can just pull them back out of the fns-map
                   (fns-map (keyword fn-name)))

               ~service-protocol-sym
               ~@(for [fn-name service-fn-names]
                   (let [[_ fn-args & _] (fns-map (keyword fn-name))]
                     ;; for the service protocol functions, we'll just proxy them to the
                     ;; functions in the prismatic graph, where all of the dependencies and
                     ;; such have been taken care of.
                     `(~fn-name ~fn-args
                       ;; look up the service fn in the graph
                       ((~'service-fns ~(keyword fn-name))
                        ;; remove 'this' from the args that we pass, because it's
                        ;; implicit at this point and not used in the graph.
                        ~@(rest fn-args))))))))))))

(defmacro defservice
  [svc-name & forms]
  (let [[svc-name forms] (name-with-attributes svc-name forms)]
    `(def ~svc-name (service ~@forms))))

(defn boot!
  [services]
  {:pre [(every? #(satisfies? ServiceDefinition %) services)]
   :post [(satisfies? TrapperkeeperApp %)]}
  (let [service-map    (apply merge (map service-map services))
        ;; this gives us an ordered graph that we can use to call lifecycle
        ;; functions in the correct order later
        graph          (g/->graph service-map)
        compiled-graph (g/eager-compile graph)
        ;; this is the application context for this app instance.  its keys
        ;; will be the service ids, and values will be maps that represent the
        ;; context for each individual service
        context        (atom {})
        ;; when we instantiate the graph, we pass in the context atom.
        graph-instance (compiled-graph {:context context})
        ;; here we build up a map of all of the services by calling the
        ;; constructor for each one
        services-by-id (into {} (map
                                  (fn [sd] [(service-id sd)
                                            ((constructor sd) graph-instance context)])
                                  services))
        ;; finally, create the app instance
        app            (reify
                         TrapperkeeperApp
                         (get-service [this protocol] (services-by-id (keyword protocol))))]

    ;; iterate over the lifecycle functions in order
    (doseq [[lifecycle-fn lifecycle-fn-name]  [[init "init"] [start "start"]]
            ;; and iterate over the services, based on the ordered graph so
            ;; that we know their dependencies are taken into account
            graph-entry                       graph]

      (let [service-id    (first graph-entry)
            s             (services-by-id service-id)
            ;; call the lifecycle function on the service, and keep a reference
            ;; to the updated context map that it returns
            updated-ctxt  (lifecycle-fn s (get @context service-id {}))]
        (if-not (map? updated-ctxt)
          (throw (IllegalStateException.
                   (format
                     "Lifecycle function '%s' for service '%s' must return a context map (got: %s)"
                     lifecycle-fn-name
                     service-id
                     (pr-str updated-ctxt)))))
        ;; store the updated service context map in the application context atom
        (swap! context assoc service-id updated-ctxt)))
    app))

