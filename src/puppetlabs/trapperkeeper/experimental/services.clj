(ns puppetlabs.trapperkeeper.experimental.services
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as g]))

(defrecord ServiceDefinition [service-id service-map constructor])

(defprotocol App
  (get-service [this protocol]))

(defprotocol ServiceLifecycle
  (init [this context]) ;; must return (possibly modified) context map
  (startup [this context])) ;; must return (possibly modified) context map

(defprotocol PrismaticGraphService
  (service-graph [this]) ;; must return a valid trapperkeeper/prismatic "service graph"
  (service-id [this])) ;; returns the identifier that is used to represent the service in the prismatic graph

(defn protocol?
  ;; TODO DOCS
  [p]
  (and (map? p)
       (contains? p :on)
       (instance? Class (resolve (:on p)))))

(defn check-for-required-fns!
  ;; TODO Docs preconds
  [fns-map required-fn-names protocol-name]
  (doseq [fn-name required-fn-names]
    (if-not (contains? fns-map (keyword fn-name))
      (throw (IllegalArgumentException.
               (format "Service does not implement required function '%s'" fn-name))))))

(defmacro service
  [service-protocol-sym dependencies & fns]
  (let [service-protocol-var  (resolve service-protocol-sym)
        _                     (if-not service-protocol-var
                                (throw (IllegalArgumentException.
                                         (format "Unrecognized service protocol '%s'" service-protocol-sym))))
        service-protocol      (var-get (resolve service-protocol-sym))
        ;; TODO: verify that the service protocol doesn't define any functions
        ;;  whose names collide with our built-ins.
        _                     (if-not (protocol? service-protocol)
                                (throw (IllegalArgumentException.
                                         (format "Specified service protocol '%s' does not appear to be a protocol!"))))
        service-fn-names      (map :name (vals (:sigs service-protocol)))
        lifecycle-fn-names    (map :name (vals (:sigs ServiceLifecycle)))
        fns-map               (reduce (fn [acc f] (assoc acc (keyword (first f)) f)) {} fns)
        ;; TODO: verify that there are no functions in fns-map that aren't in one
        ;; of the two protocols
        ]
      `(ServiceDefinition. ~(keyword service-protocol-sym) {}
          (fn []
            ;(println "Constructing service; protocol:" ~service-protocol-sym
            ;         "\n\t(" (type ~service-protocol-sym) ")")
            (reify
              PrismaticGraphService
              (~'service-id [~'this] ~(keyword service-protocol-sym))
              (~'service-graph [~'this]
              {~(keyword service-protocol-sym)
                (fnk ~dependencies

                            ~(reduce
                               ;(fn [acc fn-name] (assoc acc (keyword fn-name) `(partial ~fn-name ~'this)))
                               (fn [acc fn-name]
                                 (let [[_ [_ & fn-args] & fn-body] (fns-map (keyword fn-name))]
                                   (assoc acc (keyword fn-name) `(fn [~@fn-args] ~@fn-body))))
                               {}
                               service-fn-names))})
              ServiceLifecycle
              ~@(for [fn-name lifecycle-fn-names]
                  (fns-map (keyword fn-name)))

              ~service-protocol-sym
              ~@(for [fn-name service-fn-names]
                  (fns-map (keyword fn-name))))))))

  ;(let [service-protocol-var  (resolve service-protocol-sym)
  ;      _                     (if-not service-protocol-var
  ;                              (throw (IllegalArgumentException.
  ;                                       (format "Unrecognized service protocol '%s'" service-protocol-sym))))
  ;      service-protocol      (var-get (resolve service-protocol-sym))
  ;      ;; TODO: verify that the service protocol doesn't define any functions
  ;      ;;  whose names collide with our built-ins.
  ;      _                     (if-not (protocol? service-protocol)
  ;                              (throw (IllegalArgumentException.
  ;                                       (format "Specified service protocol '%s' does not appear to be a protocol!"))))
  ;      service-fn-names      (map :name (vals (:sigs service-protocol)))
  ;      lifecycle-fn-names    (map :name (vals (:sigs ServiceLifecycle)))
  ;      fns-map               (reduce (fn [acc f] (assoc acc (keyword (first f)) f)) {} fns)
  ;      ;; TODO: verify that there are no functions in fns-map that aren't in one
  ;      ;; of the two protocols
  ;      ]
  ;  (check-for-required-fns! fns-map service-fn-names (name service-protocol-sym))
  ;  (check-for-required-fns! fns-map lifecycle-fn-names
  ;                           ;; this is silly, but I wanted compile-time checking
  ;                           ;; for the retrieval of the name string of the
  ;                           ;; ServiceLifecycle protocol in case the name changes;
  ;                           ;; could just hard-code the string here.
  ;                           (name (:name (meta (var ServiceLifecycle)))))
  ;  `(reify
  ;     PrismaticGraphService
  ;     (~'service-id [~'this] ~(keyword service-protocol-sym))
  ;     (~'service-graph [~'this]
  ;       {~(keyword service-protocol-sym)
  ;         (fnk ~dependencies
  ;              ~(reduce
  ;                 ;(fn [acc fn-name] (assoc acc (keyword fn-name) `(partial ~fn-name ~'this)))
  ;                 (fn [acc fn-name]
  ;                   (let [[_ [_ & fn-args] & fn-body] (fns-map (keyword fn-name))]
  ;                     (assoc acc (keyword fn-name) `(fn [~@fn-args] ~@fn-body))))
  ;                 {}
  ;                 service-fn-names))})
  ;
  ;     ServiceLifecycle
  ;     ~@(for [fn-name lifecycle-fn-names]
  ;         (fns-map (keyword fn-name)))
  ;
  ;     ;~service-protocol-sym
  ;     ;~@(for [fn-name service-fn-names]
  ;     ;    (fns-map (keyword fn-name)))
  ;     )))

(defmacro defservice
  [svc-name & forms]
  `(def ~svc-name (service ~@forms)))

(defn boot!
  [services]
  ;{:pre [(every? #(instance? ServiceDefinition %) services)]}
  (println "BOOTING!")
  (doseq [s services]
    (println "CHECKING SERVICE:" s)
    (println "TYPE of ServiceDefinition:" (type ServiceDefinition))
    (println "is servicedef?" (instance? ServiceDefinition s)))
  (let [services-by-id (into {} (map (fn [sd] [(:service-id sd) ((:constructor sd))]) services))
        _              (println "SERVICES BY ID:" services-by-id)
        service-map    (apply merge (map service-graph (vals services-by-id)))
        _              (println "SERVICE MAP:" service-map)
        graph          (g/->graph service-map)
        compiled-graph (g/eager-compile graph)
        graph-instance (compiled-graph {})
        app            (reify
                         App
                         (get-service [this protocol] (services-by-id (keyword protocol))))]

    (doseq [lifecycle-fn [init startup]
            graph-entry  graph]
      (let [s (services-by-id (first graph-entry))]
        (lifecycle-fn s {})))
    app))
  ;(let [services-by-id  (into {} (map (fn [s] [(service-id s) s]) services))
  ;      service-map     (apply merge (map service-graph services))
  ;      graph           (g/->graph service-map)
  ;      compiled-graph  (g/eager-compile graph)
  ;      graph-instance  (compiled-graph {})]
  ;
  ;  (doseq [lifecycle-fn [init startup]
  ;          graph-entry graph]
  ;    (let [s (services-by-id (first graph-entry))]
  ;      (lifecycle-fn s {})))
  ;
  ;  (println "Booted!")
  ;  graph-instance)

