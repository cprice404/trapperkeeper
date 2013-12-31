(ns puppetlabs.trapperkeeper.experimental.services
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as g]
            [clojure.walk :refer [postwalk]]
            [puppetlabs.kitchensink.core :refer [select-values]]))

(defrecord ServiceDefinition [service-id service-map constructor])

(defprotocol App
  (get-service [this protocol]))

(defprotocol ServiceLifecycle
  (init [this context]) ;; must return (possibly modified) context map
  (startup [this context])) ;; must return (possibly modified) context map

;(defprotocol PrismaticGraphService
;  (service-graph [this]) ;; must return a valid trapperkeeper/prismatic "service graph"
;  (service-id [this])) ;; returns the identifier that is used to represent the service in the prismatic graph



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

  (let [result (and (seq? form)
                    (> (count form) 1)
                    (= service (second form))
                    (contains? fns (first form)))]
    (prn "is-fn-call?" fns "|" service "|" form "|" result)
    (if (and (seq? form) (> (count form) 1))
      (do
        ;(println "YO")
        ;(println "FORM!:" form)
        (prn "    second form:" (second form))
        (prn "    service equals second form?" (= service (second form)))
        (prn "    contains?" (contains? fns (first form)))
        (prn "    type first form:" (type (first form)))
        (prn "    type first fns:" (type (first fns)))
        (prn "    namespace first form:" (namespace (first form)))
        (prn "    namespace first fns:" (namespace (first fns)))
        ))
    result))

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
  (let [service-id            (keyword service-protocol-sym)
        service-protocol-var  (resolve service-protocol-sym)
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
      `(let [service-map#           (into {}
                                       ~(mapv
                                          (fn [f]
                                            (let [[fn-name fn-args & fn-body] f
                                                  [deps fn-body] (replace-fn-calls #{'fn1} 'this fn-body)]
                                              (println "F:" f)
                                              (println "fn-name:" fn-name)
                                              (println "fn-args:" fn-args)
                                              (println "fn-body:" fn-body)
                                              [(keyword fn-name) `(fnk [~@deps] (fn [~@(rest fn-args)] ~@fn-body))]))
                                          (select-values fns-map (map keyword service-fn-names))))
             service-graph-instance# ((g/eager-compile service-map#) {})]
         (ServiceDefinition. ~service-id
                             ;; service map for prismatic graph
                             {~service-id
                               (fnk ~dependencies
                                    service-graph-instance#)}
                                    ;~(reduce
                                    ;   (fn [acc fn-name]
                                    ;     (let [[_ [_ & fn-args] & fn-body] (fns-map (keyword fn-name))]
                                    ;       (assoc acc (keyword fn-name) `(fn [~@fn-args] ~@fn-body))))
                                    ;   {}
                                    ;   service-fn-names))}
                             ;; protocol-based service constructor function
                             (fn [~'graph]
                               (let [~'service-fns (~'graph ~service-id)]
                                 (reify
                                   ;PrismaticGraphService
                                   ;(~'service-id [~'this] ~service-id)
                                   ;(~'service-graph [~'this]
                                   ; {~service-id
                                   ;   (fnk ~dependencies
                                   ;
                                   ;        ~(reduce
                                   ;           (fn [acc fn-name]
                                   ;             (let [[_ [_ & fn-args] & fn-body] (fns-map (keyword fn-name))]
                                   ;               (assoc acc (keyword fn-name) `(fn [~@fn-args] ~@fn-body))))
                                   ;           {}
                                   ;           service-fn-names))})
                                   ServiceLifecycle
                                   ~@(for [fn-name lifecycle-fn-names]
                                       (fns-map (keyword fn-name)))

                                   ~service-protocol-sym
                                   ~@(for [fn-name service-fn-names]
                                       (let [[_ fn-args & _] (fns-map (keyword fn-name))]
                                         (list fn-name fn-args `((~'service-fns ~(keyword fn-name)) ~@(rest fn-args)))))
                                   ;(fns-map (keyword fn-name)))

                                   )
                                 )

              )))))

(defmacro defservice
  [svc-name & forms]
  `(def ~svc-name (service ~@forms)))

(defn boot!
  [services]
  (let [service-map    (apply merge (map :service-map services))
        graph          (g/->graph service-map)
        compiled-graph (g/eager-compile graph)
        graph-instance (compiled-graph {})
        services-by-id (into {} (map
                                  (fn [sd] [(:service-id sd)
                                            ((:constructor sd) graph-instance)])
                                  services))
        app            (reify
                         App
                         (get-service [this protocol] (services-by-id (keyword protocol))))]

    (doseq [lifecycle-fn [init startup]
            graph-entry  graph]
      (let [s (services-by-id (first graph-entry))]
        (lifecycle-fn s {})))
    app))

