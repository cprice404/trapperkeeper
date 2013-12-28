(ns scratch.protocol-apis
  (:require [plumbing.core :refer [fnk]]))

(defprotocol ServiceLifecycle
  (init [this context]) ;; must return (possibly modified) context map
  (startup [this context])) ;; must return (possibly modified) context map

(defprotocol PrismaticGraphService
  (service-graph [this])) ;; must return a valid trapperkeeper/prismatic "service graph"

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
    (check-for-required-fns! fns-map service-fn-names (name service-protocol-sym))
    (check-for-required-fns! fns-map lifecycle-fn-names
                             ;; this is silly, but I wanted compile-time checking
                             ;; for the retrieval of the name string of the
                             ;; ServiceLifecycle protocol in case the name changes;
                             ;; could just hard-code the string here.
                             (name (:name (meta (var ServiceLifecycle)))))
    `(reify
       PrismaticGraphService
       (~'service-graph [~'this]
         {~(keyword service-protocol-sym)
           (fnk ~dependencies
                ~(reduce
                   (fn [acc fn-name] (assoc acc (keyword fn-name) `(partial ~fn-name ~'this)))
                   {}
                   service-fn-names))})

       ServiceLifecycle
       ~@(for [fn-name lifecycle-fn-names]
           (fns-map (keyword fn-name)))

       ~service-protocol-sym
       ~@(for [fn-name service-fn-names]
           (fns-map (keyword fn-name))))))

(defmacro defservice
  [svc-name & forms]
  `(def ~svc-name (service ~@forms)))
