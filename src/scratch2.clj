(ns scratch2
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            [plumbing.map :as map]
            [plumbing.fnk.pfnk :as pfnk]
            [clojure.pprint :refer [pprint]]))

;; scratch sandbox for playing around with the idea of exposing a
;; context map to each individual service.

;; alias prismatic's 'walk' function to a more clear name:
(def walk-leaves-and-path map/map-leaves-and-path)

;; for now we'll just create one big atom to hold the system context
(def system (atom {}))

;; some dynamic vars that we can bind for use in interacting with the
;; service context
(def ^{:dynamic true} *get-in-context* nil)
(def ^{:dynamic true} *assoc-in-context* nil)

;; a dynamic var that we can bind

;; Now we create a couple of services to play with.
(defn service1
  []
  {:service1
    (fnk []
         (println "Service1 initializing!")
         {
          ;; These first two fns are weird.  I don't expect there would ever
          ;; be a real-world use-case where we'd want to directly expose access
          ;; to the service context to the outside world, but for now they'll
          ;; be useful for testing.
          :set-context-var (fn [v] (*assoc-in-context* [:key] v))
          :get-context-var (fn [] (*get-in-context* [:key]))

          ;; And this would be an actual public function that would presumably
          ;; perform some more interesting computation that might happen to
          ;; be influenced by the context.
          :service1-public-fn (fn [] (*get-in-context* [:key]))})})

(defn service2
  []
  {:service2
    (fnk [[:service1 service1-public-fn]]
         ;; here we are just going to hard-code the context :key for this service
         ;; to :bar.
         (*assoc-in-context* [:key] :bar)
         {:service2-public-fn (fn []
                                {:service1-val (service1-public-fn)
                                 :service2-val (*get-in-context* [:key])})})})

;; Now a var that contains the merged service map
(def main-service-map (merge (service1) (service2)))

;; And a utility function for compiling and instantiating the graph
(defn graph-instance
  [service-map]
  ((graph/eager-compile service-map) {}))

;; Here's a utility function to play with the whole system, so that we
;; can call it multiple times.
(defn run-stuff
  [service-map]
  (let [mygraph   (graph-instance service-map)
        s1-getter (get-in mygraph [:service1 :get-context-var])
        s1-setter (get-in mygraph [:service1 :set-context-var])
        s2-fn     (get-in mygraph [:service2 :service2-public-fn])]
    (println "Graph compiled, functions bound.")
    (println "Initial value for s1 context var:" (s1-getter))
    (println "Setting s1 context var to :foo.")
    (s1-setter :foo)
    (println "s1 context var is now:" (s1-getter))
    (println "s2-fn returns: " (s2-fn))))

;; OK.  So if we try to run this now, we (shockingly!) get an exception, because
;; the services reference dynamic vars that are not bound to anything.
(try
  (run-stuff main-service-map)
  (catch Throwable t
    (println "Caught an exception!:" t)))

;; So now we need to add the bindings so that those dynamic vars won't be
;; nil.  We're not going to be modifying the schemas at all so I'm going to
;; copy in my helper function from the previous examples...
(defn fn->fnk-with-same-schema
  [f orig-fnk]
  (let [in  (pfnk/input-schema orig-fnk)
        out (pfnk/output-schema orig-fnk)]
    (pfnk/fn->fnk f [in out])))

;; And now, to write a wrapper function that will do the context var bindings:
(defn wrap-with-context-bindings
  [p orig-fnk]
  ;; alright, when we get in here we know what service we're messing with
  ;; based on 'p'.  The first thing we'll do is create the context getter/setter
  ;; functions that we want the service to have access to.
  (let [assoc-in-fn (fn [ks v] (swap! system assoc-in (concat p ks) v))
        get-in-fn   (fn [ks] (get-in @system (concat p ks)))
        ;; now we'll create our new function that we're eventually going to
        ;; replace the original fnk with.
        f (fn [injected-vals]
            ;; before we do anything else, we want to bind the dynamic vars to
            ;; those getter/setter fns.  That way, the body of the original fnk
            ;; has access to them.
            (binding [*get-in-context* get-in-fn
                      *assoc-in-context* assoc-in-fn]
              ;; now we'll call the original fnk and get it's service map,
              ;; which we can walk and manipulate.
              (let [orig-fnk-service-map (orig-fnk injected-vals)
                    ;; Each leaf of the service map should be a function, and
                    ;; basically our goal here is to wrap each of those functions
                    ;; with a function that contains our bindings.
                    ;;
                    ;; Note that a binding only lasts for the duration of the call
                    ;; stack that it encapsulates.  So even though we're inside of
                    ;; a binding right now, we're about to return a bunch of
                    ;; functions that will be executed *outside* of this call stack.
                    ;; That's why we also need the bindings inside of these wrapped
                    ;; functions.
                    ;;
                    ;; So.. let's create a function for use with 'walk' that
                    ;; simply returns a new function, which wraps the service
                    ;; functions with the bindings.
                    wrap-fn   (fn [p service-fn]
                                (fn [& args]
                                  (binding [*get-in-context* get-in-fn
                                            *assoc-in-context* assoc-in-fn]
                                    (apply service-fn args))))]
              ;; and now we can do the walk, and just return the resulting map
              ;; as the final service map for the service
              (walk-leaves-and-path wrap-fn orig-fnk-service-map))))]
    ;; and the last step--we have our new function, we just need to turn it
    ;; back into an fnk.
    (fn->fnk-with-same-schema f orig-fnk)))

;; so now we have our main wrapper function that we can apply to the full service
;; map:
(let [context-service-map (walk-leaves-and-path wrap-with-context-bindings main-service-map)]
  ;; and then run!
  (run-stuff context-service-map))





