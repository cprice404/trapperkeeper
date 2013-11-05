(ns scratch
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            [plumbing.map :as map]
            [plumbing.fnk.pfnk :as pfnk]
            [clojure.pprint :refer [pprint]]))

;; This is a sort of tutorial on some advanced features of the prismatic
;; graph library; mostly around applying transformations to the service
;; maps before compiling them to graphs.  If you'd like to play along at home,
;; the easiest thing to do is probably to fire up a REPL and paste in the
;; bits of code one section at a time.

;; First we'll create a couple of services to play with.  Note that I'm putting
;; a println in the body of the config service, so that we can see when it gets
;; executed.  This is a parallel to the "let"-based startup/initialization code
;; that Nate's been experimenting with.
(defn config-service
  []
  {:config-service
    (fnk []
         (println "Config service initializing!")
         {:config (fn [k] ({:foo "foo" :bar "bar"} k))
          :shutdown (fn [] (println "Config service shutting down!"))})})

(defn hello-service
  []
  {:hello-service
    (fnk [[:config-service config]]
         {:hello (fn [] (format "Hello, %s!" (config :foo)))
          :shutdown (fn [] (println "Hello service shutting down!"))})})

;; Now a var that contains the merged service map
(def main-service-map (merge (config-service) (hello-service)))

;; And a utility function for compiling and instantiating the graph
(defn graph-instance
  [service-map]
  ((graph/eager-compile service-map) {}))

;; Now let's make sure the basic graph does what we expect
(println "1) testing basic graph...")
(let [hello-fn (get-in (graph-instance main-service-map) [:hello-service :hello])]
  (println (hello-fn)))
(println)

;; OUTPUT:
;1) testing basic graph...
;Config service initializing!
;Hello, foo!
;-------------------------------

;; Cool.  Now let's play around with a graph function that allow us to
;; walk over the full graph and manipulate it.  This function is poorly
;; named (inconsistent with clojure conventions), so I will make an alias here:
(def walk-leaves-and-path map/map-leaves-and-path)

;; The function above accepts two arguments, a function f and a map (of the sort
;; that the graph library expects to deal with; keywords -> fns).
;;
;; f itself takes two arguments; a path p and a value v.  f will be invoked on
;; each leaf node of the map; p will be a vector of keywords that gives the path
;; in the map, and v will be the value in the map.
;;
;; So, we can write a simple f that will just print that stuff out.
(defn print-path-and-val
  [p v]
  (println (format "Path: '%s'; value: '%s'" p v)))

;; Now we can see what happens when we walk that f over the service map:
(println "2) walking the service map...")
(walk-leaves-and-path print-path-and-val main-service-map)

;; OUTPUT:
;2) walking the service map...
;Path: '[:hello-service]'; value: 'clojure.lang.AFunction$1@127a7800'
;Path: '[:config-service]'; value: 'clojure.lang.AFunction$1@265c517a'
;-------------------------------

;; hmm.  This isn't quite what we wanted; this is basically just showing us the
;; fnks.  We really need to be able to see the service functions that each
;; service provides so that we can check and see if they provide, e.g., a
;; :shutdown function.  What if walk the graph instance instead of the
;; service map?
(println "3) walking the graph instance...")
(walk-leaves-and-path print-path-and-val (graph-instance main-service-map))

;; OUTPUT:
;3) walking the graph instance...
;Config service initializing!
;Path: '[:hello-service :hello]'; value: 'scratch$hello_service$pos_fn__1554__auto____3738$fn__3739@46e9d255'
;Path: '[:config-service :config]'; value: 'scratch$config_service$pos_fn__1554__auto____3723$fn__3724@20efeed2'
;-------------------------------

;; OK, this is a little better.  Now we get the fully nested paths,
;; (e.g. [:hello-service :hello]) and the real service functions as the values.
;; However, note that we already executed the println for the config service
;; initialization... that could be trouble since we're thinking we may be
;; using that for service startup code, and now we've already executed it before
;; we've had a chance to manipulate the graph in any way.
;;
;; Well, we do know that the fnks have an input schema and an output schema.  And
;; theoretically that should be available before we compile the graph; the
;; prismatic library provides some tools for inspecting those.  Let's
;; define another f to use with "walk" that will show us the schemas.  In this case,
;; `v` will be the fnk itself:
(defn print-path-and-schemas
  [p v]
  (println (format "Path: '%s'" p))
  (println "Input schema:")
  (pprint  (pfnk/input-schema v))
  (println "Output schema:")
  (pprint  (pfnk/output-schema v))
  (println))

;; Now let's do a walk with that f:
(println "4) walking the service map for schemas...")
(walk-leaves-and-path print-path-and-schemas main-service-map)

;; OUTPUT:
;4) walking the service map for schemas...
;Path: '[:hello-service]'
;    Input schema:
;{:config-service {:config true}}
;Output schema:
;{:shutdown true, :hello true}
;
;Path: '[:config-service]'
;    Input schema:
;{}
;Output schema:
;{:config true}
;-------------------------------

;; Cool.  So now we can actually see the schemas, but notice that it did NOT
;; print the "Config service initializing!" line, because we haven't compiled
;; the graph yet.  This gives us a way to introspect the graph and do some
;; manipulation prior to compiling it.
;;
;; So what kind of manipulation can we do?  Well, first, we need to think about
;; the return value of our 'walk' function.  Let's examine that, using another
;; simple f:
(defn hello-walk-fn
  [p v]
  (format "Hello '%s'" p))

(println "5) return value of walk function...")
(pprint (walk-leaves-and-path hello-walk-fn main-service-map))

;; OUTPUT:
;5) return value of walk function...
;{:hello-service "Hello '[:hello-service]'",
; :config-service "Hello '[:config-service]'"}
;-------------------------------

;; OK.  So walk is returning us a new map that has the same structure as the
;; original one, but with the leaves replaced by the return value of our f.
;;
;; So... this is where it starts to get interesting... what if our walk function
;; returns a higher-order function?
(defn wrap-with-howdy
  [p v]
  (fn []
    (println (format "Howdy from '%s'" p))
    ;; the original fnk is v, so we can just call it.  More details on this
    ;; signature in a moment.
    (v {})))

;; Let's see what that does:
(println "6) Higher order howdy...")
(pprint (walk-leaves-and-path wrap-with-howdy main-service-map))

;; OUTPUT:
;6) Higher order howdy...
;{:hello-service
;  #<scratch$wrap_with_howdy$fn__5835 scratch$wrap_with_howdy$fn__5835@3d2bde69>,
; :config-service
;  #<scratch$wrap_with_howdy$fn__5835 scratch$wrap_with_howdy$fn__5835@434f9cff>}
;-------------------------------

;; Mmmkay.  We got back a modified service map that we can see involves our
;; higher-order function, but it didn't print anything.  So what happens if
;; we instantiate a graph from that?
(println "7) Compile the howdy...")
;; I'm commenting out this line because it throws an error, see below
;(pprint (graph-instance (walk-leaves-and-path wrap-with-howdy main-service-map)))

;; OUTPUT:
;7) Compile the howdy...
;RuntimeException Missing or malformed io-schemata metadata in null  plumbing.fnk.pfnk/eval1378/fn--1379 (pfnk.clj:21)
;-------------------------------

;; Well, crap.  So what happened here is that we took a valid graph/service map
;; (where all leaf nodes are fnks), and we swapped the leaves out with our higher-order
;; function... which is just a regular fn, and not a fnk.  That makes the graph
;; library sad.
;;
;; We're not actually trying to change the input/output schema of the service, so
;; what we really need to do is just grab those from the fnk, and use them to
;; create a higher-order fnk (instead of just a fn).
;;
;; To make that a little easier, prismatic provides a helper function that can be
;; used to transform a regular fn into a fnk.  This is the signature:
;;    (defn fn->fnk
;;        "Make a keyword function into a PFnk, by associating input and
;;        output schema metadata."
;;        [f [input-schema output-schema :as io]] ...))
;;
;; So you just call this with two args; your fn, and a vector containing the
;; input schema and output schema.  There is one trick, though: any function
;; that you pass to fn->fnk must accept exactly one argument, which is a map.
;; This is how graph injects dependencies to fnks, by passing them in as part
;; of this map argument.  In our case, we just want to pass any injected values
;; directly through to the original fnk.
;;
;; I realize we're getting a little meta here... but thankfully most of this
;; can be generalized away with some utility functions so we won't have to
;; interact with it too frequently.
;;
;; Let's revise our howdy fn to use this:
(defn wrap-with-howdy-fnk
  [p v]
  ;; first we grap the original input/output schemas:
  (let [in (pfnk/input-schema v)
        out (pfnk/output-schema v)
        ;; and we'll assign our original howdy fn to a var for readability:
        f (fn [injected-vals]
            (println (format "Howdy from '%s'" p))
            ;; the original fnk is v, so we can just call it and pass through
            ;; the injected values.
            (v injected-vals))]
    ;; and now we can just transform it to a fnk without modifying the
    ;; schema at all:
    (pfnk/fn->fnk f [in out])))

;; What happens when we compile a graph using that wrapper?
(println "8) Compile the howdy FNK...")
(pprint (graph-instance (walk-leaves-and-path wrap-with-howdy-fnk main-service-map)))

;; OUTPUT:
;8) Compile the howdy FNK...
;Howdy from '[:config-service]'
;Config service initializing!
;Howdy from '[:hello-service]'
;    {:hello-service
;      {:hello
;        #<scratch$hello_service$pos_fn__1554__auto____6734$fn__6735 scratch$hello_service$pos_fn__1554__auto____6734$fn__6735@5f49b5e6>,
;                                                                   :shutdown
;       #<scratch$hello_service$pos_fn__1554__auto____6734$fn__6737 scratch$hello_service$pos_fn__1554__auto____6734$fn__6737@6ad5f9c7>},
;     :config-service
;      {:config
;        #<scratch$config_service$pos_fn__1554__auto____6719$fn__6720 scratch$config_service$pos_fn__1554__auto____6719$fn__6720@6500cd32>}}
;-------------------------------

;; Alright, now we're cooking with... something.  Noteworthy stuff here:
;; * Our new fnk has a side-effect println that gets executed during graph
;;   compile, just like the side-effect println that is directly embedded in
;;   the original config service fnk.
;; * Our wrapper fnk does it's println before we call the original fnk,
;;   and thus we see the printlns executed in that order.
;; * The hello service has a dependency on the config service, and we can see
;;   here that the graph library is making sure that the compilation (and
;;   corresponding side-effects) happen in the correct order.
;;
;; Does all of this have any effect on the final implementation of the
;; actual service functions?
;; Now let's make sure the basic graph does what we expect
(println "9) testing howdy-fnk graph...")
(let [howdy-service-map (walk-leaves-and-path wrap-with-howdy-fnk main-service-map)
      hello-fn (get-in (graph-instance howdy-service-map) [:hello-service :hello])]
  (println (hello-fn)))

;; OUTPUT:
;9) testing howdy-fnk graph...
;Howdy from '[:config-service]'
;Config service initializing!
;Howdy from '[:hello-service]'
;Hello, foo!
;-------------------------------

;; Nope, other than the new side-effect printlns, everything works exactly the
;; same.

;; OK, so now let's say that we want to abstract away some of the implementation
;; details of the pfnk/input-schema/output-schema stuff so that we can
;; accomplish this kind of stuff more easily as we move forward.  The only
;; part of the `wrap-with-howdy-fnk` that is really meaningful to us right now
;; is the simple 'f' that we define in the let block.  We're not doing anything
;; with the schema stuff in this case, so let's assume we can write a higher-order
;; function to abstract away those details.  We'll call that `fn->fnk-with-same-schema`
;; and ignore the implementation details for the moment.
(declare fn->fnk-with-same-schema)

;; So now, assuming we have that available, what would our new version of
;; the howdy wrapper look like?  (Note I've changed the variable name of 'v'
;; to 'orig-fnk' to try to clarify things a bit.
(defn wrap-with-hola-fnk
  [p orig-fnk]
  (let [f (fn [injected-vals]
            (println (format "Hola from '%s'" p))
            (orig-fnk injected-vals))]
    (fn->fnk-with-same-schema f orig-fnk)))

;; A little cleaner.  So let's see if we can write that schema handling function:
(defn fn->fnk-with-same-schema
  [f orig-fnk]
  (let [in  (pfnk/input-schema orig-fnk)
        out (pfnk/output-schema orig-fnk)]
      (pfnk/fn->fnk f [in out])))

;; Word, let's see if we can run it this way.
(println "10) testing hola-fnk graph...")
(let [hola-service-map (walk-leaves-and-path wrap-with-hola-fnk main-service-map)
      hello-fn (get-in (graph-instance hola-service-map) [:hello-service :hello])]
  (println (hello-fn)))

;; OUTPUT:
;10) testing hola-fnk graph...
;Hola from '[:config-service]'
;Config service initializing!
;Hola from '[:hello-service]'
;Hello, foo!
;-------------------------------

;; yep.  so now we could stick that fn->fnk-with-same-schema into a library
;; somewhere and write our wrappers with the simpler syntax.  If we wanted to,
;; we could use a macro to abstract it even further so that we could just write
;; something like:
;;
;;    (defnkwithsameschema wrap-with-hola
;;        [p orig-fnk injected-vals]
;;        (println (format "Hola from '%s'" p))
;;        (orig-fnk injected-vals))
;;
;; OK, so that's all well and good.  But sometimes we actually *do* want to do
;; things with the schema.  For example, if we're trying to determine whether
;; or not the various services provide a :shutdown function, we absolutely
;; need to use the schema to determine that.  Here's a simple wrapper that will
;; print out whether or not each service has a :shutdown function:
(defn wrap-with-schema-check-for-shutdown
  [p orig-fnk]
  (let [out (pfnk/output-schema orig-fnk)]
    (if (contains? out :shutdown)
      (println (format "Service '%s' has a shutdown function!" p)))
    orig-fnk))

(println "11) simple check for shutdown fn...")
(walk-leaves-and-path wrap-with-schema-check-for-shutdown main-service-map)

;; OUTPUT:
;11) simple check for shutdown fn...
;Service '[:hello-service]' has a shutdown function!
;Service '[:config-service]' has a shutdown function!
;-------------------------------

;; Noteworthy there:
;; * We didn't compile the graph, and the printlns were executed... because we
;;   didn't wrap the fnk in any way in this example.
;; * We can detect the presence of the shutdown function via the schema, but
;;   this example code doesn't actually give us access to the actual shutdown
;;   function.
;;
;; To get access to the actual shutdown functiont, we would have to wrap the fnk,
;; and in a way slightly differently from what we've done before.
(defn wrap-with-fnk-check-for-shutdown
  [p orig-fnk]
  (let [in (pfnk/input-schema orig-fnk)
        out (pfnk/output-schema orig-fnk)
        f (fn [injected-vals]
            ;; so what we're going to do here is to call the orig-fnk from within
            ;; a let block.  That gives us access to its service function map
            ;; from within our code, so we can do some things before we actually
            ;; return it.
            (let [result (orig-fnk injected-vals)]
              ;; for now we'll just print it
              (println (format "Service graph for '%s':" p))
              (pprint result)
              (if-let [shutdown-fn (result :shutdown)]
                (println "###Service has a shutdown function!:" shutdown-fn))
              result))]
    ;; and then we still need to flip our f back to a fnk with the original
    ;; schemas.
    (pfnk/fn->fnk f [in out])))

(println "12) check for actual shutdown fn in service map (not just in schema)...")
(let [wrapped-map (walk-leaves-and-path wrap-with-fnk-check-for-shutdown main-service-map)]
  (println "Wrapped the map!")
  (graph-instance wrapped-map))

;; OUTPUT:
;12) check for actual shutdown fn in service map (not just in schema)...
;Wrapped the map!
;Config service initializing!
;Service graph for '[:config-service]':
;{:config #<scratch$config_service$pos_fn__1721__auto____4905$fn__4906 scratch$config_service$pos_fn__1721__auto____4905$fn__4906@499375c2>,
; :shutdown #<scratch$config_service$pos_fn__1721__auto____4905$fn__4908 scratch$config_service$pos_fn__1721__auto____4905$fn__4908@523ed23c>}
;###Service has a shutdown function!: #<scratch$config_service$pos_fn__1721__auto____4905$fn__4908 scratch$config_service$pos_fn__1721__auto____4905$fn__4908@523ed23c>
;Service graph for '[:hello-service]':
;{:hello #<scratch$hello_service$pos_fn__1721__auto____4922$fn__4923 scratch$hello_service$pos_fn__1721__auto____4922$fn__4923@3f3a1398>,
; :shutdown #<scratch$hello_service$pos_fn__1721__auto____4922$fn__4925 scratch$hello_service$pos_fn__1721__auto____4922$fn__4925@4d04fff>}
;###Service has a shutdown function!: #<scratch$hello_service$pos_fn__1721__auto____4922$fn__4925 scratch$hello_service$pos_fn__1721__auto____4922$fn__4925@4d04fff>
;-------------------------------

;; So, you can see a few things from that output:
;; * The 'Wrapped the map!' println executes first; because our other prints are
;;   inside of our wrapped fnk, they don't execute until the graph is compiled.
;; * They have full access to the entire map of service functions for the service.
;;   This allows us to do things like store a reference to the shutdown function,
;;   or we could even wrap those inner functions with some other logic if we
;;   needed to.
;;
;; This file is a lot longer than I intended, so I'll just show one more example;
;; In it, I'll create an atom to use to save references to all of the shutdown
;; functions, and then call them (in the correct order) after compiling the
;; graph.
(defn wrap-with-shutdown-registration
  [shutdown-fns-atom p orig-fnk]
  (let [in (pfnk/input-schema orig-fnk)
        out (pfnk/output-schema orig-fnk)
        f (fn [injected-vals]
            (let [result (orig-fnk injected-vals)]
              (when-let [shutdown-fn (result :shutdown)]
                (println (format "Registering shutdown function for service '%s'" p))
                (swap! shutdown-fns-atom conj shutdown-fn))
              result))]
    (pfnk/fn->fnk f [in out])))

(let [shutdown-fns (atom ())
      wrapped-map (walk-leaves-and-path
                    (partial wrap-with-shutdown-registration shutdown-fns)
                    main-service-map)
      g-i         (graph-instance wrapped-map)]
  (doseq [shutdown-fn @shutdown-fns]
    (shutdown-fn)))





