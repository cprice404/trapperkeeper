(ns puppetlabs.trapperkeeper.experimental.services-internal
  (:require [clojure.walk :refer [postwalk]]
            [puppetlabs.kitchensink.core :refer [keyset]]))

(defn protocol?
  "A predicate to determine whether or not an object is a protocol definition"
  [p]
  ;; there might be a better way to do this, but it seems to work
  (and (map? p)
       (contains? p :on)
       (instance? Class (resolve (:on p)))))

(defn add-default-lifecycle-fn
  "TODO docs"
  [fns-map fn-name]
  (if (contains? fns-map (keyword fn-name))
    fns-map
    (assoc fns-map (keyword fn-name)
      (cons fn-name '([this context] context)))))

(defn add-default-lifecycle-fns
  "TODO docs"
  [lifecycle-fn-names fns-map]
  (reduce add-default-lifecycle-fn fns-map lifecycle-fn-names))

(defn fnk-binding-form
  "Given a fnk dependencies binding form vector and a list of functions that
  a service provides, return the binding form annotated with metadata containing
  an explicit output schema for the fnk."
  [depends provides]
  {:pre [(vector? depends)
         (coll? provides)
         (every? symbol? provides)]
   :post [(vector? %)]}
  (let [to-output-schema  (fn [provides]
                            (reduce (fn [m p] (assoc m (keyword p) true))
                                    {}
                                    provides))
        output-schema     (to-output-schema provides)]
    ;; Add an output-schema entry to the depends vector's metadata map
    (vary-meta depends assoc :output-schema output-schema)))

(defn postwalk*
  "Convenience wrapper for clojure.walk/postwalk.  Given two functions `matches?`
  and `replace` walks form `form`.  For each node, if `matches?` returns true,
  then replaces the node with the result of `replace`."
  [matches? replace form]
  {:pre [(ifn? matches?)
         (ifn? replace)]}
  (postwalk
    (fn [x]
      (if-not (matches? x)
        x
        (replace x)))
    form))

(defn is-protocol-fn-call?
  "Given a set of function names, a symbol representing the 'this' object of
  a protocol function signature, and a form: return true if the form represents
  a call to one of the protocol functions.  (This is for use in macros that are
  transforming protocol function definition code.)"
  [fns this form]
  {:pre [(set? fns)
         (every? symbol? fns)
         (symbol? this)]}
  (and (seq? form)
       (> (count form) 1)
       (= this (second form))
       (contains? fns (first form))))

(defn replace-fn-calls
  "Given a set of function names, a symbol representing the 'this' object of
  a protocol function signature, and a form: find all of the calls to the protocol
  functions anywhere in the form and replace them with calls to the prismatic
  graph functions.  (This is for use in macros that are transforming protocol
  function definition code.)

  Returns a tuple whose first element is a set containing the names of all of
  the functions that were found in the form, and whose second element is
  the modified form."
  [fns this form]
  {:pre [(set? fns)
         (every? symbol? fns)
         (symbol? this)]
   :post [(vector? %)
          (set? (first %))
          (every? symbol? (first %))]}
  ;; in practice, all our 'replace' function really needs to do is to
  ;; remove the 'this' argument.  The function signatures in the graph are
  ;; identical to the ones in the protocol, except without the 'this' arg.
  (let [replace     (fn [form] (cons (first form) (nthrest form 2)))
        ;; we need an atom to accumulate the matches that we find, because
        ;; clojure.walk doesn't provide any signatures that support an accumulator.
        ;; could eventually look into replacing this with an implementation based
        ;; off of a zipper, but that looked like a lot of work for now.
        acc         (atom #{})
        accumulate  (fn [form] (swap! acc conj (first form)))
        result      (postwalk*
                      (partial is-protocol-fn-call? fns this)
                      (fn [form] (accumulate form) (replace form))
                      form)]
    [@acc result]))

(defn protocol-fns->prismatic-fns
  "Given a list of protocol function definition forms, transform them into a format
  that can be used to construct the equivalent fnks for a prismatic graph.  (This
  is for use in macros that are transforming protocol function definition code.)
  For each function, return a map containing:

  :name - the function name
  :deps - a list of function names that the function has a dependency on
  :args - the arguments list for the prismatic function
  :body - the function body

  The only major change involved in the transformation is removing the implicit
  'this' arguments from the protocol functions, because they are not necessary or
  meaningful in the graph fnks."
  [fns service-fn-names]
  {:pre [(coll? fns)
         (every? seq? fns)
         (coll? service-fn-names)
         (every? symbol? service-fn-names)]
   :post [(vector? %)
          (every? map? %)
          (every? (fn [m] (= #{:name :deps :args :body} (keyset m))) %)]}
  (mapv
    (fn [f]
      ;; first we destructure the function form into its various parts
      (let [[fn-name [this & fn-args] & fn-body] f
            ;; now we need to transform all calls to `service-context` from
            ;; protocol form to prismatic form.  we don't need to track this as
            ;; a dependency because it will be provided by the app.
            [_ fn-body]     (replace-fn-calls #{'service-context} this fn-body)
            ;; transform all the functions from the service protocol, and keep
            ;; a list of the dependencies so that prismatic can inject them
            [deps fn-body]  (replace-fn-calls (set service-fn-names) this fn-body)]
        {:name (keyword fn-name)
         :deps deps
         :args fn-args
         :body fn-body}))
    fns))

