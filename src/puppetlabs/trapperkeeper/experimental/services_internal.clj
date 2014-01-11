(ns puppetlabs.trapperkeeper.experimental.services-internal
  (:require [clojure.walk :refer [postwalk]]
            [clojure.set :refer [difference]]
            [plumbing.core :refer [fnk]]
            [puppetlabs.kitchensink.core :refer [keyset]]))

(defn validate-fn-forms!
  ;; TODO: docs
  [protocol-sym deps fns]
  (if (every? seq? fns)
    [protocol-sym deps fns]
    (throw (IllegalArgumentException.
             (format
               "Invalid service definition; expected function definitions following dependency list, invalid value: '\"hi\"'"
               (pr-str (first (filter #(not (seq? %)) fns))))))))

(defn find-deps-form!
  ;; TODO: docs
  [protocol-sym forms]
  (let [f (first forms)]
    (if (vector? f)
      (validate-fn-forms! protocol-sym f (rest forms))
      (throw (IllegalArgumentException.
               (format
                 "Invalid service definition; expected dependency list following protocol, found: '%s'"
                 (pr-str f)))))))

(defn find-prot-and-deps-forms!
  ;; TODO: docs
  [forms]
  (let [f (first forms)]
    (cond
      (symbol? f) (find-deps-form! f (rest forms))
      (vector? f) (find-deps-form! nil forms)
      :else (throw (IllegalArgumentException.
                     (format
                       "Invalid service definition; first form must be protocol or dependency list; found '%s'"
                       (pr-str f)))))))

(defn protocol?
  "A predicate to determine whether or not an object is a protocol definition"
  [p]
  ;; there might be a better way to do this, but it seems to work
  (and (map? p)
       (contains? p :on)
       (instance? Class (resolve (:on p)))))

(defn validate-protocol-sym!
  ;; TODO docs
  [sym var]
  (if-not var
    (throw (IllegalArgumentException.
             (format "Unrecognized service protocol '%s'" sym))))
  (let [protocol (var-get var)]
    (if-not (protocol? protocol)
      (throw (IllegalArgumentException.
               (format "Specified service protocol '%s' does not appear to be a protocol!"
                       sym))))
    protocol))

(defn validate-provided-fns!
  ;; TODO docs
  [service-protocol-sym service-fns provided-fns]
  (if (and (nil? service-protocol-sym)
           (> (count provided-fns) 0))
    (throw (IllegalArgumentException.
             (format
               "Service attempts to define function '%s', but does not provide protocol"
               (name (first provided-fns))))))
  (let [extras (difference provided-fns service-fns)]
    (when-not (empty? extras)
      (throw (IllegalArgumentException.
               (format
                 "Service attempts to define functions '%s' which do not exist in protocol '%s'"
                 extras service-protocol-sym))))))

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

(defn build-fns-map!
  ;; TODO docs
  [service-protocol-sym service-fn-names lifecycle-fn-names fns]
  (let [fns-map (->> (reduce
                       (fn [acc f] (assoc acc (keyword (first f)) f))
                       {}
                       fns)
                  (add-default-lifecycle-fns lifecycle-fn-names))]
    (validate-provided-fns!
      service-protocol-sym
      (map keyword service-fn-names)
      (difference (keyset fns-map)
                  (map keyword lifecycle-fn-names)))
    fns-map))

(defn get-service-id
  ;; TODO docs
  [service-protocol-sym]
  (if service-protocol-sym
    (keyword service-protocol-sym)
    (keyword (gensym "tk-service"))))

(defn get-service-fn-names
  ;; TODO DOCS
  [service-protocol-sym]
  (let [service-protocol  (if service-protocol-sym
                            (validate-protocol-sym!
                              service-protocol-sym
                              (resolve service-protocol-sym)))]
    (if service-protocol
      (map :name (vals (:sigs service-protocol)))
      [])))

(defn check-for-required-fns!
  ;; TODO Docs preconds
  [fns-map required-fn-names protocol-name]
  (doseq [fn-name required-fn-names]
    (if-not (contains? fns-map (keyword fn-name))
      (throw (IllegalArgumentException.
               (format "Service does not implement required function '%s'" fn-name))))))

(defn parse-service-forms!
  ;; TODO DOCS
  [lifecycle-protocol-name lifecycle-fn-names forms]
  (let [[service-protocol-sym dependencies fns]
                          (find-prot-and-deps-forms! forms)
        service-id        (get-service-id service-protocol-sym)
        service-fn-names  (get-service-fn-names service-protocol-sym)

        fns-map           (build-fns-map!
                            service-protocol-sym
                            service-fn-names
                            lifecycle-fn-names
                            fns)]

    ;;; TODO: verify that the service protocol doesn't define any functions
    ;;;  whose names collide with our built-ins.

    (check-for-required-fns! fns-map service-fn-names (name service-id))
    (check-for-required-fns! fns-map lifecycle-fn-names lifecycle-protocol-name)
    {:service-protocol-sym  service-protocol-sym
     :service-id            service-id
     :service-fn-names      service-fn-names
     :dependencies          dependencies
     :fns-map               fns-map}))


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
  (postwalk (fn [x]
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

(defn protocol-fn->graph-fn
  ;; TODO docs
  [fn-names f]
  ;; first we destructure the function form into its various parts
  (let [[_ [this & fn-args] & fn-body] f
        ;; now we need to transform all calls to `service-context` from
        ;; protocol form to prismatic form.  we don't need to track this as
        ;; a dependency because it will be provided by the app.
        [_ fn-body] (replace-fn-calls #{'service-context} this fn-body)
        ;; transform all the functions from the service protocol, and keep
        ;; a list of the dependencies so that prismatic can inject them
        [deps fn-body] (replace-fn-calls (set fn-names) this fn-body)]
    {:deps deps
     :f    (concat (list 'fn (vec fn-args)) fn-body)}))

(defn add-prismatic-service-fnk
  ;; TODO: docs
  [fn-names fns-map fnk-acc fn-name]
  (let [{:keys [deps f]} (protocol-fn->graph-fn
                           fn-names
                           (fns-map fn-name))]
    (assoc fnk-acc fn-name
      (list 'plumbing.core/fnk (vec deps) f))))

(defn prismatic-service-map
  ;; TODO DOCS
  [fn-names fns-map]
  (reduce
    (partial add-prismatic-service-fnk fn-names fns-map)
    {}
    (map keyword fn-names)))

(defn protocol-fns
  ;; TODO DOCS
  [fn-names fns-map]
  (for [fn-name fn-names]
    (let [[_ fn-args & _] (fns-map (keyword fn-name))]
      (list fn-name fn-args
        (cons
          (list 'service-fns (keyword fn-name))
          (rest fn-args))))))

