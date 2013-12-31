(ns puppetlabs.trapperkeeper.experimental.scratch
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as g]
            [clojure.walk :refer [postwalk]]
            [clojure.test :refer :all]))
;
;(defn foo
;  [pattern x]
;  (let [partfoo (partial foo pattern)]
;    (if-not (seq? x)
;      x
;      (let [patsize (count pattern)]
;        (if (and (>= (count x) patsize)
;                 (= (take patsize x) pattern))
;          (map partfoo (cons :match (nthrest x patsize)))
;          (map partfoo x))))))
;
;(def x
;  {:fn1 (fnk [] (fn [] "foo"))
;   :fn2 (fnk [fn1] (fn [] (str "hello " (fn1))))})
;
;(def g-i
;  (-> x
;      (g/eager-compile)
;      ((fn [cg] (cg {})))))
;
;(println "graph instance: " g-i)
;(println ((:fn2 g-i)))
;
;(def service-impl
;  {:fn1 '(fn1 [this] "foo")
;   :fn2 '(fn2 [this] (str "hello " (fn1 this)))})
;
;(defmacro build-service-fnk-map
;  [service-spec]
;  (println "SERVICE SPEC" service-spec)
;  `~(reduce ;(fn [acc i] (cons i acc)) [] [:a :b]
;      (fn [acc [fn-kw fn-form]]
;        acc)
;      {}
;      ~service-spec))
      ;(fn [acc [fn-kw fn-form]]
      ;  (let [[fn-name fn-args & fn-body] fn-form]
      ;    (println "fn-kw" fn-kw)
      ;    (println "fn-form" fn-form)
      ;    (prn "fn-body" fn-body)
      ;    (assoc acc fn-kw `(fnk [] ~@fn-body))))
      ;{}
      ;service-spec))
  ;`{
  ;   ~@()
  ; }
   ;(reduce
   ;  (fn [acc# [fn-kw# fn-form#]]
   ;    (let [[fn-name# fn-args# & fn-body#] fn-form#]
   ;      (println "fn-kw" fn-kw#)
   ;      (println "fn-form" fn-form#)
   ;      (prn "fn-body" fn-body#)
   ;      (assoc acc# fn-kw# (fnk [] (fn [] "hi")))))
   ;  {}
   ;  ~service-spec))

;(defmacro build-service-fnk
;  [fn-kw fn-form]
;  (println "fn-kw" fn-kw)
;  (println "fn-form" fn-form)
;  `(let [[fn-name fn-args & fn-body] ~fn-form]
;      (fnk [] ~@fn-body)))

;(defn build-service-graph
;  [service-spec]
;  (let [service-fnk-map (reduce
;                          (fn [acc [fn-kw fn-form]]
;                            (let [[fn-name fn-args & fn-body] fn-form]
;                              (assoc acc fn-kw (eval (apply list 'fnk (vec (cons 'f1 fn-args)) (quote fn-body))))))
;                          {}
;                          service-spec)]
;    ((g/eager-compile service-fnk-map) {})))
;
;(let [s (build-service-graph service-impl)]
;  (println "calling fn2:" ((:fn2 s))))
;
;(deftest service-impl-test
;  (let [s (build-service-graph service-impl)]
;    (is (= "hello foo" ((:fn2 s))))))

  ;
  ;(let [partfoo (partial replace-pattern pattern)]
  ;  (if-not (seq? x)
  ;    [acc x]
  ;    (let [patsize (count pattern)]
  ;      (if (and (>= (count x) patsize)
  ;               (= (take patsize x) pattern))
  ;        (map (partial partfoo (conj acc (take patsize x)))
  ;             (cons :match (nthrest x patsize)))
  ;        (map partfoo x))))))

;(defn find-deps
;  [fn-body]
;  (prn "Finding deps for:" fn-body)
;  [[] fn-body])



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


(defmacro foomacro
  [& fns]
  ;(reduce
  ;  (fn [acc f]
  ;    (let [[fn-name fn-args & fn-body] f]
  ;      (println "F:" f)
  ;      (prn "f-body:" fn-body)
  ;      (assoc acc (keyword fn-name) "hi")))
  ;  {}
  ;  fns)
  ;(let [x [[:foo :bar]]]
    `(into {}
           ~(mapv
              (fn [f]
                (let [[fn-name fn-args & fn-body] f
                      [deps fn-body] (replace-fn-calls #{'fn1} 'this fn-body)]
                  ;(println "F:" f)
                  ;(println "fn-name:" fn-name)
                  ;(println "fn-args:" fn-args)
                  ;(println "fn-body:" fn-body)
                  [(keyword fn-name) `(fnk [~@deps] (fn [~@(rest fn-args)] ~@fn-body))]))
              fns)))

(foomacro
  (fn1 [this] "foo")
  (fn2 [this] (str "hello " (fn1 this)))
  ;(fn2 [this] (str "hello " "there"))
  )