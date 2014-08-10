(ns scratch
  (:import (clojure.lang IFn))
  (:require [plumbing.graph :as graph]
            [plumbing.core :as plumbing]
            [schema.core :as schema]))

;(def m (graph/graph
;         :foo-service (plumbing/fnk [] {:foo (fn [] "foo")})
;         :bar-service (plumbing/fnk [[:foo-service foo]]
;                                    {:bar (fn [] (str (foo) "bar"))})))
;
;(def g (graph/eager-compile m))
;
;(def graph-instance (g {}))
;
;(let [bar (get-in graph-instance [:bar-service :bar])]
;  (println (bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defprotocol FooService
;  (foo [this]))
;
;(defprotocol BarService
;  (bar [this]))
;
;(def foo-service
;  (reify
;    FooService
;    (foo [this] "foo")))
;
;(def bar-service
;  (reify
;    BarService
;    (bar [this] "bar")))
;
;(def m (graph/graph
;         :foo-service (plumbing/fnk []
;                                    {:foo (partial foo foo-service)})
;         :bar-service (plumbing/fnk []
;                                    {:bar (partial bar bar-service)})))
;
;(def g (graph/eager-compile m))
;
;(def graph-instance (g {}))
;
;(let [bar (get-in graph-instance [:bar-service :bar])]
;  (println (bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(schema/set-fn-validation! true)

(defprotocol FooService
  (foo [this]))

(defprotocol BarService
  (bar [this]))

(def foo-service
  (plumbing/fnk foofnk :- {:foo IFn}
                []
    (let [svc (reify FooService
                (foo [this] "foo"))]
      {:foo (partial foo svc)})))

(def bar-service
  (plumbing/fnk barfnk :- {:bar IFn}
                [[:foo-service foo]]
    (let [svc (reify BarService
                (bar [this] (str (foo) "bar")))]
      {:bar (partial bar svc)})))

(def m (graph/graph
         :foo-service foo-service
         :bar-service bar-service))

(def g (graph/eager-compile m))

(def graph-instance (g {}))

(let [bar (get-in graph-instance [:bar-service :bar])]
  (println (bar)))



