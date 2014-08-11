 (ns scratch3
   (:require [plumbing.graph :as graph]
             [plumbing.core :as plumbing]
             [schema.core :as schema]))


(def m (graph/graph
         :foo-service (plumbing/fnk [] {:foo (fn [] "foo")})
         :bar-service (plumbing/fnk [context]
                                    {:bar (fn [] (str @context "bar"))})))

(def g (graph/eager-compile m))

(def graph-instance (g {} #_{:context (atom "hi")}))

(let [bar (get-in graph-instance [:bar-service :bar])]
  (println (bar)))
