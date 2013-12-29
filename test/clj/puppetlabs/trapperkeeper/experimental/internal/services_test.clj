;; This namespace contains tests for the experimental service API
;; that are specific to the prismatic implementation

(ns puppetlabs.trapperkeeper.experimental.internal.services-test
  (:require [clojure.test :refer :all]
            [plumbing.fnk.pfnk :as pfnk]
            [puppetlabs.trapperkeeper.experimental.services :refer [service]]))

(defprotocol Service1
  (service1-fn [this]))

(defprotocol Service2
  (service2-fn [this]))

(deftest prismatic-functionality-test
  (testing "prismatic fnk is initialized properly"
    (let [service1  (service Service1
                       []
                       (init [this context] context)
                       (startup [this context] context)
                       (service1-fn [this] "Foo!"))
          service2  (service Service2
                       [[:Service1 service1-fn]]
                       (init [this context] context)
                       (startup [this context] context)
                       (service2-fn [this] "Bar!"))
          s1-graph  (:service-map service1)
          s2-graph  (:service-map service2)]
      (is (map? s1-graph))
      (let [graph-keys (keys s1-graph)]
        (is (= (count graph-keys) 1))
        (is (= (first graph-keys) :Service1)))

      (let [service-fnk  (:Service1 s1-graph)
            depends      (pfnk/input-schema service-fnk)
            provides     (pfnk/output-schema service-fnk)]
        (is (ifn? service-fnk))
        (is (= depends  {:context true}))
        (is (= provides {:service1-fn true})))

      (is (map? s2-graph))
      (let [graph-keys (keys s2-graph)]
        (is (= (count graph-keys) 1))
        (is (= (first graph-keys) :Service2)))

      (let [service-fnk  (:Service2 s2-graph)
            depends      (pfnk/input-schema service-fnk)
            provides     (pfnk/output-schema service-fnk)
            fnk-instance (service-fnk {:Service1 {:service1-fn identity}
                                       :context (atom {})})
            s2-fn        (:service2-fn fnk-instance)]
        (is (ifn? service-fnk))
        (is (= depends  {:Service1 {:service1-fn true}
                         :context true}))
        (is (= provides {:service2-fn true}))
        (is (= "Bar!" (s2-fn)))))))