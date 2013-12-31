(ns puppetlabs.trapperkeeper.experimental.services-test
  (:require [clojure.test :refer :all]
            [plumbing.fnk.pfnk :as pfnk]
            [puppetlabs.trapperkeeper.experimental.services :refer [App ServiceLifecycle defservice service boot! get-service]])
  (:import [puppetlabs.trapperkeeper.experimental.services ServiceDefinition]))

(defprotocol HelloService
  (hello [this msg]))

(defservice hello-service
  HelloService
  []
  (init [this context] context)
  (startup [this context] context)
  (hello [this msg] (str "HELLO!: " msg)))

(deftest test-satisfies-protocols
  (testing "creates a record"
    (is (instance? ServiceDefinition hello-service)))

  (let [app (boot! [hello-service])]
    (println "APP:" app)
    (testing "app satisfies protocol"
      (is (satisfies? App app)))

    (let [h-s (get-service app :HelloService)]
      (testing "service satisfise all protocols"
        (is (satisfies? ServiceLifecycle h-s))
        ;(is (satisfies? PrismaticGraphService h-s))
        (is (not true)) ;; ^^ revisit and see if there's another way we should be testing something like this
        (is (satisfies? HelloService h-s)))

      (testing "service functions behave as expected"
        (is (= "HELLO!: yo" (hello h-s "yo"))))
      )))

(defprotocol Service1
  (service1-fn [this]))

(defprotocol Service2
  (service2-fn [this]))

(defprotocol Service3
  (service3-fn [this]))

;; TODO: might be nice to move all of the prismatic-specific tests
;; into a separate namespace, and only keep api-level tests here.
(deftest prismatic-functionality-test
  (testing "prismatic fnk is initialized properly"
    ;(let [service1  (service Service1
    ;                   []
    ;                   (init [this context] context)
    ;                   (startup [this context] context)
    ;                   (service1-fn [this] "Foo!"))
    ;      service2  (service Service2
    ;                   [[:Service1 service1-fn]]
    ;                   (init [this context] context)
    ;                   (startup [this context] context)
    ;                   (service2-fn [this] "Bar!"))
    ;      app       (boot! [service1 service2])
    ;      s1-graph  (service-graph (get-service app :Service1))
    ;      s2-graph  (service-graph (get-service app :Service2))]
    ;  (is (map? s1-graph))
    ;  (let [graph-keys (keys s1-graph)]
    ;    (is (= (count graph-keys) 1))
    ;    (is (= (first graph-keys) :Service1)))
    ;
    ;  (let [service-fnk  (:Service1 s1-graph)
    ;        depends      (pfnk/input-schema service-fnk)
    ;        provides     (pfnk/output-schema service-fnk)]
    ;    (is (ifn? service-fnk))
    ;    (is (= depends  {}))
    ;    (is (= provides {:service1-fn true})))
    ;
    ;  (is (map? s2-graph))
    ;  (let [graph-keys (keys s2-graph)]
    ;    (is (= (count graph-keys) 1))
    ;    (is (= (first graph-keys) :Service2)))
    ;
    ;  (let [service-fnk  (:Service2 s2-graph)
    ;        depends      (pfnk/input-schema service-fnk)
    ;        provides     (pfnk/output-schema service-fnk)
    ;        fnk-instance (service-fnk {:Service1 {:service1-fn identity}})
    ;        s2-fn        (:service2-fn fnk-instance)]
    ;    (is (ifn? service-fnk))
    ;    (is (= depends  {:Service1 {:service1-fn true}}))
    ;    (is (= provides {:service2-fn true}))
    ;    (is (= "Bar!" (s2-fn)))))))
    (is (not true))
    ))

(deftest lifecycle-test
  (testing "life cycle functions are called in the correct order"
    (let [call-seq  (atom [])
          service1  (service Service1
                      []
                      (init [this context] (swap! call-seq conj :init-service1))
                      (startup [this context] (swap! call-seq conj :startup-service1))
                      (service1-fn [this] (swap! call-seq conj :service1-fn)))
          service2  (service Service2
                      [[:Service1 service1-fn]]
                      (init [this context] (swap! call-seq conj :init-service2))
                      (startup [this context] (swap! call-seq conj :startup-service2))
                      (service2-fn [this] (swap! call-seq conj :service2-fn)))
          service3  (service Service3
                       [[:Service2 service2-fn]]
                       (init [this context] (swap! call-seq conj :init-service3))
                       (startup [this context] (swap! call-seq conj :startup-service3))
                       (service3-fn [this] (swap! call-seq conj :service3-fn)))]
      (boot! [service1 service3 service2])
      (is (= [:init-service1 :init-service2 :init-service3
              :startup-service1 :startup-service2 :startup-service3]
             @call-seq)))))

(deftest dependencies-test
  (testing "services should be able to call functions in dependency list"
    (let [service1 (service Service1
                            []
                            (init [this context] context)
                            (startup [this context] context)
                            (service1-fn [this] "FOO!"))
          service2 (service Service2
                            [[:Service1 service1-fn]]
                            (init [this context] context)
                            (startup [this context] context)
                            (service2-fn [this] (str "HELLO " (service1-fn))))
          ;(service2-fn [this] (str "HELLO")))
          app (boot! [service1 service2])
          s2 (get-service app :Service2)]
      (is (= "HELLO FOO!" (service2-fn s2))))))

(defprotocol Service4
  (service4-fn1 [this])
  (service4-fn2 [this]))

;(deftest service-this-test
;  (testing "should be able to call other functions in same service via 'this'"
;    (let [service4  (service Service4
;                      []
;                      (service4-fn1 [this] "foo!")
;                      (service4-fn2 [this] (str (service4-fn1 this) " bar!")))
;          app       (boot! [service4])
;          s4        (get-service app :Service4)]
;      (is (= "foo! bar!" (service4-fn2 s4))))))

(deftest context-test
  (testing "should error if lifecycle function doesn't return context"
    (is (not true)))
  (testing "context should be available in subsequent lifecycle functions"
    (is (not true)))
  (testing "context should be accessible in service functions"
    (is (not true))))
