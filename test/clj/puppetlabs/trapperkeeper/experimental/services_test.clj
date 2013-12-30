(ns puppetlabs.trapperkeeper.experimental.services-test
  (:require [clojure.test :refer :all]
            [plumbing.fnk.pfnk :as pfnk]
            [puppetlabs.trapperkeeper.experimental.services :refer [App ServiceLifecycle PrismaticGraphService defservice service service-graph boot! get-service]])
  (:import [puppetlabs.trapperkeeper.experimental.services ServiceDefinition]))

(defprotocol HelloService
  (hello [this msg]))

(defservice hello-service
  HelloService
  ;[[:foo-service foo]
  ; [:bar-service bar]]
  []
  (init [this context]
    (println "INIT!")
    context)
  (startup [this context]
    (println "STARTUP!")
    context)
  (hello [this msg]
    (str "HELLO!: " msg)))

(deftest test-satisfies-protocols
  (testing "creates a record"
    (is (instance? ServiceDefinition hello-service)))

  (let [app (boot! [hello-service])]
    (println "APP:" app)
    (testing "app satisfies protocol"
      (is (satisfies? App app)))

    (let [h-s (get-service app HelloService)]
      (testing "service satisfise all protocols"
        (is (satisfies? ServiceLifecycle h-s))
        (is (satisfies? PrismaticGraphService h-s))
        (is (satisfies? HelloService h-s)))

      (testing "service functions behave as expected"
        (is (= "HELLO!: yo" (hello h-s "yo"))))

      ;(testing "prismatic fnk is initialized properly"
      ;  (let [service-graph (service-graph h-s)]
      ;    (is (map? service-graph))
      ;
      ;    (let [graph-keys (keys service-graph)]
      ;      (is (= (count graph-keys) 1))
      ;      (is (= (first graph-keys) :HelloService)))
      ;
      ;    (let [service-fnk  (:HelloService service-graph)
      ;          depends      (pfnk/input-schema service-fnk)
      ;          provides     (pfnk/output-schema service-fnk)]
      ;      (is (ifn? service-fnk))
      ;      (is (= depends  {:foo-service {:foo true} :bar-service {:bar true}}))
      ;      (is (= provides {:hello true}))
      ;
      ;      (let [fnk-instance  (service-fnk {:foo-service {:foo identity}
      ;                                        :bar-service {:bar identity}})
      ;            hello-fn     (:hello fnk-instance)]
      ;        (is (= "HELLO!: hi" (hello-fn "hi")))))))
      )))

(defprotocol Service1
  (service1-fn [this]))

(defprotocol Service2
  (service2-fn [this]))

(defprotocol Service3
  (service3-fn [this]))

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
    (let [service1  (service Service1
                      []
                      (init [this context] context)
                      (startup [this context] context)
                      (service1-fn [this] "FOO!"))
          service2  (service Service2
                      [[:Service1 service1-fn]]
                      (init [this context] context)
                      (startup [this context] context)
                      ;(service2-fn [this] (str "HELLO " (service1-fn))))
                      (service2-fn [this] (str "HELLO")))
          _           (println "SERVICES:" [service1 service2])
          app         (boot! [service1 service2])
          s2          (get-service app Service2)]
      (is (= "HELLO FOO!" (service2-fn s2))))))

(deftest context-test
  (testing "should error if lifecycle function doesn't return context"
    (is false))
  (testing "context should be available in subsequent lifecycle functions"
    (is false))
  (testing "context should be accessible in service functions"
    (is false)))
