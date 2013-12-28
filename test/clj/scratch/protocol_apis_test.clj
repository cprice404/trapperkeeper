(ns scratch.protocol-apis-test
  (:require [clojure.test :refer :all]
            [plumbing.fnk.pfnk :as pfnk]
            [scratch.protocol-apis :refer [ServiceLifecycle PrismaticGraphService defservice service service-graph boot!]]))

(defprotocol HelloService
  (hello [this msg]))

(defservice hello-service
  HelloService
  [[:foo-service foo]
   [:bar-service bar]]
  (init [this context]
    (println "INIT!")
    context)
  (startup [this context]
    (println "STARTUP!")
    context)
  (hello [this msg]
    (str "HELLO!: " msg)))

(deftest test-satisfies-protocols
  (testing "satisfies all protocols"
    (is (satisfies? ServiceLifecycle hello-service))
    (is (satisfies? PrismaticGraphService hello-service))
    (is (satisfies? HelloService hello-service)))

  (testing "service functions behave as expected"
    (is (= "HELLO!: yo" (hello hello-service "yo"))))

  (testing "prismatic fnk is initialized properly"
    (let [service-graph (service-graph hello-service)]
      (is (map? service-graph))

      (let [graph-keys (keys service-graph)]
        (is (= (count graph-keys) 1))
        (is (= (first graph-keys) :HelloService)))

      (let [service-fnk  (:HelloService service-graph)
            depends      (pfnk/input-schema service-fnk)
            provides     (pfnk/output-schema service-fnk)]
        (is (ifn? service-fnk))
        (is (= depends  {:foo-service {:foo true} :bar-service {:bar true}}))
        (is (= provides {:hello true}))

        (let [fnk-instance  (service-fnk {:foo-service {:foo identity}
                                          :bar-service {:bar identity}})
               hello-fn     (:hello fnk-instance)]
          (is (= "HELLO!: hi" (hello-fn "hi"))))))))

(defprotocol Service1
  (service1-fn [this]))

(defprotocol Service2
  (service2-fn [this]))

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
                      (service2-fn [this] (swap! call-seq conj :service2-fn)))]
      (is (satisfies? PrismaticGraphService service1))
      (is (satisfies? PrismaticGraphService service2))
      (boot! [service1 service2])
      (is (= [:init-service1 :init-service2 :startup-service1 :startup-service2]
             @call-seq)))))
