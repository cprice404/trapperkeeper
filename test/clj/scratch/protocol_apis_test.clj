(ns scratch.protocol-apis-test
  (:require [clojure.test :refer :all]
            [plumbing.fnk.pfnk :as pfnk]
            [scratch.protocol-apis :refer [ServiceLifecycle PrismaticGraphService prot-service service-graph]]))

(defprotocol HelloService
  (hello [this msg]))

(defn instantiate-service
  []
  (prot-service HelloService
    [[:foo-service foo]
     [:bar-service bar]]
    (init [this context]
      (println "INIT!")
      context)
    (startup [this context]
      (println "STARTUP!")
      context)
    (hello [this msg]
      (str "HELLO!: " msg))))

(deftest test-satisfies-protocols
  (let [hello-service (instantiate-service)]
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
            (is (= "HELLO!: hi" (hello-fn "hi")))))))))
