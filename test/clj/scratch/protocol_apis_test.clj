(ns scratch.protocol-apis-test
  (:require [clojure.test :refer :all]
            [scratch.protocol-apis :refer [ServiceLifecycle PrismaticGraphService prot-service]]))

(defprotocol MyService
  (foo [this msg]))

(defn instantiate-service
  []
  (prot-service MyService
    (init [this context]
      (println "INIT!")
      context)
    (startup [this context]
      (println "STARTUP!")
      context)
    (foo [this msg]
      (str "FOO!: " msg))))

(deftest test-satisfies-protocols
  (testing "satisfies both protocols"
    (let [myservice (instantiate-service)]
      (is (satisfies? ServiceLifecycle myservice))
      (is (satisfies? PrismaticGraphService myservice))
      (is (satisfies? MyService myservice))
      (is (= "FOO!: foo" (foo myservice "foo"))))))
