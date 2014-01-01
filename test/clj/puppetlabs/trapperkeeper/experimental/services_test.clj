(ns puppetlabs.trapperkeeper.experimental.services-test
  (:require [clojure.test :refer :all]
            [puppetlabs.trapperkeeper.experimental.services :refer [App Service ServiceLifecycle defservice service boot! get-service service-context]])
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
        (is (satisfies? Service h-s))
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

(deftest lifecycle-test
  (testing "life cycle functions are called in the correct order"
    (let [call-seq  (atom [])
          lc-fn     (fn [context action] (swap! call-seq conj action) context)
          service1  (service Service1
                      []
                      (init [this context] (lc-fn context :init-service1))
                      (startup [this context] (lc-fn context :startup-service1))
                      (service1-fn [this] (lc-fn nil :service1-fn)))
          service2  (service Service2
                      [[:Service1 service1-fn]]
                      (init [this context] (lc-fn context :init-service2))
                      (startup [this context] (lc-fn context :startup-service2))
                      (service2-fn [this] (lc-fn nil :service2-fn)))
          service3  (service Service3
                       [[:Service2 service2-fn]]
                       (init [this context] (lc-fn context :init-service3))
                       (startup [this context] (lc-fn context :startup-service3))
                       (service3-fn [this] (lc-fn nil :service3-fn)))]
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
          app (boot! [service1 service2])
          s2 (get-service app :Service2)]
      (is (= "HELLO FOO!" (service2-fn s2))))))

(defprotocol Service4
  (service4-fn1 [this])
  (service4-fn2 [this]))

(deftest service-this-test
  (testing "should be able to call other functions in same service via 'this'"
    (let [service4  (service Service4
                      []
                      (init [this context] context)
                      (startup [this context] context)
                      (service4-fn1 [this] "foo!")
                      (service4-fn2 [this] (str (service4-fn1 this) " bar!")))
          app       (boot! [service4])
          s4        (get-service app :Service4)]
      (is (= "foo! bar!" (service4-fn2 s4))))))

(deftest context-test
  (testing "should error if lifecycle function doesn't return context"
    (let [service1 (service Service1
                      []
                      (init [this context] "hi")
                      (startup [this context] context)
                      (service1-fn [this] "hi"))]
      (is (thrown-with-msg?
            IllegalStateException
            #"Lifecycle function 'init' for service ':Service1' must return a context map \(got: \"hi\"\)"
            (boot! [service1]))))
    (let [service1 (service Service1
                            []
                            (init [this context] context)
                            (startup [this context] "hi")
                            (service1-fn [this] "hi"))]
      (is (thrown-with-msg?
            IllegalStateException
            #"Lifecycle function 'startup' for service ':Service1' must return a context map \(got: \"hi\"\)"
            (boot! [service1])))))
  (testing "context should be available in subsequent lifecycle functions"
    (let [startup-context (atom nil)
          service1        (service Service1
                            []
                            (init [this context] (assoc context :foo :bar))
                            (startup [this context] (reset! startup-context context))
                            (service1-fn [this] "hi"))]
      (boot! [service1])
      (is (= {:foo :bar} @startup-context))))
  (testing "context should be accessible in service functions"
    (let [sfn-context (atom nil)
          service1 (service Service1
                            []
                            (init [this context] (assoc context :foo :bar))
                            (startup [this context] context)
                            (service1-fn [this] (reset! sfn-context (service-context this))))
          app       (boot! [service1])
          s1        (get-service app :Service1)]
      (service1-fn s1)
      (is (= {:foo :bar} @sfn-context))
      (is (= {:foo :bar} (service-context s1)))))
  (testing "context from other services should not be visible"
    (let [s2-context (atom nil)
          service1 (service Service1
                            []
                            (init [this context] (assoc context :foo :bar))
                            (startup [this context] context)
                            (service1-fn [this] "hi"))
          service2 (service Service2
                            [[:Service1 service1-fn]]
                            (init [this context] context)
                            (startup [this context] (reset! s2-context (service-context this)))
                            (service2-fn [this] "hi"))

          app       (boot! [service1 service2])]
      (is (= {} @s2-context)))))
