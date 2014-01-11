;; This namespace contains tests for the experimental service API
;; that are specific to the prismatic implementation

(ns puppetlabs.trapperkeeper.experimental.services-internal-test
  (:require [clojure.test :refer :all]
            [plumbing.fnk.pfnk :as pfnk]
            [puppetlabs.trapperkeeper.experimental.services :refer [service service-map]]
            [puppetlabs.trapperkeeper.experimental.services-internal :as si]))

(deftest service-forms-test
  (testing "should support forms that include protocol"
    (is (= ['Foo [] '()]
           (si/find-prot-and-deps-forms! '(Foo [])))))
  (testing "should support forms that do not include protocol"
    (is (= [nil [] '()]
           (si/find-prot-and-deps-forms! '([])))))
  (testing "result should include vector of fn forms if provided"
    (is (= ['Foo [] '((fn1 [] "fn1") (fn2 [] "fn2"))]
           (si/find-prot-and-deps-forms!
             '(Foo [] (fn1 [] "fn1") (fn2 [] "fn2")))))
    (is (= [nil [] '((fn1 [] "fn1") (fn2 [] "fn2"))]
           (si/find-prot-and-deps-forms!
             '([] (fn1 [] "fn1") (fn2 [] "fn2"))))))
  (testing "should throw exception if the first form is not the protocol symbol or dependency vector"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Invalid service definition; first form must be protocol or dependency list; found '\"hi\"'"
          (si/find-prot-and-deps-forms! '("hi" [])))))
  (testing "should throw exception if the first form is a protocol sym and the second is not a dependency vector"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Invalid service definition; expected dependency list following protocol, found: '\"hi\"'"
          (si/find-prot-and-deps-forms! '(Foo "hi")))))
  (testing "should throw an exception if all remaining forms are not seqs"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Invalid service definition; expected function definitions following dependency list, invalid value: '\"hi\"'"
          (si/find-prot-and-deps-forms! '(Foo [] (fn1 [] "fn1") "hi"))))))

(defn local-resolve
  "TODO: docs"
  [sym]
  (ns-resolve
    'puppetlabs.trapperkeeper.experimental.services-internal-test
    sym))

(defprotocol EmptyProtocol)
(def NonProtocolSym "hi")

(deftest protocol-syms-test
  (testing "should not throw exception if protocol exists"
    (is (si/protocol?
          (si/validate-protocol-sym!
            'EmptyProtocol
            (local-resolve 'EmptyProtocol)))))

  (testing "should throw exception if service protocol sym is not resolvable"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Unrecognized service protocol 'UndefinedSym'"
          (si/validate-protocol-sym! 'UndefinedSym (local-resolve 'UndefinedSym)))))

  (testing "should throw exception if service protocol symbol is resolveable but does not resolve to a protocol"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Specified service protocol 'NonProtocolSym' does not appear to be a protocol!"
          (si/validate-protocol-sym! 'NonProtocolSym (local-resolve 'NonProtocolSym))))))

(deftest build-fns-map-test
  (testing "minimal services may not define functions other than lifecycle functions"
    (is (thrown-with-msg?
          IllegalArgumentException
          #"Service attempts to define function 'foo', but does not provide protocol"
          (si/build-fns-map! nil [] ['init 'start]
            '((init [this context] context)
              (start [this context] context)
              (foo [this] "foo")))))))

(defprotocol Service1
  (service1-fn [this]))

(defprotocol Service2
  (service2-fn [this]))

(deftest prismatic-functionality-test
  (testing "prismatic fnk is initialized properly"
    (let [service1  (service Service1
                       []
                       (init [this context] context)
                       (start [this context] context)
                       (service1-fn [this] "Foo!"))
          service2  (service Service2
                       [[:Service1 service1-fn]]
                       (init [this context] context)
                       (start [this context] context)
                       (service2-fn [this] "Bar!"))
          s1-graph  (service-map service1)
          s2-graph  (service-map service2)]
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