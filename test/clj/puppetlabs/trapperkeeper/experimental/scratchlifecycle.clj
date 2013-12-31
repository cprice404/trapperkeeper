(ns puppetlabs.trapperkeeper.experimental.scratchlifecycle
  (:require [puppetlabs.trapperkeeper.experimental.services :refer [service boot!]]))

(defprotocol Service1
  (service1-fn [this]))

(defprotocol Service2
  (service2-fn [this]))

(defprotocol Service3
  (service3-fn [this]))
;
;(let [call-seq  (atom [])
;      service1  (service Service1
;                         []
;                         (init [this context] (swap! call-seq conj :init-service1))
;                         (startup [this context] (swap! call-seq conj :startup-service1))
;                         (service1-fn [this] (swap! call-seq conj :service1-fn)))
;      service2  (service Service2
;                         [[:Service1 service1-fn]]
;                         (init [this context] (swap! call-seq conj :init-service2))
;                         (startup [this context] (swap! call-seq conj :startup-service2))
;                         (service2-fn [this] (swap! call-seq conj :service2-fn)))
;      service3  (service Service3
;                         [[:Service2 service2-fn]]
;                         (init [this context] (swap! call-seq conj :init-service3))
;                         (startup [this context] (swap! call-seq conj :startup-service3))
;                         (service3-fn [this] (swap! call-seq conj :service3-fn)))]
;  (boot! [service1 service3 service2])
;  (println "CALL SEQ IS: " @call-seq))

(service Service2
         [[:Service1 service1-fn]]
         (init [this context] context)
         (startup [this context] context)
         (service2-fn [this] (str "HELLO " (service1-fn))))