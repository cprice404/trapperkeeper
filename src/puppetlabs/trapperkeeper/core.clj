(ns puppetlabs.trapperkeeper.core
  (:require [clojure.tools.logging :as log]
            [slingshot.slingshot :refer [try+]]
            [puppetlabs.kitchensink.core :refer [without-ns]]
            [puppetlabs.trapperkeeper.services :as services]
            [puppetlabs.trapperkeeper.bootstrap :as bootstrap]
            [puppetlabs.trapperkeeper.internal :refer [parse-cli-args! run-app]]))

(def #^{:macro true
        :doc "An alias for the `puppetlabs.trapperkeeper.services/service` macro
             so that it is accessible from the core namespace along with the
             rest of the API."}
  service #'services/service)

(def #^{:macro true
        :doc "An alias for the `puppetlabs.trapperkeeper.services/defservice` macro
             so that it is accessible from the core namespace along with the
             rest of the API."}
  defservice #'services/defservice)

;; TODO docs
(def main-app nil)

(defn run
  "Bootstraps a trapperkeeper application and runs it.
  Blocks the calling thread until trapperkeeper is shut down.
  `cli-data` is expected to be a map constructed by parsing the CLI args.
  (see `parse-cli-args`)"
  [cli-data]
  {:pre [(map? cli-data)]}
  (let [app (bootstrap/bootstrap cli-data)]
    ;; TODO docs
    (alter-var-root #'main-app (fn [_] app))
    (run-app app)))

(defn main
  "Launches the trapperkeeper framework. This function blocks until
  trapperkeeper is shut down. This may be called directly, but is also called by
  `puppetlabs.trapperkeeper.core/-main` if you use `puppetlabs.trapperkeeper.core`
  as the `:main` namespace in your leinengen project."
  [& args]
  {:pre [((some-fn sequential? nil?) args)
         (every? string? args)]}
  (try+
    (-> args
        (parse-cli-args!)
        (run))
    (catch map? m
      (println (:message m))
      (case (without-ns (:type m))
        :cli-error (System/exit 1)
        :cli-help  (System/exit 0)))))
