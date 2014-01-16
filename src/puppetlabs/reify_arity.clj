(ns puppetlabs.reify-arity)

(defprotocol Foo
  (foo [this x] [this x y]))

(let [r (reify Foo
          (foo [this x] x)
          (foo [this x y] (+ x y)))]
  (println "(foo r 3): " (foo r 3))
  (println "(foo r 5 8): " (foo r 5 8)))

(let [p (proxy [puppetlabs.reify_arity.Foo] []
          (foo
            ([x] x)
            ([x y] (+ x y))))
      ]
  (println "(.foo r 3): " (.foo p 3))
  (println "(.foo r 5 8): " (.foo p 5 8)))