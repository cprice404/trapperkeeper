(ns puppetlabs.trapperkeeper.experimental.scratch)

(defn foo
  [pattern x]
  (let [partfoo (partial foo pattern)]
    (if-not (seq? x)
      x
      (let [patsize (count pattern)]
        (if (and (>= (count x) patsize)
                 (= (take patsize x) pattern))
          (map partfoo (cons :match (nthrest x patsize)))
          (map partfoo x))))))
