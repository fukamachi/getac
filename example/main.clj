(defn solve []
  (let [a (Integer/parseInt (read-line))
        [b c] (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))
        s (read-line)]
    (println (+ a b c) s)))

(solve)
