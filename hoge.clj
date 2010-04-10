
(defn sum-stm [coll]
  (let [sum (ref 0)]
    (doseq [x coll]
      (dosync (alter sum + x)))
    @sum))

(defn sum-no-stm [coll]
  (apply + coll))

(defmacro many-times [n body]
  `(dotimes [i# ~n]
     (time ~body)))


(defmacro komatta1 [foo]
  `(fn [x] (~foo x)))

(defmacro komatta2 [foo]
  `(fn [x#] (~foo x#)))
