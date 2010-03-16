
(ns prejure.misc
  (:use [clojure core]
        [clojure.contrib str-utils java-utils])
  )

(defn map-even [f coll]
  (map (fn [[idx val]]
	 (if (even? idx)
	   (f val)
	   val))
       (map list (iterate inc 1) coll)))

(defn map-odd [f coll]
  (map (fn [[idx val]]
	 (if (odd? idx)
	   (f val)
	   val))
       (map list (iterate inc 1) coll)))

(defmacro case [val & clauses]
  (let [formalize-keys
	(fn [keys]
	  (cond
	   (and (list? keys)
		(not (= 'list (first keys)))) (cons 'list keys)
	   (not (list? keys)) `(list ~keys)
	   true keys))
	arg1 (gensym)
	arg2 (gensym)
	arg3 (gensym)]
    `(condp (fn [~arg1 ~arg2] (some (fn [~arg3] (= ~arg2 ~arg3)) ~arg1)) ~val
       ~@(map-odd formalize-keys clauses))))