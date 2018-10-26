(ns engine.order
  (:require [engine.core :refer :all]))

(deforder (:with :x) (:without :y) :oldest)

(def inst-counter (atom 0))

(defn tally [rulekey]
  (insert! {:type rulekey :value (swap! inst-counter inc)}))

(defrule rule1
  [?x :x]
  =>
  (tally :rule1))

(defrule rule2
  [?x :x]
  [?y :y]
  =>
  (tally :rule2))

(defrule rule3
  [?z :z]
  =>
  (tally :rule3)
  (insert! {:type :a}))

(defrule rule4
  [?z :z]
  [?y :y]
  =>
  (tally :rule4))

(defrule rule5
  [?z :z]
  [?y :y]
  [?a :a]
  =>
  (tally :rule5))
