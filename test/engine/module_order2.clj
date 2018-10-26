(ns engine.module-order2
  (:require [engine.core :refer :all]))

(defrule rule2
  [?x :x]
  =>
  (remove! ?x)
  (insert! {:type :order2}))
