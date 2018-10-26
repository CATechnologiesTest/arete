(ns engine.module-order
  (:require [engine.core :refer :all]))

(deforder (:from-module :engine.module-order))

(defrule rule1
  [?x :x]
  =>
  (remove! ?x)
  (insert! {:type :order}))
