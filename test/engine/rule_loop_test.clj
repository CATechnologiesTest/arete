(ns engine.rule-loop-test
  (:require [engine.core :refer :all]))

(defrule looper
  [?x :x]
  =>
  (remove! ?x)
  (insert! ?x))
