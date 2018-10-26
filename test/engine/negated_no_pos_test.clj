(ns engine.negated-no-pos-test
  (:require [engine.core :refer :all]))

(defrule testrule
  [:not [? :val]]
  =>
  (insert! {:type :val}))




