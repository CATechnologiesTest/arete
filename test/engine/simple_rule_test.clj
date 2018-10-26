(ns engine.simple-rule-test
  (:require [engine.core :refer :all]))

(defrule foo
  [?ball1 :ball
   (= (:pattern ?ball1) :stripe)]
  [?ball2 :ball
   (= (:pattern ?ball2) :solid)
   (= (:color ?ball2) (:color ?ball1))
   (> (:value ?ball2) (:value ?ball1))]
  =>
  (insert! {:type :pair :ball1 (:value ?ball1) :ball2 (:value ?ball2)}))


