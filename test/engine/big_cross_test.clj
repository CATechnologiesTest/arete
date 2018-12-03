(ns engine.big-cross-test
  (:require [engine.core :refer :all]))

(defrule foo
  [?ball1 :ball
   (= (:pattern ?ball1) :stripe)]
  [?ball2 :ball
   (= (:pattern ?ball2) :solid)
   (= (:color ?ball2) (:color ?ball1))
   (>> (:value ?ball2) (:value ?ball1))]
  [?gurk :gurk (= (:value ?gurk) (:value ?ball2))]
  =>
  (insert! {:type :triple :ball1 (:value ?ball1) :ball2 (:value ?ball2)
            :gurk (:value ?gurk)}))


