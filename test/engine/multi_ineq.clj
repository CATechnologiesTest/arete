(ns engine.multi-ineq
  (:require [engine.core :refer :all]))

(defrule multi1
  [?ball1 :ball
   (= (:pattern ?ball1) :stripe)]
  [?ball2 :ball
   (= (:pattern ?ball2) :solid)
   (= (:color ?ball2) (:color ?ball1))
   (<< (:value ?ball2) (:value ?ball1))
   (>> (:other-value ?ball2) (:other-value ?ball1))]
  [?brong :brong (= (:value ?brong) (:value ?ball2))]
  =>
  (insert! {:type :triple :ball1 (:value ?ball1) :ball2 (:value ?ball2)
            :brong (:value ?brong)}))

(defrule multi2
  [?ball1 :ball
   (= (:pattern ?ball1) :stripe)]
  [?ball2 :ball
   (= (:pattern ?ball2) :solid)
   (= (:color ?ball2) (:color ?ball1))
   (<<= (:value ?ball2) (:value ?ball1))
   (>>= (:other-value ?ball2) (:other-value ?ball1))]
  [?brong :brong (= (:value ?brong) (:value ?ball2))]
  =>
  (insert! {:type :triple :ball1 (:value ?ball1) :ball2 (:value ?ball2)
            :brong (:value ?brong)}))

