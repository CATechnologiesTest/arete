(ns engine.negated-wme-removal-test
  (:require [engine.core :refer :all]))

(defrule initrule
  =>
  (insert! {:type :foobar :val 3}))

(defrule update-rule
  [?u :update]
  [?f :foobar]
  [?other :other]
  =>
  (remove! ?other)
  (insert! {:type :updated}))

(defrule testrule
  {:priority 28}
  [?f :foobar (= (:val ?f) 3)]
  [:not [?t :tally]]
  [:nand
   [?eto :eto (> (:val ?eto) (:val ?f)) (not= (rem (:val ?eto) 2) 0)]
   [?other :other]]
  =>
  (remove! ?f)
  (insert! {:type :tally}))



