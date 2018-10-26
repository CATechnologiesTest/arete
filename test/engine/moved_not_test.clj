(ns engine.moved-not-test
  (:require [engine.core :refer :all]))

(defrule initrule
  =>
  (insert! {:type :foobar :val 6})
  (insert! {:type :bazquux :val 28})
  (insert! {:type :other :val 4}))

(defrule remove-other4
  [?f :foobar]
  [?b :bazquux]
  [?other :other (= (:val ?other) 4)]
  =>
  (remove! ?f)
  (remove! ?b)
  (remove! ?other)
  (insert! (assoc ?other :val 15))
  (insert! (assoc ?f :val 3))
  (insert! (assoc ?b :val 3)))

(defrule update-other
  [?other :other (> (:val ?other) 4)]
  =>
  (remove! ?other)
  (insert! (assoc ?other :val 3)))

(defrule mainrule
  [?f :foobar]
  [:not [?other :other (= (:val ?other) (:val ?f))]]
  [?b :bazquux (= (:val ?b) (:val ?f))]
  =>
  (insert! {:type :correct}))



