(ns engine.type-hierarchy-test
  (:require [engine.core :refer :all]))

(defancestor :ancestor :relative)
(defancestor :parent :ancestor)
(defancestor :descendant :parent)

(defancestor [:a :b :c] :d)
(defancestor [:d :e] :f)

(defrule hierarchy
  [?person :ancestor]
  =>
  (remove! ?person)
  (insert! {:type :relative}))

(defrule hierarchy2
  [?f :f]
  =>
  (remove! ?f)
  (insert! {:type :z}))


