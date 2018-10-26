(ns engine.simple-priority-test
  (:require [engine.core :refer :all]))

(defrule shutoff
  [?counter :counter]
  =>
  (remove! ?counter))

(defrule increment
  {:priority 10}
  [?thing :thing]
  [?counter :counter]
  =>
  (remove! ?thing)
  (insert! {:type :tally}))


