(ns engine.negated-conjunction-test
  (:require [engine.core :refer :all]))

(defrule initrule
  =>
  (insert! {:type :foobar :val 3}))

(defrule testrule
  [?f :foobar (= (:val ?f) 3)]
  [:nand
   [?eto :eto (> (:val ?eto) (:val ?f)) (not= (rem (:val ?eto) 2) 0)]
   [:not [?other :other]]]
  =>
  (insert! {:type :tally}))

(defrule removeinst
  [?item :item]
  [:not [?prod :product (= (:pval ?prod) (:val ?item))]]
  =>
  (insert! {:type :product :pval (:val ?item)}))

(defrule x-when-not-y
  [?x :x]
  [:not [?y :y]]
  =>
  (remove! ?x)
  (insert! {:type :y}))

(defrule y-to-result
  [?y :y]
  =>
  (remove! ?y)
  (insert! {:type :result}))

(defrule multi-hash
  [?z :z]
  [?q :q (= (:val ?q) (:lav ?z)) (= (:oog ?z) (:goo ?q))]
  =>
  (insert! {:type :multi}))

(defrule upstream-neg
  [?beep :beep]
  [:not [?boop :boop (= (:value ?boop) (:value ?beep))]]
  [?borp :borp (= (:value ?borp) (:blerj ?beep))]
  =>
  (insert! {:type :boop :value (:value ?beep)})
  (insert! {:type :feebrak}))

(defrule upstream
  [? :feebrak]
  [?boop :boop]
  =>
  (remove! ?boop))

;; Problem:

;; a negation of a wme type that IS PRESENT is bypassed by a
;; non-matching previous negation (higher on the LHS)
(defrule not-not
  [?smorg :smorg]
  [:not [?smoop :smoop (= (:value ?smoop) (:value ?smorg))]]
  [:not [?skoolj :skoolj (= (:value ?skoolj) (:value ?smorg))]]
  =>
  (insert! {:type :smoop :value (:value ?smorg)})
  (insert! {:type :gonzo}))

(defrule not-not2
  [?smorg :smorg2]
  [:not [?smoop :smoop2 (= (:value ?smoop) (:value ?smorg))]]
  [:not [?skoolj :skoolj2 (= (:value ?skoolj) (:value ?smorg))]]
  =>
  (insert! {:type :smoop2 :value (:value ?smorg)})
  (insert! {:type :gonzo2}))

(defrule one-step
  [?skoolj :skoolj-again]
  =>
  (remove! ?skoolj))

(defrule not-not-again
  [?smorg :smorg-again]
  [:not [?smoop :smoop-again (= (:value ?smoop) (:value ?smorg))]]
  [:not [?skoolj :skoolj-again (= (:value ?skoolj) (:value ?smorg))]]
  =>
  (insert! {:type :smoop-again :value (:value ?smorg)})
  (insert! {:type :gonzo-again}))


;; nested not-not

(defrule nested-not-not
  [?nsmorg :nsmorg]
  [:nand
   [?nfoo :nfoo]
   [:not [?nsmoop :nsmoop (= (:value ?nsmoop) (:value ?nsmorg))]]
   [:not [?nskoolj :nskoolj (= (:value ?nskoolj) (:value ?nsmorg))]]]
  =>
  (insert! {:type :nsmoop :value (:value ?nsmorg)})
  (insert! {:type :ngonzo}))

;; nested not-not-not

(defrule nested-not-not-not
  [?nsmorg :nsmorg3]
  [:nand
   [?nfoo :nfoo3]
   [:not [?nsmoop :nsmoop3 (= (:value ?nsmoop) (:value ?nsmorg))]]
   [:not [?nstoop :nstoop3 (= (:value ?nstoop) (:value ?nsmorg))]]
   [:not [?nskoolj :nskoolj3 (= (:value ?nskoolj) (:value ?nsmorg))]]]
  =>
  (insert! {:type :nsmoop3 :value (:value ?nsmorg)})
  (insert! {:type :ngonzo3}))
