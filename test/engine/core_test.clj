(ns engine.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [engine.core :refer :all]
            [engine.context-test :as ctxt]))

(deftest big-cross
  (testing "inequality performance"
    (let [data (atom [])
          eng (engine :engine.big-cross-test)]
      (loop [i 0 j -9998]
        (when (< i 10000)
          (swap! data conj {:type :ball :pattern :stripe :color :red
                            :value i})
          (swap! data conj {:type :ball :pattern :solid :color :red
                            :value j})
          (recur (inc i) (inc j))))
      (loop [g 0]
        (when (< g 5)
          (swap! data conj {:type :gurk :value g})
          (recur (inc g))))
      (time (eng :run-list @data)))))

(deftest simple-priority
  (testing "Simple rule, two priorities w/ deletion"
    (is (= ((engine :engine.simple-priority-test)
            :run
            [{:type :thing} {:type :thing} {:type :thing} {:type :counter}])
           {:tally [{:type :tally} {:type :tally} {:type :tally}]}))))

(deftest multiple-rule-modules
  (testing "Same priority test with multiple rule sets loaded into engine"
    (is (= ((engine :engine.simple-priority-test :engine.big-cross-test)
            :run
            [{:type :thing} {:type :thing} {:type :thing} {:type :counter}])
           {:tally [{:type :tally} {:type :tally} {:type :tally}]}))))

(deftest type-hierarchy
  (testing "Ancestor types work correctly"
    (let [result ((engine :engine.type-hierarchy-test) :run
                  [{:type :descendant} {:type :parent} {:type :unrelated}
                   {:type :a}])]
      (is (nil? (:descendent result)))
      (is (nil? (:parent result)))
      (is (= (:unrelated result) [{:type :unrelated}]))
      (is (= (count (:relative result)) 2))
      (is (nil? (:f result)))
      (is (= (count (:z result)) 1)))))

(deftest negated-conjunctions
  (testing "Support for NAND1"
    (is (= (:tally
            ((engine :engine.negated-conjunction-test)
            :run
             [{:type :eto :val 6} {:type :eto :val 4}]))
           [{:type :tally}])))
  (testing "Support for NAND2"
    (is (= (:tally
            ((engine :engine.negated-conjunction-test)
            :run
             [{:type :eto :val 6} {:type :eto :val 5}]))
           nil)))
  (testing "Support for NAND3"
    (is (= (:tally
            ((engine :engine.negated-conjunction-test)
            :run
             [{:type :eto :val 6} {:type :eto :val 5} {:type :other}]))
           [{:type :tally}])))
  (testing "Remove instantiations on blocker creation"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:product
                     (eng :run [{:type :item :val 10} {:type :item :val 10}])))
             1))))
  (testing "Multiple wmes with same blocking wmes"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:result
                     (eng :run [{:type :x} {:type :x} {:type :x} {:type :x}])))
             4))))
  (testing "Multiple hash tests"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:multi
                     (eng :run [{:type :z :lav 6 :oog 28}
                                {:type :z :lav 496 :oog 8128}
                                {:type :q :val 6 :goo 28}
                                {:type :q :val 496 :goo 8128}
                                {:type :q :val 6 :goo 4}
                                {:type :q :val 4 :goo 28}])))
             2))))
  (testing "upstream not"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:feebrak
                     (eng :run [{:type :beep :value 6 :blerj 28}
                                {:type :beep :value 6 :blerj 28}
                                {:type :borp :value 28}])))
             2))))
  (testing "not not"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:gonzo
                     (eng :run [{:type :smorg :value 6}
                                {:type :skoolj :value 6}])))
             0))))
  (testing "not not2"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:gonzo2
                     (eng :run [{:type :smorg2 :value 6}])))
             1))))
  (testing "not not again"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:gonzo-again
                     (eng :run [{:type :smorg-again :value 6}
                                {:type :skoolj-again :value 6}])))
             1))))
  (testing "nested not not"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:ngonzo
                     (eng :run [{:type :nsmorg :value 6}
                                {:type :nfoo}
                                {:type :nskoolj :value 6}])))
             1))))
  (testing "nested not not not"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (count (:ngonzo3
                     (eng :run [{:type :nsmorg3 :value 6}
                                {:type :nfoo3}
                                {:type :nskoolj3 :value 6}])))
             1))))
  )

(deftest removing-wme-from-negated-conjunction
  (testing "removing a wme from nand unlocks blocked instantiations"
    (let [eng (engine :engine.negated-wme-removal-test)
          interim-result (eng :cycle
                              [{:type :eto :val 6}
                               {:type :eto :val 5}
                               {:type :other}])]
      (is (nil? (:tally interim-result)))
      (is (= (count (:foobar interim-result)) 1))
      (is (= (count (:other interim-result)) 1))
      (let [final-result (eng :cycle [{:type :update}])]
        (is (= (count (:foobar final-result)) 0))
        (is (= (count (:tally final-result)) 1))
        (is (= (count (:updated final-result)) 1))
        (is (= (count (:other final-result)) 0))))))

(deftest clearing-engine
  (testing "Clearing state from engine"
    (let [eng (engine :engine.negated-conjunction-test)]
      (is (= (:tally
              (eng
               :run
               [{:type :eto :val 6} {:type :eto :val 5} {:type :other}]))
             [{:type :tally}]))
      (is (empty? (eng :wmes)))
      (is (= (:tally
              (eng
               :cycle
               [{:type :eto :val 6} {:type :eto :val 5} {:type :other}]))
             [{:type :tally}]))
      (is (= (:tally (eng :wmes))
             [{:type :tally}]))
      (is (= (count (:eto (eng :wmes))) 2)))))

(deftest no-pos
  (testing "Negated condition without a positive condition"
    (let [eng (engine :engine.negated-no-pos-test)]
      (is (= (:val (eng :run []))
             [{:type :val}])))))

(deftest ordering
  (testing "Instantiation ordering is working"
    (doseq [eng [(engine :engine.order) (engine :engine.order2)]]
      (let [result (eng :run [{:type :x} {:type :y} {:type :z}
                              {:type :counter :value 1}])]
        (is (= (:value (first (:rule1 result))) 1))
        (is (= (:value (first (:rule2 result))) 2))
        (is (= (:value (first (:rule3 result))) 3))
        (is (= (:value (first (:rule4 result))) 4))
        (is (= (:value (first (:rule5 result))) 5))))
    (let [modeng (engine :engine.module-order2 :engine.module-order)
          result (modeng :run [{:type :x} {:type :x}])]
      (is (= (count (:order result)) 2))
      (is (= (count (:order2 result)) 0)))))

(deftest empty-modules
  (testing "Creating an engine with a module containing no rules raises an error"
    (is (thrown-with-msg? RuntimeException #":engine.core-test contains no rules."
                          (engine :engine.core-test)))))

(deftest context
  (testing "Rule module context - before function, after function, bound data"
    (let [result ((engine :engine.context-test) :run [])]
      (is (= @ctxt/foo 496)))))

(deftest rule-loop
  (testing "Looping rule causes engine exit"
    (is (thrown-with-msg?
         RuntimeException
         #"Rule: 'engine.rule-loop-test/looper' is stuck in a loop."
         ((engine :engine.rule-loop-test) :run [{:type :x}])))))

(deftest multi-optimized-inequality
  (testing "Chained optimized inequalities"
    (let [data (atom [])
          eng (engine :engine.multi-ineq)]
      (loop [i 0 j -10]
        (when (< i 20)
          (swap! data conj {:type :ball :pattern :stripe :color :red
                            :value i :other-value j})
          (swap! data conj {:type :ball :pattern :solid :color :red
                            :value j :other-value i})
          (recur (inc i) (inc j))))
      (loop [b 0]
        (when (< b 5)
          (swap! data conj {:type :brong :value b})
          (recur (inc b))))
      (is (= (count (:triple (eng :run @data))) 175)))))
