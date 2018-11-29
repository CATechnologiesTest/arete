(ns engine.speed-test
  (:require [clojure.test :refer :all]
            [engine.core :refer :all]))

(defrule finish
  {:priority 10}
  [?limit :limit]
  [?counter :counter (>>= ^long (:value ?counter) ^long (:value ?limit))]
  =>
  (remove! ?limit)
  (remove! ?counter)
  (insert! {:type :result :value (:value ?counter)}))

(defrule increment
  [?counter :counter]
  =>
  (remove! ?counter)
  (insert! (update ?counter :value inc)))

(deftest performance-test
  (testing "raw simmple rule performance"
    (let [eng (engine :engine.speed-test)
          _ (eng :configure {:max-repeated-firings 200000})
          result (time (eng :run [{:type :limit :value 100000}
                                  {:type :counter :value 0}]))]
      (is (= (:value (first (:result result))) 100000)))))



