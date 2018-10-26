(ns engine.context-test
  (:require [engine.core :refer :all]))

(def foo (atom 6))

(defrule dummy
  => 6)

(defcontext {:before (fn [] (swap! foo #(+ % (context-value :to-add))))
             :after #(if (= @foo 28)
                       (reset! foo 496)
                       (throw (RuntimeException. "Context failed")))
             :data {:to-add 22}})


