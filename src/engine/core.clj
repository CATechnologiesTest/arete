(ns engine.core
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [clojure.stacktrace :as st]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [flatland.ordered.set :as ordered]
            [clojure.java.io :as io]
            [potemkin :refer [import-vars]]
            [engine.runtime
             :refer :all
             :exclude [get-stack-trace ppwrap wme-types >> << >>= <<=]]
            [engine.viewer :refer [view]])
  (:import [org.javasimon SimonManager Stopwatch]
           [java.util HashMap TreeSet TreeMap TreeMap$Entry
            TreeMap$DescendingSubMap Comparator ArrayDeque]
           clojure.lang.PersistentVector))

;; Variables from runtime that we want to expose from core (using the Potemkin
;; library's "import-vars")
(import-vars [engine.runtime
              get-stack-trace
              ppwrap
              wme-types
              >>
              <<
              >>=
              <<=])

;; Whether or not to turn on simon performance monitoring -- assumes
;; NO_PERF_COMPILE environment variable was not set during compilation
(def enable-perf-mon (atom false))

;; Main API and ruleset construction code for RETE/TREAT based rule
;; engine. All the macros for defining rules and their support code is
;; in this file along with the main engine calls available to engine
;; users: insert!, remove!, collect!, and the calls on the engine
;; itself (run, run-map, run-list, cycle, configure, timing, wmes,
;; and wme-list.

;; Define conflict resolution ordering for a ruleset. The functions passed
;; to 'deforder' are applied in order to sort instantiations. So, the
;; expression: (deforder (:with :x) (:without :y) :oldest) would favor
;; instantiations containing wmes of type :x followed by those NOT
;; containing wmes of type :y, followed by older over younger.
(defmacro deforder [& funs]
  (let [left (gensym "left")
        right (gensym "right")]
    `(swap! orderings
            #(assoc % (keyword (ns-name *ns*))
                    (list (fn [~left ~right]
                            ~(compile-ordering funs left right)))))))

(defn update-all [in-map f]
  (reduce (fn [m k] (update m k f)) in-map (keys in-map)))

;; Search for variables to determine if an expression references multiple
;; wmes and needs to be treated as a beta node.
(defn find-any [vars sexp]
  (cond (or (nil? sexp) (and (seq? sexp) (empty? sexp))) false
        (vars sexp) true
        (or (seq? sexp) (vector? sexp)) (or (find-any vars (first sexp))
                                            (find-any vars (rest sexp)))
        (map? sexp) (or (find-any vars (keys sexp))
                        (find-any vars (vals sexp)))
        true false))

;; Every test must reference the wme declared in its match expression.
(defn translate-obj-test [x var]
  (if (not (find-any #{var} x))
    (throw (RuntimeException. "Must reference matched wme in tests."))
    x))

(defn get-upstream [nodes]
  (first (:upstream (last nodes))))

(defn node-type [fnode]
  (if (= fnode '=>)
    nil
    (first fnode)))


(defn order-sub-exps [test-exp vars]
  (let [left (second test-exp)
        right (nth test-exp 2)
        in-order (find-any vars left)]
    (if in-order [left right] [right left])))

(defn make-vec-funs [test-exp vars wme-var]
  (let [pairs (map #(order-sub-exps % vars) test-exp)]
    [(mapv first pairs) `(fn [~wme-var] ~(mapv second pairs))]))

(defn make-hash-funs [test-exp vars wme-var]
  (if (vector? test-exp)
    (make-vec-funs test-exp vars wme-var)
    (let [pair (order-sub-exps test-exp vars)]
      [(first pair) `(fn [~wme-var] ~(second pair))])))

(defn make-ie-data [test-exp vars wme-var]
  (conj (make-vec-funs test-exp vars wme-var)
        (mapv first test-exp)))

(defn make-add-fun [alpha-tests lhash-fun luie-fun comps alpha-name
                    rname-string wme-var
                    oset ocur empty-count-name rule-name main-fun]
  ;; Select a function to handle alpha nodes depending on whether:
  ;; a) there are single object filtering tests
  ;; b) there is a beta equality match requiring hashing
  ;; c) we want debug output
  (case [(some? alpha-tests) (some? lhash-fun) @compile-with-debug]
    ;; Tests, Hashing, Debug
    [true true true] `(~alpha-name
                       (debug-alpha-hash-fun
                        ~rname-string
                        '~alpha-name
                        (fn [~wme-var]
                          (and ~@(map #(translate-obj-test % wme-var)
                                      alpha-tests)))
                        ~lhash-fun
                        ~luie-fun
                        ~comps
                        ~oset
                        ~ocur
                        ~empty-count-name
                        '~rule-name
                        ~main-fun))
    ;; Tests, Hashing, No Debug
    [true true false] `(~alpha-name
                        (alpha-hash-fun
                         (fn [~wme-var]
                           (and ~@(map #(translate-obj-test % wme-var)
                                       alpha-tests)))
                         ~lhash-fun
                         ~luie-fun
                         ~comps
                         ~oset
                         ~ocur
                         ~empty-count-name
                         ~main-fun))
    ;; Tests, No Hashing, Debug
    [true false true] `(~alpha-name
                        (debug-alpha-fun
                         ~rname-string
                         '~alpha-name
                         (fn [~wme-var]
                           (and ~@(map #(translate-obj-test % wme-var)
                                       alpha-tests)))
                         ~luie-fun
                         ~comps
                         ~oset
                         ~ocur
                         ~empty-count-name
                         '~rule-name
                         ~main-fun))
    ;; Tests, No Hashing, No Debug
    [true false false] `(~alpha-name
                         (alpha-fun
                          (fn [~wme-var]
                            (and ~@(map #(translate-obj-test % wme-var)
                                        alpha-tests)))
                          ~luie-fun
                          ~comps
                          ~oset
                          ~ocur
                          ~empty-count-name
                          ~main-fun))
    ;; No Tests, Hashing, Debug
    [false true true] `(~alpha-name
                        (debug-alpha-hash-fun-no-tests
                         ~rname-string
                         '~alpha-name
                         ~lhash-fun
                         ~luie-fun
                         ~comps
                         ~oset
                         ~ocur
                         ~empty-count-name
                         '~rule-name
                         ~main-fun))
    ;; No Tests, Hashing, No Debug
    [false true false] `(~alpha-name
                        (alpha-hash-fun-no-tests
                         ~lhash-fun
                         ~luie-fun
                         ~comps
                         ~oset
                         ~ocur
                         ~empty-count-name
                         ~main-fun))
    ;; No Tests, No Hashing, Debug
    [false false true] `(~alpha-name
                        (debug-alpha-fun-no-tests
                         ~rname-string
                         '~alpha-name
                         ~luie-fun
                         ~comps
                         ~oset
                         ~ocur
                         ~empty-count-name
                         '~rule-name
                         ~main-fun))
    ;; No Tests, No Hashing, No Debug
    [false false false] `(~alpha-name
                          (alpha-fun-no-tests
                           ~luie-fun
                           ~comps
                           ~oset
                           ~ocur
                           ~empty-count-name
                           ~main-fun))))

(defn make-upstream-fun-no-beta-tests [op-name uhash-exp uie-exp ocur
                                       outer-vars vars
                                       upstream-node upstream-name
                                       outer-var-sublist sub-fun wme-var]
  (let [down (gensym "down")]
    (if upstream-node
      `(fn ~upstream-name [~down]
         (debugf "Running: %s" '~upstream-name)
         (~upstream-node
          (fn ~upstream-name [~@outer-var-sublist ~@vars]
            (~op-name
             ~@(when uhash-exp
                 (if outer-vars
                   [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                       ~uhash-exp)]
                   [uhash-exp]))
             ~@(when uie-exp
                 (if outer-vars
                   [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                       ~uie-exp)]
                 [uie-exp]))
             (fn ~sub-fun
               [~@outer-var-sublist ~wme-var]
               (~down ~@outer-var-sublist ~@(conj vars wme-var)))))))
      `(fn ~upstream-name [~down]
         (debugf "Running: %s" '~upstream-name)
         (~op-name
          ~@(when uhash-exp
              (if outer-vars
                [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                    ~uhash-exp)]
                [uhash-exp]))
          ~@(when uie-exp
              (if outer-vars
                [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                    ~uie-exp)]
                [uie-exp]))
          (fn ~sub-fun [~@outer-var-sublist ~wme-var]
            (~down ~@outer-var-sublist ~@(conj vars wme-var))))))))

(defn make-upstream-fun-with-beta-tests [op-name uhash-exp uie-exp ocur
                                         outer-vars vars
                                         beta-tests upstream-node upstream-name
                                         outer-var-sublist sub-fun wme-var
                                         rname-string]
  (let [down (gensym "down")]
    (if upstream-node
      `(fn ~upstream-name [~down]
         (debugf "Running: %s" '~upstream-name)
         (~upstream-node
          (fn ~upstream-name [~@outer-var-sublist ~@vars]
            (~op-name
             ~@(when uhash-exp
                 (if outer-vars
                   [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                       ~uhash-exp)]
                 [uhash-exp]))
             ~@(when uie-exp
                 (if outer-vars
                   [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                       ~uie-exp)]
                 [uie-exp]))
             ;; beta tests
             ~(if @compile-with-debug
                `(fn ~sub-fun [~@outer-var-sublist ~wme-var]
                   (let [split# (.start (SimonManager/getStopwatch "beta"))
                         rsplit# (.start (SimonManager/getStopwatch
                                          ~(str rname-string "-beta")))]
                     (when (and ~@(map #(translate-obj-test % wme-var)
                                       beta-tests))
                       (~down ~@outer-var-sublist ~@(conj vars wme-var)))
                     (simon-stop rsplit#)
                     (simon-stop split#)))
                `(fn ~sub-fun [~@outer-var-sublist ~wme-var]
                   (when (and ~@(map #(translate-obj-test % wme-var)
                                     beta-tests))
                     (~down ~@outer-var-sublist ~@(conj vars wme-var)))))))))
      `(fn ~upstream-name [~down]
         (debugf "Running: %s" '~upstream-name)
         (~op-name
          ~@(when uhash-exp
              (if outer-vars
                [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                    ~uhash-exp)]
                [uhash-exp]))
          ~@(when uie-exp
              (if outer-vars
                [`(let [~@outer-var-sublist engine.runtime/*outer-vars*]
                    ~uie-exp)]
                [uie-exp]))
          ~(if @compile-with-debug
             `(fn ~sub-fun [~@outer-var-sublist ~wme-var]
                (let [split# (.start (SimonManager/getStopwatch "beta"))
                      rsplit# (.start (SimonManager/getStopwatch
                                       ~(str rname-string "-beta")))]
                  (when (and ~@(map #(translate-obj-test % wme-var)
                                    beta-tests))
                    (~down ~@outer-var-sublist ~@(conj vars wme-var)))
                  (simon-stop rsplit#)
                  (simon-stop split#)))
             `(fn ~sub-fun [~@outer-var-sublist ~wme-var]
                (when (and ~@(map #(translate-obj-test % wme-var) beta-tests))
                  (~down ~@outer-var-sublist ~@(conj vars wme-var))))))))))

(defn- unpack [x]
  (let [comparison (first x)]
    (if (> (count x) 3)
      (cons (list comparison (second x) (third x))
            (unpack (cons comparison (nthnext x 2))))
      (list x))))

(defn is-optimizable [upstream-vars]
  (fn [x] (or (and (find-any upstream-vars (second x))
                   (not (find-any upstream-vars (third x))))
              (and (find-any upstream-vars (third x))
                   (not (find-any upstream-vars (second x)))))))

(defn optimizable-tests [x comparisons upstream-vars]
  (filter (is-optimizable upstream-vars)
          (mapcat unpack (filter #(comparisons (first %)) x))))

;; Construct a standard object match node for a positive LHS match
(defn make-obj-node [rname-string rule-name net-name outer-names empty-count-name
                     x nodes vars outer-vars]
  (let [wme-var (first x)
        set-base (str (first x) "-")
        upstream-vars (set (concat vars outer-vars))
        oset (gensym set-base)
        ocur (gensym (str set-base "cur-"))
        ;; any test containing no variables from previous tests is an alpha test
        tests (group-by #(find-any upstream-vars %) (nthnext x 2))
        alpha-tests (tests false)
        base-beta-tests (tests true)
        find-tests #(optimizable-tests base-beta-tests % upstream-vars)
        ;; We handle any equality comparison between the current object
        ;; and a previous object as a hash comparison
        hash-tests (find-tests #{'=})
        ie-tests (find-tests #{'>> '<< '>>= '<<=})
        all-optimizable (concat hash-tests ie-tests)
        beta-tests (if (empty? all-optimizable)
                     base-beta-tests
                     (let [otest-set (into #{} all-optimizable)]
                       (filter #(not (otest-set %)) base-beta-tests)))
        [uhash-exp lhash-fun] (if (empty? hash-tests)
                                [nil nil]
                                (make-hash-funs
                                 (if (= (count hash-tests) 1)
                                   (first hash-tests)
                                   (vec hash-tests))
                                 upstream-vars wme-var))
        [uie-exp luie-fun comps] (if (empty? ie-tests)
                                   [nil nil nil]
                                   (make-ie-data (vec ie-tests) upstream-vars
                                                 wme-var))
        ;; A negated conjunction acts as a full nested network with its
        ;; own main function. We need to tweak the root nodes of all outer
        ;; nets before and after running the negated main function so that
        ;; we don't move past the outer node containing the nested network
        ;; when traveling upstream from the main root node. Since the main
        ;; function of a nested network will only get invoked when
        ;; changes are made to the wmes referenced in the nested
        ;; network, there can't be any new wmes in the maps "above"
        ;; the current node containing the nested network in the next outer
        ;; network.
        main-fun (if (= rule-name (last outer-names))
                   rule-name
                   (if (> (count (rest outer-names)) 1)
                     `(atom (fn []
                              (doseq [oname1# '~(rest outer-names)]
                                (set-nand-mode oname1# :sub))
                              (@~rule-name)
                              (doseq [oname2# '~(rest outer-names)]
                                (set-nand-mode oname2# :pass))))
                     `(atom (fn []
                              (set-nand-mode '~(first (rest outer-names))
                                             :sub)
                              (@~rule-name)
                              (set-nand-mode '~(first (rest outer-names))
                                             :pass)))))
        ntype (name (second x))
        alpha-name (gensym (str rname-string "-alpha-" ntype "-"))
        alpha-rem-name (gensym (str rname-string "-alpha-rem-" ntype "-"))
        op-name (gensym "op-")
        upstream-name (gensym "upstream-")
        sub-fun (gensym "subfun-")
        upstream-node (get-upstream nodes)
        outer-var-sublist (if outer-vars [outer-vars] [])
        ;; function to add a new wme to alpha memory
        add-fun (make-add-fun alpha-tests lhash-fun luie-fun comps alpha-name
                              rname-string
                              wme-var oset ocur empty-count-name rule-name
                              main-fun)
        ;; function to remove a wme from alpha memory
        rem-fun (if lhash-fun
                  (if @compile-with-debug
                    `(~alpha-rem-name
                      (debug-alpha-hash-rem-fun ~rname-string '~alpha-name ~oset
                                                ~lhash-fun ~luie-fun
                                                ~empty-count-name))
                    `(~alpha-rem-name
                      (alpha-hash-rem-fun ~oset ~lhash-fun ~luie-fun
                                          ~empty-count-name)))
                  (if @compile-with-debug
                    `(~alpha-rem-name
                      (debug-alpha-rem-fun ~rname-string '~alpha-name ~oset
                                     ~luie-fun ~empty-count-name))
                    `(~alpha-rem-name
                      (alpha-rem-fun ~oset ~luie-fun ~empty-count-name))))
        ;; function to feed alpha values to beta tests
        op-fun `(~op-name
                 ~(if uhash-exp
                    (if uie-exp
                      `(fn ~op-name [hash-val# uie-val# beta#]
                         (debugf "Running: %s" '~op-name)
                         (let [tree-map# (.get (deref ~ocur) hash-val#)]
                           (tree-map-apply
                            tree-map#
                            ~comps
                            uie-val#
                            (fn [cur#]
                              (beta# ~@(if outer-vars
                                         '[engine.runtime/*outer-vars*]
                                         [])
                                     cur#)))))
                      `(fn ~op-name [hash-val# beta#]
                         (debugf "Running: %s" '~op-name)
                         (doseq [cur# (vals (.get (deref ~ocur) hash-val#))]
                           (beta# ~@(if outer-vars
                                      '[engine.runtime/*outer-vars*]
                                      [])
                                  cur#))))
                    (if uie-exp
                      `(fn ~op-name [uie-val# beta#]
                         (debugf "Running: %s" '~op-name)
                         (let [tree-map# (.get (deref ~ocur) :ie)]
                           (tree-map-apply
                            tree-map#
                            ~comps
                            uie-val#
                            (fn [cur#]
                              (beta# ~@(if outer-vars
                                         '[engine.runtime/*outer-vars*]
                                         [])
                                     cur#)))))
                      `(fn ~op-name [beta#]
                         (debugf "Running: %s" '~op-name)
                         (doseq [cur# (vals (deref ~ocur))]
                           (beta# ~@(if outer-vars
                                      '[engine.runtime/*outer-vars*]
                                      [])
                                  cur#))))))
        ;; function called to run upstream tests before invoking downward
        ;; result function
        upstream-fun `(~upstream-name
                       ~(cond (and (empty? vars) (empty? outer-vars))
                              ;; If nothing from earlier nodes, just invoke
                              ;; the downward function
                              op-name

                              ;; No tests against upstream objects (except
                              ;; hash equality)
                              (empty? beta-tests)
                              (make-upstream-fun-no-beta-tests
                               op-name uhash-exp uie-exp ocur outer-vars vars
                               upstream-node upstream-name outer-var-sublist
                               sub-fun wme-var)

                              ;; Full generality; w/ beta tests and hashing
                              true
                              (make-upstream-fun-with-beta-tests
                               op-name uhash-exp uie-exp ocur outer-vars vars
                               beta-tests upstream-node upstream-name
                               outer-var-sublist sub-fun wme-var rname-string)))]
    [`{:type :obj-node
       :wme-type ~(second x)
       :oset (~oset (make-map))
       :ocur (~ocur (atom ~oset))
       :add ~add-fun
       :rem ~rem-fun
       :op ~op-fun
       :upstream ~upstream-fun}
     wme-var]))

;;
;; Cases
;;
;; Add a wme:
;;   when a nand is reached on the way up, run main line,
;;   for each [<wme>, <wme>, ...] at nand
;;     run nand
;; [ninst] if any <nand insts>
;;           add an entry <nand record <pos wmes>> -> <nand insts>
;;           add reference to <nand record> for each <wme> in <nand record>
;;           for each <nand inst>
;;             add <nand inst> to map for <nand record>
;;             add reference to <nand record> for each <nand inst>
;;           remove any outer insts w/ <pos wme> prefix from <nand>
;;         else
;;           call <downstream> with vars from main line
;;   when an instantiation is created (full rule, or nand),
;;     add path for wme prefixes in trie for (sub)net
;;   if <nand inst> created
;;     if matching <nand record> exists
;;       add <nand inst> to list for <nand record>
;;       add reference from <nand inst> to <nand record>
;;     else
;;       [ninst]

;; Remove a wme:
;;   for each <nand record> referenced by wme
;; [nrem] remove <nand record>
;;        remove <nand record> -> <nand insts> entry
;;        for each <nand inst> referencing <nand record>
;;          remove reference from <nand inst> to <nand record>
;;        remove references to <nand record> from each <pos wme> in <nand record>
;;   for each <inst> referenced by wme
;;     remove <inst>
;;     remove subtrie for wme
;;
;;  When <nand insts> are removed:
;;    if <nand inst> list for at least one <nand record> becomes empty
;;      mark <nand> as <removal mode>
;;      call <net fun>
;;        when nand is reached
;;        for each <nand record> with empty <nand inst> list
;;          call <downstream> with <pos wmes> from <nand inst>
;;          [nrem]
;;      set <nand> back to <add mode>
;;


;; Node utility functions

(defn collect-nands [nodes idx]
  (cond (empty? nodes) '()

        (= (:type (first nodes)) :nand-node)
        (cons [(:main-fun-name (first nodes)) idx]
              (collect-nands (rest nodes) idx))

        :else (collect-nands (rest nodes) (inc idx))))

(declare process-net)

(defn get-node-fields [sub-net field-name]
  (let [nodes (:nodes sub-net)]
    (mapcat field-name nodes)))

(defn get-all-nodes-of-type [sub-net typ]
  (let [nodes (:nodes sub-net)]
    (concat (filter (fn [node] (= (:type node) typ)) nodes)
            (mapcat #(get-all-nodes-of-type % typ)
                    (map :sub-net
                         (filter (fn [node] (= (:type node) :nand-node))
                                 nodes))))))

(defn deep-get-all-obj-nodes [sub-net]
  (get-all-nodes-of-type sub-net :obj-node))

(defn get-all-obj-nodes [sub-net]
  (filter (fn [node] (= (:type node) :obj-node)) (:nodes sub-net)))

(defn get-all-nand-nodes [sub-net]
  (filter (fn [node] (= (:type node) :nand-node)) (:nodes sub-net)))

;; Create a "nand" (i.e. negated conjunction node). It will act as a full
;; inner network except that its instantiations will be used to block matches in
;; outer networks
(defn make-nand [rname-string rule-name outer-names node nodes
                 outer-vars vars outer-neg-index]
  (let [sub-net-name (gensym "net-")
        upstream-name (gensym "upstream-")
        ;; Create the inner network
        sub-net (process-net rname-string
                             rule-name
                             (last outer-names)
                             (conj outer-names sub-net-name)
                             nil
                             (gensym "empty-count-")
                             nil
                             (rest node)
                             `[~@outer-vars ~@vars]
                             outer-neg-index)
        sub-obj-nodes (filter (fn [node] (= (:type node) :obj-node))
                              (:nodes sub-net))
        empty-count-name (:empty-count-name sub-net)
        upstream-node (get-upstream nodes)
        neg-fun (gensym "neg-")
        outer-vars-list (if outer-vars [outer-vars] [])
        upstream-fun
        ;; Four cases for nand:
        ;; a) Wme added to outermost network - pass request up the network
        ;;    and for each set of wmes that come back down, run the inner
        ;;    network and block passage if any instantiations are created
        ;; b) Wme removed from outermost network - remove any nand-records
        ;;    and instantiations from the nand that contain the wme (not shown
        ;;    as the actual work happens elsewhere)
        ;; c) Wme added to inner network - Run inner network and if any
        ;;    instantiations are created, remove any positive instantiations
        ;;    from outer networks that include the wme prefix for the nand
        ;; d) Wme removed from inner network - Remove any instantiations from
        ;;    nand records in the nand containing the wme. If any instantiation
        ;;    lists go empty, pass the matching wmes on downward.
        `(~upstream-name
          (fn ~upstream-name [down#]
            (debugf "%s Running: %s" ~rname-string '~upstream-name)
            (case (get *nand-modes* '~sub-net-name)
              ;; Case (a) - wme added to outermost network
              :pass (~upstream-node
                     (fn ~neg-fun [~@outer-vars-list ~@vars]
                       (debugf "Running: %s" '~neg-fun)
                       (binding [engine.runtime/*outer-vars*
                                 [~@outer-vars ~@vars]]
                         (@~(:main-fun-name sub-net))
                         (let [record#
                               (get-sub-nand-record
                                *nand-records*
                                '~sub-net-name engine.runtime/*outer-vars*)]
                           (debugf "%s: Pass -- looking up nand record: %s"
                                   ~rname-string
                                   (with-out-str (pprint/pprint record#)))
                           (debugf "Sub-nands: %s"
                                   (.get *nand-records*
                                         '~sub-net-name))
                           (if record#
                             (debugf
                              "%s %s/%s: Blocking:\n------\n%s------%s\n\n"
                              ~rname-string '~neg-fun '~sub-net-name
                              (with-out-str
                                (pprint/pprint engine.runtime/*outer-vars*))
                              (with-out-str (pprint/pprint record#)))
                             (do
                               (debugf
                                "%s %s/%s: Passing:\n------\n%s------\n\n"
                                ~rname-string '~neg-fun '~sub-net-name
                                (with-out-str
                                  (pprint/pprint engine.runtime/*outer-vars*)))
                               (put-sub-nand-record *nand-records*
                                                    '~sub-net-name
                                                    engine.runtime/*outer-vars*
                                                    (->Nand
                                                     '~sub-net-name
                                                     engine.runtime/*outer-vars*
                                                     (atom #{})
                                                     (atom :live)))
                               (down# ~@outer-vars-list ~@vars)))))))
              ;; Case (c) - wme added to inner "sub" network
              :sub (let [nand-records#
                         (.get ^HashMap
                               *nand-records*
                               '~sub-net-name)]
                     (doseq [nr# (vals nand-records#)]
                       (binding [engine.runtime/*outer-vars* (:wmes nr#)]
                         (@~(:main-fun-name sub-net))
                         (let [record#
                               (get-sub-nand-record
                                *nand-records*
                                '~sub-net-name engine.runtime/*outer-vars*)]
                           (debugf "%s: Sub -- looking up nand record: %s"
                                   ~rname-string
                                   (with-out-str (pprint/pprint record#)))
                           (debugf "Sub-nands: %s"
                                   (.get *nand-records* '~sub-net-name))
                           (if record#
                             (debugf
                              "%s %s/%s: Blocking:\n------\n%s------%s\n\n"
                              ~rname-string '~neg-fun '~sub-net-name
                              (with-out-str
                                (pprint/pprint engine.runtime/*outer-vars*))
                              (with-out-str (pprint/pprint record#)))
                             (do
                               (debugf
                                "%s %s/%s: Passing:\n------\n%s------\n\n"
                                ~rname-string '~neg-fun '~sub-net-name
                                (with-out-str
                                  (pprint/pprint engine.runtime/*outer-vars*)))
                               (put-sub-nand-record *nand-records*
                                                    '~sub-net-name
                                                    engine.runtime/*outer-vars*
                                                    (->Nand
                                                     '~sub-net-name
                                                     engine.runtime/*outer-vars*
                                                     (atom #{})
                                                     (atom :live)))
                               (apply down# engine.runtime/*outer-vars*)))))))
              ;; Case (d) - wme removed from inner "sub" network
              :rem (let [nand-records# *nand-records*
                         nr# ^Nand (get-sub-nand-record
                                    nand-records#
                                    '~sub-net-name engine.runtime/*outer-vars*)]
                     (debugf "%s Rem -- looking up nand record: %s - %s"
                             ~rname-string (cons '~sub-net-name
                                                 engine.runtime/*outer-vars*)
                             @(:state nr#))
                     (let [wmes# (:wmes nr#)]
                       (debugf "%s %s/%s: Releasing:\n------\n%s------\n\n"
                               ~rname-string '~neg-fun '~(last outer-names)
                               (with-out-str
                                 (pprint/pprint wmes#)))
                       (apply down# wmes#))))))]
    {:type :nand-node
     :upstream (concat upstream-fun (get-node-fields sub-net :upstream))
     :sub-empty-count-name (concat
                            `(~empty-count-name (atom ~(count sub-obj-nodes)))
                            (mapcat :sub-empty-count-name
                                    (get-all-nand-nodes sub-net)))
     :empty-count-reset (cons `(.add *empty-counts*
                                     [~empty-count-name @~empty-count-name])
                              (mapcat :empty-count-reset
                                      (get-all-nand-nodes sub-net)))
     :sub-net-names (concat `((reset! ~sub-net-name ~(:main-fun sub-net)))
                            (mapcat :sub-net-names
                                    (get-all-nand-nodes sub-net)))
     :sub-net-placeholders (concat `(~sub-net-name (atom nil))
                                   (mapcat :sub-net-placeholders
                                           (get-all-nand-nodes sub-net)))
     :net-funs (concat `((.put *net-funs* '~sub-net-name
                               ~(last outer-names)))
                       (mapcat :net-funs (get-all-nand-nodes sub-net)))
     :nand-modes (concat `((.put *nand-modes* '~sub-net-name :pass))
                         (mapcat :nand-modes (get-all-nand-nodes sub-net)))
     :sub-net sub-net
     :main-fun-name (:main-fun-name sub-net)
     :main-fun (:main-fun sub-net)
     :oset (get-node-fields sub-net :oset)
     :ocur (get-node-fields sub-net :ocur)
     :op (get-node-fields sub-net :op)
     :add (get-node-fields sub-net :add)
     :rem (get-node-fields sub-net :rem)
     :subnet-groups (let [groups (group-by :wme-type
                                           (deep-get-all-obj-nodes sub-net))]
                      (reduce
                       (fn [m k]
                         (update m k
                                 (fn [vals]
                                   (mapv #(vector (first (:add %))
                                                  (first (:rem %))
                                                  (first (:oset %)))
                                         vals))))
                       groups
                       (keys groups)))}))

;; Create a network, either outermost positive or inner negated
(defn process-net [rname-str rule-name main-name outer-names priority empty-count
                   rule-output-name rule-body outer-vars outer-neg-index]
  (loop [nodes [] vars [] net rule-body neg-index 0]
    (let [node (first net)
          ntype (node-type node)]
      (cond (nil? ntype)
            (if (empty? nodes)
              (recur nodes vars (cons '[?_s :_start] net) neg-index)
              (let [nands (vec (collect-nands nodes 0))
                    body (if priority
                           ;; Only the outermost network has a priority
                           `(fn ~(gensym "body-") [~@(and outer-vars [outer-vars])
                                                   ~@vars]
                              ;; create a positive instantiation
                              (create-instantiation
                               ~(keyword (ns-name *ns*))
                               ~(keyword rule-output-name)
                               ~priority
                               '~(last outer-names)
                               ~vars
                               '~nands
                               (fn []
                                 ~@(if @compile-with-debug
                                     `((let [split#
                                             (simon-start "body")
                                             rsplit#
                                             (simon-start
                                              ~(str rname-str "-body"))]
                                         (firing-debug ~rule-output-name ~vars)
                                         ~@(rest net)
                                         (simon-stop rsplit#)
                                         (simon-stop split#)))
                                    (rest net)))))
                           `(fn ~(gensym "neg-conj-body-")
                              ;; In an inner network
                              ;; outer vars come from surrounding networks
                              [~@(and outer-vars [outer-vars]) ~@vars]
                              (create-neg-instantiation
                               ~rname-str
                               ~outer-vars
                               ~vars
                               '~(last outer-names)
                               '~main-name
                               ~(if (> (count outer-names) 2)
                                  outer-neg-index
                                  0))))]
                {:nodes nodes
                 :vars vars
                 :empty-count-name empty-count
                 :main-fun `(fn []
                              (when (= @~empty-count 0)
                                (debugf "Running %s" '~(last outer-names))
                                (~(first (:upstream (last nodes))) ~body)))
                 :main-fun-name (last outer-names)}))

            ;; NAND node
            (or (= ntype :nand) (= ntype :not))
            (if (empty? nodes)
              (recur nodes vars (cons '[?_s :_start] net) neg-index)
              (recur (conj nodes
                           (make-nand rname-str rule-name
                                      outer-names node nodes outer-vars
                                      vars neg-index))
                     vars
                     (rest net)
                     neg-index))

            ;; Object match node
            (= (nth (str ntype) 0) \?)
            (let [[onode avar] (make-obj-node rname-str
                                              rule-name
                                              (or main-name (last outer-names))
                                              outer-names
                                              empty-count
                                              node nodes vars outer-vars)]
              (recur (conj nodes onode) (conj vars avar) (rest net)
                     (inc neg-index)))

            true {:n nodes :v vars :net net}))))

;; Extract pieces of a rule

(defn get-priority [rule]
  (let [ruletail (rest rule)]
    (cond (string? (first ruletail)) (get-priority ruletail)
          (map? (first ruletail)) (or (:priority (first ruletail)) 0)
          true 0)))

(defn get-rule-body [rule]
  (let [ruletail (rest rule)
        ruletailhead (first ruletail)]
    (if (or (string? ruletailhead) (map? ruletailhead))
      (get-rule-body ruletail)
      ruletail)))

(defn extract-network [rule]
  (let [rname-str (str (nth rule 0))
        rule-name (gensym (str rname-str "-"))
        empty-count (gensym "empty-count-")
        rule-output-name (str (ns-name *ns*) "/" rname-str)]
    (process-net rname-str rule-name nil [rule-name]
                 (get-priority rule)
                 empty-count
                 rule-output-name (get-rule-body rule) nil 0)))

(defn node-defs [node]
  (let [base-defs
        (concat
         (:oset node)
         (:ocur node)
         (:op node)
         (:upstream node)
         (:add node)
         (:rem node))]
    (if (= (:type node) :nand-node)
      (concat (:sub-empty-count-name node) base-defs)
      base-defs)))

(defn collect-single-match-vars [omatch]
  (case (first omatch)
    (:nand :not) (mapcat collect-single-match-vars (rest omatch))
    [((juxt first second) omatch)]))

(defn collect-vars-by-type [lhs]
  (if (empty? lhs)
    '()
    (concat (collect-single-match-vars (first lhs))
            (collect-vars-by-type (rest lhs)))))

(defn get-match-exp [omatch]
  (case (first omatch)
    (:nand :not) `(and ~@(mapcat get-match-exp (rest omatch)))
    (let [tests (nthrest omatch 2)]
      (if (empty? tests)
        '()
      `((and ~@tests))))))

(defn get-lhs-exp [lhs]
  (let [tests (mapcat get-match-exp lhs)]
    (if (empty? tests)
      true
      `(and ~@tests))))

(defn build-lhs-exp [rule]
  (let [body (get-rule-body rule)
        lhs (take-while (partial not= '=>) body)
        vars-by-type (collect-vars-by-type lhs)
        lhs-exp (get-lhs-exp lhs)]
    {:fun `(fn [~@(map first vars-by-type)] ~lhs-exp)
     :var-types (map second vars-by-type)}))

;; Construct a live rule instance from a network of LHS nodes and RHS body.
;; The resulting function will get invoked at engine creation time to create
;; a set of variables and functions invokable from the engine.
(defn build-rule [network]
  (let [all-nodes (filter (fn [node] (or (= (:type node) :obj-node)
                                         (= (:type node) :nand-node)))
                          (:nodes network))
        obj-nodes (filter (fn [node] (= (:type node) :obj-node))
                          (:nodes network))
        nand-nodes (filter (fn [node] (= (:type node) :nand-node))
                           (:nodes network))
        main-fun-name (:main-fun-name network)
        empty-count-name (:empty-count-name network)]
    `(fn []
       (let [~main-fun-name (atom nil)
             ~empty-count-name (atom ~(count obj-nodes))
             ~@(mapcat :sub-net-placeholders nand-nodes)
             ~@(mapcat node-defs all-nodes)]
         (.add *empty-counts* [~empty-count-name @~empty-count-name])
         ~@(mapcat :empty-count-reset nand-nodes)
         ~@(mapcat :sub-net-names nand-nodes)
         ~@(mapcat :net-funs nand-nodes)
         ~@(mapcat :nand-modes nand-nodes)
         (reset! ~main-fun-name ~(:main-fun network))
         ~(let [obj-groups (group-by :wme-type obj-nodes)]
            (apply merge-with
                   into
                   (reduce
                    (fn [m k]
                      (update m k
                              (fn [vals]
                                (mapv #(vector (first (:add %))
                                               (first (:rem %))
                                               (first (:oset %)))
                                      vals))))
                    obj-groups
                    (keys obj-groups))
                   (map :subnet-groups nand-nodes)))))))

;; Support for defining type hierarchies. If a type is specified as
;; an "ancestor" of other types, any rule that matches the ancestor type will
;; also match the descendent types.
(defn type-and-descendents [wme-type]
  (conj (filter (fn [wtype] (some #{wme-type} (ancestor-types wtype)))
                (keys @wme-type-hierarchy))
        wme-type))

(defn defancestor [wme-type-seq ancestor]
  (doseq [wme-type (mapcat
                    type-and-descendents
                    (if (vector? wme-type-seq) wme-type-seq [wme-type-seq]))]
    (reset! wme-type-hierarchy
            (assoc @wme-type-hierarchy
                   wme-type
                   (conj
                    (into (ancestor-types ancestor)
                          (ancestor-types wme-type))
                    wme-type)))))

;; Support for rule execution contexts (not currently being used -- see
;; rule tests for example)
(defmacro defcontext [{data :data before :before after :after
                       :or {data {}
                            before (constantly true)
                            after (constantly true)}}]
  `(swap! contexts #(assoc %
                           (keyword (ns-name *ns*))
                           {:data ~data :before ~before :after ~after})))

(defmacro context-value [key]
  `(~key (~(keyword (ns-name *ns*)) *context*)))


;; Macro providing 'defrule' syntax
(defmacro defrule [& body]
  (let [rule (build-rule (extract-network body))
        ruleset-name (keyword (ns-name *ns*))
        priority (get-priority body)]
    (binding [*out* *err*]
      (println (str "Compiling " (ns-name *ns*) "/" (first body))))
    (when @show-rule-bodies (pprint/pprint rule))
    `(let [ruleset# (or (@rulesets ~ruleset-name) [])
           rule-name# (str (ns-name *ns*) "/" '~(first body))]
       (swap! rulesets #(assoc % ~ruleset-name (conj ruleset# ~rule)))
       (swap! rules-by-name #(assoc % rule-name# '~(cons 'defrule body)))
       (swap! rule-left-sides #(assoc % rule-name# '~(build-lhs-exp body)))
       @rulesets)))

;; Code to construct rule functions at engine creation time
(defn alphas-by-wme-type [rule-alpha-list]
  (apply merge-with concat (map (fn [f] (f)) rule-alpha-list)))

(defn extract-alphas [rulesets]
  (reduce
   (fn [result-map rule-alpha-list]
     (let [rule-alpha-map (alphas-by-wme-type rule-alpha-list)
           ;; {:ball1 <alpha1> <alpha2>}
           funs {:alphas (update-all rule-alpha-map #(map first %))
                 :alpha-rems (update-all rule-alpha-map #(map second %))
                 :osets (update-all rule-alpha-map #(map third %))}]
       (merge-with #(merge-with concat %1 %2) result-map funs)))
   {}
   rulesets))

;; Main engine API below

(defn insert! [input-wme]
  (insert-wmes-impl [input-wme] *actions*))

(defn remove! [wme]
  (.add ^ArrayDeque *actions* [:remove wme]))

(defn collect!
  ([fun]
   (for [[_ wme] *wmes* :when (fun wme)] wme))
  ([wme-type fun]
   (for [[_ wme] *wmes*
         :when (and (some #(identical? % wme-type) (wme-types ^Wme wme))
                    (fun wme))]
     wme)))

;; Creating and running engines. A call to 'engine' with a set of
;; keywordized module names representing sets of rules. All the rule
;; modules will get loaded into the newly created engine. Then, the
;; engine can be manipulated using its command set to run rules, check
;; wme states or timing, etc.
;;
(defn engine [& modules]
  (when (< (count modules) 1)
    (throw (RuntimeException. "Engine must include at least one rule module.")))
  (binding [*current-id* (atom 0)
            ;; we initially bind values here because they need to be referenced
            ;; during rule module processing. The full engine gets created
            ;; later
            *net-funs* (make-map)
            *nand-modes* (make-map)
            *empty-counts* (make-set)]
    ;; Engine data -- at runtime, data that needs to be referenced from rules,
    ;; etc. is bound to dynamic variables that can be seen anywhere in the thread
    (let [current-id (atom 0)
          logging-set (atom #{})
          trace-set (atom nil)
          echo-firings (atom false)
          stop-before (atom nil)
          stop-after (atom nil)
          run-before (atom nil)
          run-after (atom nil)
          history (atom [])
          record (atom nil)
          ;; Rule ordering for the modules used by the engine
          inst-set-fun (let [ordering
                             (combine-orders (mapcat (fn [mod] (mod @orderings))
                                                     modules))]
                         (fn [] (make-ordered-set ordering)))
          ;; Construct all rule functions
          rsets (map (fn [mod]
                       (let [rset (@rulesets (keyword mod))]
                         (if (empty? rset)
                           (throw (RuntimeException.
                                   (str mod " contains no rules.")))
                           rset)))
                     modules)
          ;; Core data for engine runtime
          rule-alpha-map (extract-alphas rsets)
          alphas (:alphas rule-alpha-map)
          alpha-rems (:alpha-rems rule-alpha-map)
          omap (:osets rule-alpha-map)
          wmes (make-map)
          actions (make-queue)
          instantiations (make-priority-map)
          insts-by-wme-id (make-map)
          rule-insts (make-map)
          nand-records (make-map)
          nand-records-by-wme-id (make-map)
          max-identical (atom max-identical-rule-firings)
          nand-modes ^HashMap *nand-modes*
          net-funs ^HashMap *net-funs*
          empty-counts *empty-counts*
          ;; Function to configure the rule engine based on a configuration
          ;; map argument
          configure (fn [config-map]
                      (reset! record (:record config-map))
                      (when (:debug config-map)
                        (reset! logging-set (conj @logging-set :debug)))
                      (reset! echo-firings (or (:log-rule-firings config-map)
                                               (:debug config-map)))
                      (reset! trace-set (:trace-set config-map))
                      (reset! stop-before (:stop-before config-map))
                      (reset! stop-after (:stop-after config-map))
                      (reset! run-before (:run-before config-map))
                      (reset! run-after (:run-after config-map))
                      (reset! enable-perf-mon (:enable-perf-mon config-map))
                      (when-let [val (:max-repeated-firings config-map)]
                        (reset! max-identical val)))
          ;; Init the rule engine with a start action for rules w/o LHSs
          init-wmes (fn [] (insert-wmes-impl [{:type :_start}] actions))
          ;; Reset the rule engine state; done before each new "run"
          clear (fn []
                  (doseq [[_ v] omap]
                    (doseq [oset v]
                      (.clear ^HashMap oset)))
                  (.clear wmes)
                  (.clear actions)
                  (.clear instantiations)
                  (.clear insts-by-wme-id)
                  (.clear rule-insts)
                  (.clear nand-records)
                  (.clear nand-records-by-wme-id)
                  (reset! history [])
                  (doseq [[k _] nand-modes] (.put nand-modes k :pass))
                  (doseq [[ecatom val] empty-counts]
                    (reset! ecatom val))
                  (binding [*current-id* (atom 0)]
                    (init-wmes)
                    (reset! current-id @*current-id*)))
          ;; Should we record output?
          maybe-dump #(when-let [recording-file @record]
                                   (swap!
                                    history
                                    conj
                                    {:wmes
                                     (group-by
                                      :type
                                      (mapv to-map (vals *wmes*)))})
                                    (if (= recording-file true)
                                      (view @history)
                                      (dump-recording @history @rules-by-name
                                                      @rule-left-sides
                                                      recording-file)))
          ;; Main function run to process rules; used by run, run-map, run-list.
          ;; If called directly, does not clear engine between calls and can
          ;; be used to maintain intermediate state.
          cycle-action
          (fn [input-wmes]
            (binding [*current-id* (atom ^long @current-id)
                      *logging-set* ^set @logging-set
                      *trace-set* ^set @trace-set
                      *echo-firings* @echo-firings
                      *stop-before* @stop-before
                      *stop-after* @stop-after
                      *run-before* @run-before
                      *run-after* @run-after
                      *alpha-rems* alpha-rems
                      *wmes* ^HashMap wmes
                      *actions* ^ArrayDeque actions
                      *instantiations* ^TreeMap$DescendingSubMap instantiations
                      *insts-by-wme-id* ^HashMap insts-by-wme-id
                      *rule-insts* rule-insts
                      *nand-modes* nand-modes
                      *nand-records* nand-records
                      *nand-records-by-wme-id* ^HashMap nand-records-by-wme-id
                      *net-funs* ^HashFun net-funs
                      *inst-set-fun* inst-set-fun
                      *empty-counts* ^set empty-counts
                      *history* history
                      *record* record
                      *context* (into {}
                                      (map (fn [mod]
                                             (let [kmod (keyword mod)]
                                               [kmod (:data (@contexts kmod))]))
                                           modules))]
              (if @enable-perf-mon
                (SimonManager/enable)
                (SimonManager/disable))
              (cond (= @record true) (io/delete-file default-record-file true)
                    (not (nil? @record)) (io/delete-file @record true))
              ;; Beginning of real execution
              (insert-top-level-wmes input-wmes)
              (doseq [mod modules]
                (when-let [before (:before (@contexts (keyword mod)))]
                  (before)))
              (let [rule-just-run (atom nil)
                    loop-counter (atom 0)]
                ;; Main rule loop
                (loop []
                  (if (and (empty? actions) (empty? instantiations))
                    (do (reset! current-id @*current-id*)
                        (doseq [mod (reverse modules)]
                          (when-let [after (:after (@contexts (keyword mod)))]
                            (after)))
                        (maybe-dump)
                        (get-wme-state wmes))
                    (let [stop (atom false)]
                      ;; Process any changes to wmes made during last rule
                      (process-pending-wme-actions alphas actions wmes)
                      (when-let [after-fun @run-after]
                        (when @rule-just-run
                          (after-fun @rule-just-run (vals wmes))))
                      ;; Find the highest priority instantiation and execute it
                      (when-not (empty? instantiations)
                        (let [inst (get-an-instantiation instantiations)
                              rule (:rule inst)]
                          (if (= rule @rule-just-run)
                            (when (> (swap! loop-counter inc)
                                     @max-identical)
                              (throw (RuntimeException.
                                      (str "Rule: '"
                                           (subs (str rule) 1)
                                           "' is stuck in a loop."))))
                            (reset! loop-counter 0))
                          (reset! rule-just-run rule)
                          (remove-pos-instantiation inst)
                          (debugf "Running instantiation: %s"
                                  [(:rule inst) (:net-name int)
                                   (:wmes inst)])
                          (reset! stop (run-instantiation inst))))
                      (if @stop
                        (reset! current-id @*current-id*)
                        (recur))))))))]
      (init-wmes)
      (let [eng (atom nil)]
        (reset! eng
                ;; Actual engine API
                (fn ([action arg]
                     (try
                       (case action
                         (:run :run-map) (let [result (cycle-action arg)]
                                           (clear)
                                           result)
                         :run-list (do (cycle-action arg)
                                       (let [result (get-wme-list wmes)]
                                         (clear)
                                         result))
                         :cycle (cycle-action arg)
                         :configure (do (configure arg)
                                        @eng)
                         :timing (display-timing arg))
                       (catch Exception e
                         (maybe-dump)
                         (throw e))))
                  ([action]
                   (case action
                     :timing (display-timing)
                     :wmes (get-wme-state wmes)
                     :wme-list (get-wme-list wmes)))))
        (reset! current-id @*current-id*)
        @eng))))
