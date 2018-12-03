(ns engine.runtime
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [clojure.stacktrace :as st]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [flatland.ordered.set :as ordered]
            [clojure.java.io :as io])
  (:import [org.javasimon SimonManager Stopwatch]
           [java.util Map HashMap TreeSet TreeMap TreeMap$Entry
            TreeMap$DescendingSubMap Comparator ArrayDeque]
           clojure.lang.PersistentVector))

(def default-record-file "/tmp/_engine_fail_output_")

;; Make sure we don't leave any performance on the table by accidentally
;; making a call using reflection
(set! *warn-on-reflection* true)

;; All engine record definitions

;; Tracking information stored for a particular wme combination in
;; a negated conjunction node
(defrecord Nand [^clojure.lang.Keyword net-name
                 wmes
                 insts
                 ^clojure.lang.Keyword state])

;; A rule instantiation that represents a potential rule firing
(defrecord PosInst [module rule ^long priority net-name ^PersistentVector wmes
                    nands fun ^long id])

;; A rule instantiation from a nested negated conjunction
(defrecord NegInst [rule ^PersistentVector wmes net-name ^long id ^Nand nand])

;; The engine's data stores

(def ^:dynamic *logging-set* nil)
(def ^:dynamic *trace-set* nil)
(def ^:dynamic *echo-firings* nil)
(def ^:dynamic *stop-before* nil)
(def ^:dynamic *stop-after* nil)
(def ^:dynamic *run-before* nil)
(def ^:dynamic *run-after* nil)
(def ^:dynamic *alpha-rems* nil)
(def ^:dynamic *wmes* nil)
(def ^:dynamic *actions* nil)
(def ^:dynamic *instantiations* nil)
(def ^:dynamic *insts-by-wme-id* nil)
(def ^:dynamic *rule-insts* nil)
(def ^:dynamic *nand-records* nil)
(def ^:dynamic *nand-records-by-wme-id* nil)
(def ^:dynamic *nand-modes* nil)
(def ^:dynamic *net-funs* nil)
(def ^:dynamic *inst-set-fun* nil)
(def ^:dynamic *empty-counts* nil)
(def ^:dynamic *history* nil)
(def ^:dynamic *record* nil)

;; Each wme *must* contain at least __id and type
(defrecord Wme [^long __id ^clojure.lang.Keyword type])

;; Holds the set of variables bound by object matches farther up the
;; left-hand side so that they can be plugged in during execution of a
;; nested negated conjunction
(def ^:dynamic *outer-vars* nil)

;; The id of the next wme
(def ^:dynamic *current-id* (atom 0))

;; Support for running a rule execution in a "context" which provides
;; "before" and "after" functions and a map of data which is
;; accessible at runtime
(def ^:dynamic *context* (atom nil))

;; Map of rulesets so that engines can look them up when passed their
;; names as arguments
(def rulesets (atom {}))

;; Set of data contexts (w/ before and after functions) associated with rulesets
(def contexts (atom {}))

;; Rule definitions displayed by rule viewer
(def rules-by-name (atom {}))

;; Expressions representing rule left hand sides used to evaluate expressions
;; in the viewer
(def rule-left-sides (atom {}))


;; Configuration variables

(declare ppwrap)

(defn env-var-set [env-var]
  (let [env-var (System/getenv env-var)]
    (and (not (nil? env-var))
         (not= env-var ""))))

;; Whether or not to compile in support for debugging the engine
;; (not for debugging rulesets)
(def compile-with-debug (atom (env-var-set "DEBUG_COMPILE")))

;; Whether or not to include "simon" performance monitoring code -- useful
;; to find badly performing rules
(def compile-with-perf (atom (not (env-var-set "NO_PERF_COMPILE"))))

;; If true, display the clojure code generated for rules during compilation
(def show-rule-bodies (atom (env-var-set "SHOW_RULES")))

;; Whether or not to turn off ppwrap output. "ppwrap" is used to
;; display the value of any expression without interfering with normal
;; control flow. If you have a lot of ppwrap calls in your system
;; while debugging, you can turn them all off temporarily via this
;; variable.
(def silent-ppwrap (atom (env-var-set "SILENT_PPWRAP")))

;; How many times a single rule can fire consecutively before raising an
;; exception
(def max-identical-rule-firings 300)

;; Pretty-print a value during debugging but also return it so that the
;; code behaves normally
(defn ppwrap [tag val] (when-not @silent-ppwrap (pprint/pprint (list tag val)))
  val)

;; Macros used to generate engine debugging and performance output
;; when debug compilation is turned on
(defmacro debugf [& args]
  (if @compile-with-debug
    `(when (*logging-set* :debug)
       ((fn [] (printf ~@args) (println))))
    true))

(defmacro simon-start [name]
  (if @compile-with-perf
    `(.start (SimonManager/getStopwatch ~name))
    true))

(defmacro simon-stop [simon]
  (if @compile-with-perf
    `(.stop ~simon)
    true))

;; Generate a stack trace on demand
(defn get-stack-trace []
  (try (throw (RuntimeException. "STACK TRACE"))
       (catch Exception e (with-out-str (st/print-stack-trace e)))))

;; Look up a value in an engine configuration
(defn config-contains [set-or-nil val]
  (and set-or-nil (set-or-nil val)))

;; Whether or not a particular rule is currently being traced (having its
;; firings displayed)
(defn traced [name] (config-contains *trace-set* name))

(defn third [x] (nth x 2))

;; Functions to create useful Java data structures
(defn make-map ^HashMap [] (HashMap.))

(defn make-queue ^ArrayDeque [] (ArrayDeque.))

(defn make-set [] ^java.util.HashSet (java.util.HashSet.))

(defn make-priority-map ^TreeMap$DescendingSubMap [] (.descendingMap (TreeMap.)))

(defn make-ordered-set ^TreeSet [comp] (TreeSet. ^Comparator comp))

;; Get an id for a new working memory element
(defn new-id [] (swap! *current-id* inc))

;; Associate an id with a newly inserted wme and turn it into a Wme record
(defn add-id [wme]
  (let [updated (assoc wme :__id (new-id))]
    (if (= (type wme) Wme)
      updated
      (map->Wme updated))))

(defn get-sub-nand-record [^HashMap nrmap netname path]
  (debugf "get-sub-nand-record: %s" [nrmap netname path])
  (when-let [submap (.get nrmap netname)]
    (.get ^HashMap submap path)))

(defn put-sub-nand-record [^HashMap nrmap netname path val]
  (debugf "put-sub-nand-record: %s" [nrmap netname path val])
  (let [nand-map ^HashMap *nand-records-by-wme-id*]
    (doseq [wme path]
      (let [id (:__id ^Wme wme)
            wme-nand-map-entry (or (.get nand-map id) #{})]
        (.put nand-map id (conj wme-nand-map-entry val))))
    (let [submap (atom (.get nrmap netname))]
      (when-not @submap
        (reset! submap (make-map))
        (.put ^HashMap nrmap netname @submap))
      (.put ^HashMap @submap path val))))

(defn remove-sub-nand-record [^HashMap nrmap ^Nand nr]
  (debugf "remove-sub-nand-record: %s and %s" nrmap (into {} nr))
  (let [nand-map ^HashMap *nand-records-by-wme-id*
        netname (:net-name nr)
        wmes (:wmes nr)]
    (doseq [wme wmes]
      (.remove nand-map (:__id ^Wme wme)))
    (if-let [submap (.get nrmap netname)]
      (.remove ^HashMap submap wmes))))

(defmacro with-simon [tag & exp]
  (if @compile-with-perf
    `(let [split# (.start (SimonManager/getStopwatch ~tag))
           result# (do ~@exp)]
       (.stop split#)
       result#)
    `(do ~@exp)))

(defn get-prefix-ref [trie prefix]
  (get-in trie prefix))

(defn add-prefix-ref [trie prefix val]
  (with-simon "add-prefix-ref" (assoc-in trie (conj prefix 0) val)))

(defn remove-prefix-ref [trie prefix]
  (with-simon "remove-prefix-ref"
    (if (= (count prefix) 1)
      (dissoc trie (first prefix))
      (update-in trie (pop prefix) dissoc (last prefix)))))

(def orderings (atom {}))

(defn newest [inst1 inst2]
  (let [id1 (:id ^PosInst inst1) id2 (:id ^PosInst inst2)]
    (cond (> id1 id2) -1
          (< id1 id2) 1
          ;; only here because tree map compares an entry with itself when empty
          :else 0)))

(def ^HashMap order-funs (make-map))

(.put order-funs :without
      (fn [[wme-type] left right]
        `(let [lwmes# ^PersistentVector (:wmes ^PosInst ~left)
                   lmax# (count lwmes#)
                   leftval# (loop [idx# 0]
                              (cond (>= idx# lmax#) false
                                    (= (:type ^Wme
                                              (nth ^PersistentVector lwmes# idx#))
                                       ~wme-type) true
                                    :else (recur (long (inc idx#)))))
                   rwmes# ^PersistentVector (:wmes ^PosInst ~right)
                   rmax# (count rwmes#)
                   rightval# (loop [idx# 0]
                               (cond (>= idx# rmax#) false
                                     (= (:type ^Wme
                                               (nth ^PersistentVector
                                                    rwmes# idx#))
                                        ~wme-type) true
                                     :else (recur (long (inc idx#)))))]
               (cond leftval# (if rightval# 0 1)
                     rightval# -1
                     :else 0))))

(.put order-funs :with
      (fn [[wme-type] left right]
        `(let [lwmes# ^PersistentVector (:wmes ^PosInst ~left)
                   lmax# (count lwmes#)
                   leftval# (loop [idx# 0]
                              (cond (>= idx# lmax#) false
                                    (= (:type ^Wme
                                              (nth ^PersistentVector lwmes# idx#))
                                       ~wme-type) true
                                    :else (recur (long (inc idx#)))))
                   rwmes# ^PersistentVector (:wmes ^PosInst ~right)
                   rmax# (count rwmes#)
                   rightval# (loop [idx# 0]
                               (cond (>= idx# rmax#) false
                                     (= (:type ^Wme (nth ^PersistentVector
                                                         rwmes# idx#))
                                        ~wme-type) true
                                     :else (recur (long (inc idx#)))))]
               (cond leftval# (if rightval# 0 -1)
                     rightval# 1
                     :else 0))))

(.put order-funs :newest
      (fn [left right]
        `(let [left-id# (:id ^PosInst ~left)
               right-id# (:id ^PosInst ~right)]
           (cond (> left-id# right-id#) -1
                 (< left-id# right-id#) 1
                 ;; only here because tree map compares an entry with
                 ;; itself when empty
                 :else 0))))

(.put order-funs :oldest
      (fn [left right]
        `(let [left-id# (:id ^PosInst ~left)
               right-id# (:id ^PosInst ~right)]
           (cond (> left-id# right-id#) 1
                 (< left-id# right-id#) -1
                 ;; only here because tree map compares an entry with
                 ;; itself when empty
                 :else 0))))

(.put order-funs :from-module
      (fn [[module] left right]
        `(let [left-module# (:module ^PosInst ~left)
               right-module# (:module ^PosInst ~right)]
           (if (= left-module# ~module)
             (if (= right-module# ~module) 0 -1)
             (if (= right-module# ~module) 1 0)))))

(defn invalid-order-exp-error [exp]
  (throw (RuntimeException. (str "Invalid order expression: " exp))))

(defn compile-ordering-fun [exp left right]
  (cond (list? exp) (let [ofun (.get order-funs (first exp))]
                      (if ofun
                        (ofun (rest exp) left right)
                        (invalid-order-exp-error exp)))
        (keyword? exp) (let [ofun (.get order-funs exp)]
                         (if ofun
                           (ofun left right)
                           (invalid-order-exp-error exp)))
        :else (invalid-order-exp-error exp)))

(defn combine-orders-aux [funs]
  (if (empty? funs)
    newest
    (let [first-fun (first funs)
          tail (combine-orders-aux (rest funs))]
      (fn ^long [left right]
        (let [result (long (first-fun left right))]
          (case result
            (-1 1) result
            0 (tail left right)))))))

(defn combine-orders [funs]
  (let [f (combine-orders-aux funs)]
    (if @compile-with-perf
      (fn [left right]
        (with-simon "instantiation-ordering" (f left right)))
      f)))

(defn compile-ordering [funs left right]
  (cond (empty? funs) nil
        ;; so we know to just use the default ordering function directly
        (= (count funs) 1) (compile-ordering-fun (first funs) left right)
        :else (let [res (gensym "result")]
                `(let [~res (long
                             ~(compile-ordering-fun (first funs) left right))]
                   (case ~res
                     (-1 1) ~res
                     0 ~(compile-ordering (rest funs) left right)
                     (throw
                      (RuntimeException.
                       (str "Order function must return -1, 0, or 1 -- got: "
                            ~res))))))))

(def wme-type-hierarchy (atom {}))

(defn ancestor-types ^PersistentVector [wme-type]
  (or (get ^map @wme-type-hierarchy wme-type)
      (let [types (ordered/ordered-set wme-type :wme)]
        (swap! wme-type-hierarchy #(assoc % wme-type types))
        types)))

(defmacro wme-types [wme]
  `(ancestor-types (:type ^Wme ~wme)))

(defn dump-recording [history rules left-sides filename]
  (with-open [rec-file (io/writer filename)]
    (binding [*out* rec-file]
      (printf "%s\n" [history rules left-sides])
      (flush))))

(defn to-map [rec] (into {} rec))

(defn get-instantiations []
  (mapcat
   #(into [] (set/project % [:id :rule :priority :wmes]))
   (mapv #(mapv (fn [inst] (update inst :wmes (fn [wmes] (mapv to-map wmes)))) %)
         (into [] (.values ^TreeMap$DescendingSubMap *instantiations*)))))

(defn add-step [state-vector inst]
  (let [config @*record*
        filename (if (= config true)
                   default-record-file
                   config)]
    (loop [idx (dec (count state-vector)) num-matches 0]
      (cond (and (> num-matches 100)
                 (not (.exists (io/as-file filename))))
            (dump-recording state-vector @rules-by-name @rule-left-sides
                            filename)

            (and (>= idx 0)
                 (= (:rule inst) (:rule (get state-vector idx))))
            (recur (dec idx) (inc num-matches))))
    (conj state-vector
        {:instantiations
         (concat [(update (select-keys inst [:id :rule :priority :wmes])
                          :wmes
                          #(mapv to-map %))]
                 (get-instantiations))
         :wmes (let [w (group-by :type (mapv to-map (.values ^HashMap *wmes*)))]
                 (when (some (fn [[_ wmes]] (some #(instance? Wme %) wmes)) w)
                   (println (get-stack-trace)))
                 w)
         :rule (:rule inst)})))

;; Special versions of inequality operators for inequalities across
;; large numbers of wmes
(def >> >)
(def >>= >=)
(def << <)
(def <<= <=)

(defn tcomp-for [comp]
  (condp = comp
    >> >
    >>= >
    << <
    <<= <
    comp))

(defn is-inclusive? [comp]
  (or (= comp >>=) (= comp <<=)))

(defn tree-map-apply [tmap comps keys op]
  (when tmap
    (let [[key & rest-keys] keys
          [comp & rest-comps] comps
          it (.iterator
              (.values
               (.headMap ^TreeMap tmap key (is-inclusive? comp))))]
      (if rest-keys
        (loop []
          (when (.hasNext it)
            (tree-map-apply (.next it) rest-comps rest-keys op)
            (recur)))
        (loop []
          (when (.hasNext it)
            (doseq [[_ val] (.next it)] (op val))
            (recur)))))))

(defn tree-map-get [tmap comps keys]
  (let [results (atom [])]
    (tree-map-apply tmap comps keys #(swap! results conj %))
    @results))

(defn tree-map-insert [^TreeMap tmap comps keys val]
  (let [[key & rest-keys] keys]
    (if tmap
      (let [existing (.get tmap key)]
        (if existing
          (if (seq rest-keys)
            (tree-map-insert existing (rest comps) rest-keys val)
            (.put ^HashMap existing (:__id val) val))
          (if (seq rest-keys)
            (.put tmap key (tree-map-insert nil (rest comps) rest-keys val))
            (let [new-map (make-map)]
              (.put tmap key new-map)
              (.put new-map (:__id val) val))))
        tmap)
      (let [newmap (TreeMap. ^Comparator (tcomp-for (first comps)))]
        (if (seq rest-keys)
          (.put newmap key (tree-map-insert nil (rest comps) rest-keys val))
          (let [sub-map (make-map)]
              (.put newmap key sub-map)
              (.put sub-map (:__id val) val)))
        newmap))))

(defn tree-map-remove [^TreeMap tmap keys id]
  (let [[key & rest-keys] keys]
    (when tmap
      (let [existing (.get tmap key)]
        (when existing
          (if (seq rest-keys)
            (tree-map-remove existing rest-keys id)
            (.remove ^HashMap existing id))
          (when (.isEmpty ^Map existing)
            (.remove tmap key)))))))

(defn alpha-hash-fun-no-tests [hash-fun ie-fun comps ^HashMap main-hmap
                               current-hmap empty-count net-fun]
  (fn [^Wme wme-var]
    (let [hash-val (hash-fun wme-var)]
      (when (.isEmpty main-hmap)
        (reset! empty-count (dec @empty-count)))
      (let [ie-val (and ie-fun (ie-fun wme-var))]
        (let [existing (or (.get main-hmap hash-val)
                           (and (not ie-fun)
                                (let [m (make-map)]
                                  (.put main-hmap hash-val m)
                                  m)))]
          (if ie-fun
            (.put main-hmap hash-val
                  (tree-map-insert existing comps ie-val wme-var))
            (.put ^HashMap existing (:__id wme-var) wme-var)))
        (if ie-fun
          (reset! current-hmap
                  {hash-val (tree-map-insert nil comps ie-val wme-var)})
          (reset! current-hmap {hash-val {(:__id wme-var) wme-var}}))
        (@net-fun)
        (reset! current-hmap main-hmap)))))

(defn debug-alpha-hash-fun-no-tests [rname-string alpha-name hash-fun ie-fun comps
                                     ^HashMap main-hmap current-hmap
                                     empty-count net-name net-fun]
  (fn [^Wme wme-var]
    (let [split (simon-start "rules")
          asplit (simon-start "alpha")
          rulesplit (simon-start rname-string)
          hash-val (hash-fun wme-var)]
      (when (.isEmpty main-hmap)
        (reset! empty-count (dec @empty-count)))
      (debugf "Adding:\n------\n%s------\n\nto:\n------\n%s\n------\n"
              (with-out-str
                (pprint/pprint wme-var))
              (name alpha-name))
        (let [ie-val (and ie-fun (ie-fun wme-var))]
          (let [existing (or (.get main-hmap hash-val)
                             (and (not ie-fun)
                                  (let [m (make-map)]
                                    (.put main-hmap hash-val m)
                                    m)))]
            (if ie-fun
              (.put main-hmap hash-val
                    (tree-map-insert existing comps ie-val wme-var))
              (.put ^HashMap existing (:__id wme-var) wme-var)))
          (debugf "Empty count: %s" @empty-count)
          (if ie-fun
            (reset! current-hmap
                    {hash-val (tree-map-insert nil comps ie-val wme-var)})
            (reset! current-hmap {hash-val {(:__id wme-var) wme-var}}))
          (debugf "About to invoke main fun: %s" net-name)
          (simon-stop asplit)
          (@net-fun)
          (debugf "Finished invoking main fun %s" net-name)
          (reset! current-hmap main-hmap)
          (simon-stop rulesplit)
          (simon-stop split)))))

(defn alpha-fun-no-tests [ie-fun comps ^HashMap main-hmap current-map empty-count
                          net-fun]
  (fn [^Wme wme-var]
    (when (.isEmpty main-hmap) (reset! empty-count (dec @empty-count)))
    (if ie-fun
      (let [ie-val (ie-fun wme-var)]
        (.put main-hmap
              :ie
              (tree-map-insert (.get main-hmap :ie) comps ie-val wme-var))
        (reset! current-map {:ie (tree-map-insert nil comps ie-val wme-var)}))
      (do (.put main-hmap (:__id wme-var) wme-var)
          (reset! current-map {(:__id wme-var) wme-var})))
    (@net-fun)
    (reset! current-map main-hmap)))

(defn debug-alpha-fun-no-tests [rname-string alpha-name ie-fun comps
                                ^HashMap main-hmap
                                current-map empty-count net-name net-fun]
  (fn [^Wme wme-var]
    (let [split (simon-start "rules")
          asplit (simon-start "alpha")
          rulesplit (simon-start rname-string)]
      (when (.isEmpty main-hmap)
        (reset! empty-count (dec @empty-count)))
      (debugf "Adding:\n------\n%s------\n\nto:\n------\n%s\n------\n"
              (with-out-str
                (pprint/pprint wme-var))
              (name alpha-name))
      (if ie-fun
        (let [ie-val (ie-fun wme-var)]
          (.put main-hmap
                :ie
                (tree-map-insert (.get main-hmap :ie) comps ie-val wme-var))
          (reset! current-map {:ie (tree-map-insert nil comps ie-val wme-var)}))
        (do (.put main-hmap (:__id wme-var) wme-var)
            (reset! current-map {(:__id wme-var) wme-var})))
      (debugf "Empty count: %s" @empty-count)
      (debugf "About to invoke main fun: %s" net-name)
      (simon-stop asplit)
      (@net-fun)
      (debugf "Finished invoking main fun %s" net-name)
      (reset! current-map main-hmap)
      (simon-stop rulesplit)
      (simon-stop split))))

(defn alpha-hash-fun [test-fun hash-fun ie-fun comps ^HashMap main-hmap
                      current-hmap empty-count net-fun]
  (fn [^Wme wme-var]
    (let [hash-val (hash-fun wme-var)]
      (when (test-fun wme-var)
        (when (.isEmpty main-hmap)
          (reset! empty-count (dec @empty-count)))
        (let [ie-val (and ie-fun (ie-fun wme-var))]
          (let [existing (or (.get main-hmap hash-val)
                             (and (not ie-fun)
                                  (let [m (make-map)]
                                    (.put main-hmap hash-val m)
                                    m)))]
            (if ie-fun
              (.put main-hmap hash-val
                    (tree-map-insert existing comps ie-val wme-var))
              (.put ^HashMap existing (:__id wme-var) wme-var)))
          (if ie-fun
            (reset! current-hmap
                    {hash-val (tree-map-insert nil comps ie-val wme-var)})
            (reset! current-hmap {hash-val {(:__id wme-var) wme-var}}))
          (@net-fun)
          (reset! current-hmap main-hmap))))))

  (defn debug-alpha-hash-fun [rname-string alpha-name test-fun hash-fun ie-fun comps
                              ^HashMap main-hmap current-hmap empty-count net-name
                              net-fun]
    (fn [^Wme wme-var]
      (let [split (simon-start "rules")
            asplit (simon-start "alpha")
            rulesplit (simon-start rname-string)
            hash-val (hash-fun wme-var)]
        (when (test-fun wme-var)
          (when (.isEmpty main-hmap)
            (reset! empty-count (dec @empty-count)))
          (debugf "Adding:\n------\n%s------\n\nto:\n------\n%s\n------\n"
                  (with-out-str
                    (pprint/pprint wme-var))
                  (name alpha-name))
          (let [ie-val (and ie-fun (ie-fun wme-var))]
            (let [existing (or (.get main-hmap hash-val)
                               (and (not ie-fun)
                                    (let [m (make-map)]
                                      (.put main-hmap hash-val m)
                                      m)))]
              (if ie-fun
                (.put main-hmap hash-val
                      (tree-map-insert existing comps ie-val wme-var))
                (.put ^HashMap existing (:__id wme-var) wme-var)))
            (debugf "Empty count: %s" @empty-count)
            (if ie-fun
              (reset! current-hmap
                      {hash-val (tree-map-insert nil comps ie-val wme-var)})
              (reset! current-hmap {hash-val {(:__id wme-var) wme-var}}))
            (debugf "About to invoke main fun: %s" net-name)
            (simon-stop asplit)
            (@net-fun)
            (debugf "Finished invoking main fun %s" net-name)
            (reset! current-hmap main-hmap))
          (simon-stop rulesplit)
          (simon-stop split)))))

(defn alpha-fun [test-fun ie-fun comps ^HashMap main-hmap current-map empty-count
                 net-fun]
  (fn [^Wme wme-var]
      (when (test-fun wme-var)
        (when (.isEmpty main-hmap) (reset! empty-count (dec @empty-count)))
        (if ie-fun
          (let [ie-val (ie-fun wme-var)]
            (.put main-hmap
                  :ie
                  (tree-map-insert (.get main-hmap :ie) comps ie-val wme-var))
            (reset! current-map {:ie (tree-map-insert nil comps ie-val wme-var)}))
          (do (.put main-hmap (:__id wme-var) wme-var)
              (reset! current-map {(:__id wme-var) wme-var})))
        (@net-fun)
        (reset! current-map main-hmap))))

(defn debug-alpha-fun [rname-string alpha-name test-fun ie-fun comps
                       ^HashMap main-hmap
                       current-map empty-count net-name net-fun]
  (fn [^Wme wme-var]
    (let [split (simon-start "rules")
          asplit (simon-start "alpha")
          rulesplit (simon-start rname-string)]
      (when (test-fun wme-var)
        (when (.isEmpty main-hmap)
          (reset! empty-count (dec @empty-count)))
        (debugf "Adding:\n------\n%s------\n\nto:\n------\n%s\n------\n"
                (with-out-str
                  (pprint/pprint wme-var))
                (name alpha-name))
        (if ie-fun
          (let [ie-val (ie-fun wme-var)]
            (.put main-hmap
                  :ie
                  (tree-map-insert (.get main-hmap :ie) comps ie-val wme-var))
            (reset! current-map {:ie (tree-map-insert nil comps ie-val wme-var)}))
          (do (.put main-hmap (:__id wme-var) wme-var)
              (reset! current-map {(:__id wme-var) wme-var})))
        (debugf "Empty count: %s" @empty-count)
        (debugf "About to invoke main fun: %s" net-name)
        (simon-stop asplit)
        (@net-fun)
        (debugf "Finished invoking main fun %s" net-name)
        (reset! current-map main-hmap))
      (simon-stop rulesplit)
      (simon-stop split))))

(defn alpha-hash-rem-fun [^HashMap main-hmap hash-fun ie-fun empty-count]
  (fn [^Wme wme-var]
    (let [hash-val (hash-fun wme-var)
          existing (.get main-hmap hash-val)
          removed (and existing
                       (if ie-fun
                         (tree-map-remove existing (ie-fun wme-var)
                                          (:__id wme-var))
                         (.remove ^HashMap existing (:__id wme-var))))]
      (when (empty? existing)
        (.remove main-hmap hash-val))
      (when (and removed (.isEmpty main-hmap))
        (reset! empty-count (inc @empty-count))))))

(defn debug-alpha-hash-rem-fun [rname-string alpha-name ^HashMap main-hmap
                                hash-fun ie-fun empty-count]
  (fn [^Wme wme-var]
    (let [split (simon-start "rules")
          asplit (simon-start "alpha")
          rulesplit (simon-start rname-string)]
      (debugf "%s: Removing:\n------\n%s\n------\n\nfrom:\n------\n%s\n------\n"
              rname-string
              [(:type wme-var) (:__id wme-var)]
              (name alpha-name))
      (let [hash-val (hash-fun wme-var)
            existing (.get main-hmap hash-val)
            removed (and existing
                         (if ie-fun
                           (tree-map-remove existing (ie-fun wme-var)
                                          (:__id wme-var))
                         (.remove ^HashMap existing (:__id wme-var))))]
        (when (empty? existing)
          (.remove main-hmap hash-val))
        (when (and removed
                   (.isEmpty main-hmap))
          (reset! empty-count (inc @empty-count))))
      (debugf "Removed leaving: %s\n" (vec (keys main-hmap)))
      (simon-stop asplit)
      (simon-stop rulesplit)
      (simon-stop split))))

(defn alpha-rem-fun [^HashMap main-hmap ie-fun empty-count]
  (fn [^Wme wme-var]
    (when (and (if ie-fun
                 (let [submap (.get main-hmap :ie)
                       removed (tree-map-remove submap (ie-fun wme-var)
                                                (:__id wme-var))]
                   (when (and submap (.isEmpty ^TreeMap submap))
                     (.remove main-hmap :ie))
                   removed)
                 (.remove main-hmap (:__id wme-var)))
               (.isEmpty main-hmap))
      (reset! empty-count (inc @empty-count)))))

(defn debug-alpha-rem-fun [rname-string alpha-name ^HashMap main-hmap ie-fun
                           empty-count]
  (fn [^Wme wme-var]
    (let [split (simon-start "rules")
          asplit (simon-start "alpha")
          rulesplit (simon-start rname-string)]
      (debugf "%s: Removing:\n------\n%s\n------\n\nfrom:\n------\n%s\n------\n"
              rname-string
              [(get wme-var :type) (get wme-var :__id)]
              (name alpha-name))
      (when (and (if ie-fun
                   (let [submap (.get main-hmap :ie)
                         removed (tree-map-remove submap (ie-fun wme-var)
                                                  (:__id wme-var))]
                     (when (and submap (.isEmpty ^TreeMap submap))
                       (.remove main-hmap :ie))
                     removed)
                   (.remove main-hmap (:__id wme-var)))
                 (.isEmpty main-hmap))
        (reset! empty-count (inc @empty-count)))
      (debugf "Removed leaving: %s\n" (vec (keys main-hmap)))
      (simon-stop asplit)
      (simon-stop rulesplit)
      (simon-stop split))))

(defn outer-net-fun [nand-name]
  (.get ^HashMap *net-funs* nand-name))

(defn set-nand-mode [sub-net-name mode]
  (.put ^HashMap *nand-modes* sub-net-name mode))

(defn remove-nand-record [nr]
  (debugf "Removing nand record: %s" (to-map nr))
  (doseq [inst @(:insts nr)] (reset! (:nand inst) nil))
  (remove-sub-nand-record ^HashMap *nand-records* nr))

(defn remove-neg-instantiation [^NegInst inst]
  (debugf "Removing neg instantiation: %s" (to-map inst))
  (let [split (simon-start "remove-neg-instantiation")]
    (let [inst-map ^HashMap *insts-by-wme-id*
          sub-net-name (:net-name inst)]
      (doseq [wme (:wmes inst)]
        (let [id (:__id ^Wme wme)
              new-wme-inst-map-entry (dissoc (.get inst-map id) (:id inst))]
          (if (empty? new-wme-inst-map-entry)
            (.remove inst-map id)
            (.put inst-map id new-wme-inst-map-entry))))
      (when-let [nand-record ^Nand @(:nand inst)]
        (reset! (:nand inst) nil)
        (when-not (identical? @(:state nand-record) :dead)
          (let [insts (:insts nand-record)]
            (reset! insts (disj @insts inst))
            (when (empty? @insts)
              (set-nand-mode sub-net-name :rem)
              (binding [*outer-vars* (:wmes nand-record)]
                (@(outer-net-fun sub-net-name)))
              (set-nand-mode sub-net-name :pass))))))
    (simon-stop split)))

(defn add-instantiation [^PosInst inst]
  (let [inst-map ^HashMap *insts-by-wme-id*
        instantiations ^TreeMap$DescendingSubMap *instantiations*
        priority (:priority inst)
        net-name (:net-name inst)
        wmes (:wmes inst)
        rule-insts ^HashMap *rule-insts*
        entry (.get ^TreeMap$DescendingSubMap instantiations priority)]
    (doseq [wme wmes]
      (let [id (:__id ^Wme wme)
            wme-inst-map-entry (or (.get inst-map id) {})]
        (.put inst-map id (assoc wme-inst-map-entry (:id inst) inst))))
    ;; Add to insts for network
    (when-not (empty? (:nands inst))
      (let [imap (.get rule-insts net-name)
            existing (or (get (get-prefix-ref imap (:wmes inst)) 0) [])]
        (.put rule-insts net-name
              (add-prefix-ref imap (:wmes inst) (conj existing inst)))))
    (if entry
      (.add ^TreeSet entry inst)
      (let [new-set (*inst-set-fun*)]
        (.add ^TreeSet new-set inst)
        (.put instantiations priority new-set)))))

(defn remove-pos-instantiation [^PosInst inst]
  (let [split (simon-start "remove-pos-instantiation")
        inst-map ^HashMap *insts-by-wme-id*
        instantiations ^TreeMap$DescendingSubMap *instantiations*
        rule-insts ^HashMap *rule-insts*
        priority (:priority inst)
        net-name (:net-name inst)
        wmes (:wmes inst)
        entry ^TreeSet (.get instantiations priority)]
    (debugf "Removing instantiation: %s" [(:rule inst) net-name wmes])
    (with-simon "remove-instantiation-wme-loop"
      (doseq [wme wmes]
        (let [id (:__id ^Wme wme)
              new-wme-inst-map-entry (dissoc (.get inst-map id)
                                             (:id inst))]
          (if (empty? new-wme-inst-map-entry)
            (.remove inst-map id)
            (.put inst-map id new-wme-inst-map-entry)))))
    (with-simon "remove-instantiation-put-rule-insts"
      (when-not (empty? (:nands inst))
        (.put rule-insts net-name
              (remove-prefix-ref (.get rule-insts net-name) (:wmes inst)))))
    (with-simon "remove-instantiation-inst-remove"
      (when (and entry (.remove entry inst) (.isEmpty entry))
        (.remove instantiations priority)))
    (simon-stop split)))

(defn remove-instantiation [inst]
  (if (instance? NegInst inst)
    (remove-neg-instantiation inst)
    (remove-pos-instantiation inst)))

(defn remove-sub-insts [insts]
  (cond (instance? PosInst insts)
        (remove-pos-instantiation insts)

        (instance? NegInst insts)
        (remove-neg-instantiation insts)

        (map? insts)
        (doseq [val (vals insts)] (remove-sub-insts val))

        :else
        (doseq [i insts] (remove-sub-insts i))))

(defn outer-rem-fun [net-name wmes]
  (debugf "Calling outer-rem-fun: %s/%s" net-name (vec wmes))
  (with-simon "outer-rem-fun"
    (let [insts (get-prefix-ref (.get ^HashMap *rule-insts* net-name)
                                wmes)]
      (when insts
        (with-simon "remove-sub-insts"
          (remove-sub-insts insts)))))
  (with-simon "outer-nested-rem-fun"
    (when-let [insts-atom
               (:insts (get-sub-nand-record *nand-records*
                                            net-name
                                            wmes))]
      (debugf "Calling outer-rem-fun for neg insts: %s/%s -- %s" net-name wmes
              @insts-atom)
      (let [insts @insts-atom]
        (when insts
          (with-simon "remove-nested-sub-insts"
            (remove-sub-insts insts)))))))

(defn create-neg-instantiation [rname-string outer-wmes inner-wmes net-name
                                outer-name neg-index]
  (let [split (simon-start "create-neg-instantiation")]
    (debugf "Creating negative instantiation for: %s - %s"
            [net-name outer-name outer-wmes]
            [rname-string net-name outer-wmes inner-wmes])
    (let [nand-records ^HashMap *nand-records*
          inst-map ^HashMap *insts-by-wme-id*
          existing-record ^Nand (get-sub-nand-record
                                 nand-records net-name outer-wmes)
          existing-insts (and existing-record @(:insts existing-record))
          inst ^NegInst (->NegInst rname-string inner-wmes net-name (new-id)
                                   (atom nil))
          new-record ^Nand (or existing-record
                               (->Nand net-name outer-wmes (atom #{inst})
                                       (atom :live)))]
      ;; add to inst map for each contained wme
      (doseq [wme inner-wmes]
        (let [id (:__id ^Wme wme)
              wme-inst-map-entry (or (.get inst-map id) {})]
          (.put inst-map id (assoc wme-inst-map-entry (:id inst) inst))))
      (if existing-record
        (let [insts (:insts existing-record)]
          (reset! insts (conj existing-insts inst)))
        (do (debugf "Inserting nand-record: %s/%s" (conj outer-wmes net-name)
                    new-record)
            (put-sub-nand-record nand-records net-name outer-wmes new-record)))
      (reset! (:nand inst) new-record)
      (when-not (and existing-record
                     (or (not (empty? existing-insts))
                         (identical? @(:state existing-record) :dead)))
        (debugf "Calling outer-rem-fun for: %s"
                (with-out-str (pprint/pprint existing-record)))
        ;; New blocking instantiation -- remove matching mainline instantiations
        ;; and nand records for downstream nands
        (outer-rem-fun outer-name (subvec outer-wmes
                                          0
                                          (- (count outer-wmes) neg-index)))))
    (simon-stop split)))

(defn make-inst [module rule priority net-name wmes nands fun]
  (->PosInst module rule priority net-name wmes nands fun (new-id)))

(defn create-instantiation [module rule priority net-name wmes nands fun]
  (let [split (simon-start "create-instantiation")]
    (debugf "Creating instantiation: %s" [rule net-name nands wmes])
    (let [inst (make-inst module rule priority net-name wmes nands fun)]
      (add-instantiation inst))
    (simon-stop split)))

(defn get-wme-state [^HashMap wmes]
  (dissoc (group-by :type (map #(dissoc ^Wme % :__id) (.values wmes))) :_start))

(defn get-wme-list [^HashMap wmes]
  (vec (filter #(not= (:type ^Wme %) :_start)
               (map #(dissoc ^Wme % :__id) (.values wmes)))))

(defmacro firing-debug [rule-output-name vars]
  `(do
     (when *echo-firings*
       (println (str "Firing: " ~rule-output-name)))
     ~@(when @compile-with-debug
         `((when (traced ~rule-output-name)
             (pprint/pprint [~rule-output-name ~vars]))))))

(defn remove-wme-from-alphas-and-insts [^Wme wme]
  (let [id (:__id wme)
        insts ^map (.get ^HashMap *insts-by-wme-id* id)
        nand-records (.get ^HashMap *nand-records-by-wme-id* id)
        types ^PersistentVector (wme-types wme)]
    (doseq [wme-type types]
      (doseq [alpha-rem (wme-type *alpha-rems*)]
        (alpha-rem wme)))
    (doseq [nr nand-records]
      (remove-nand-record nr))
    (doseq [inst (vals insts)]
      (remove-instantiation inst))))

(defn process-pending-wme-actions [alphas ^ArrayDeque actions ^HashMap wme-map]
  (loop []
    (when-not (.isEmpty actions)
      (let [[action ^Wme wme] (.remove actions)
            types (wme-types wme)
            id (:__id wme)]
        (try
          (case action
            :add (do (.put wme-map id wme)
                     (doseq [wme-type types]
                       (doseq [addfun (wme-type alphas)] (addfun wme))))
            :remove (do
                      (remove-wme-from-alphas-and-insts wme)
                      (.remove wme-map id)))
          (catch Exception e
            (binding [*out* *err*]
              (println (str "Error processing wme: " (to-map wme)))
              (.printStackTrace e))
            (throw e))))
      (recur))))

(defn run-instantiation [^PosInst inst]
  (let [rule (:rule inst)]
    (when @*record*
      (swap! *history* #(add-step % inst)))
    (if (config-contains *stop-before* rule)
      true
      (let [nands (:nands inst)]
        (when-let [before-fun *run-before*]
          (before-fun (.values ^HashMap *wmes*)))
        (when-not (empty? nands)
          (let [nand-records *nand-records*]
            (doseq [nand nands]
              (when-let [nr ^Nand (get-sub-nand-record
                                   nand-records (first nand)
                                   (subvec (:wmes inst) 0 (second nand)))]
                (debugf "Marking: %s dead"
                        (with-out-str (pprint/pprint nr)))
                (reset! (:state nr) :dead)))))
        ((:fun inst))
        (config-contains *stop-after* rule)))))

;; Retrieve the next instantiation in priority order
(defn get-an-instantiation [^TreeMap$DescendingSubMap insts]
  (.first ^TreeSet (.get insts (.firstKey insts))))

;; Add "add" actions for a set of wmes to the action queue; they will
;; be added when we process pending actions.
(defn insert-wmes-impl [wme-list ^ArrayDeque actions]
  (doseq [input-wme wme-list]
    (.add actions [:add (add-id input-wme)])))

;; Insert initial argument wmes into the current engine for this thread
(defn insert-top-level-wmes [wme-list]
  (doseq [wme wme-list]
    (let [typ (get wme :type)]
      (if (not (keyword? typ))
        (throw (RuntimeException.
                (str "Working memory element must contain :type field: "
                     (vec wme)))))))
  (insert-wmes-impl wme-list *actions*))

;; Display the results of performance monitoring in csv format for
;; spreadsheet analysis
(defn display-timing
  ([]
   (display-timing false))
  ([keep-simons]
   (let [simons (SimonManager/getSimons
                 (proxy [org.javasimon.SimonFilter] []
                   (accept [simon] (instance? org.javasimon.Stopwatch simon))))]
     (doseq [^Stopwatch simon simons]
       (printf "%s,%s,%s,%s,%s,%s\n"
               (.getName simon)
               (.getTotal simon)
               (.getCounter simon)
               (.getMax simon)
               (.getMin simon)
               (.getMean simon))
       (when-not keep-simons
         (SimonManager/destroySimon (.getName simon)))))))
