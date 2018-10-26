(ns engine.viewer
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [clojure.stacktrace :as st]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [flatland.ordered.set :as ordered]
            [clojure.java.io :as io]
            [engine.runtime :refer :all])
  (:import [org.javasimon SimonManager Stopwatch]
           [java.util HashMap TreeSet TreeMap TreeMap$Entry
            TreeMap$DescendingSubMap Comparator ArrayDeque]
           clojure.lang.PersistentVector))

;; This file contains support for post-mortem rule debugging. If a rule engine
;; is configured to record to a file, this program can be run against the
;; file to walk through rule firings and examine wme state.

;; Match UUIDs
(def hexdigitre "[\\da-f]")
(def uuid-regexp (re-pattern (str hexdigitre "{8}-"
                                  hexdigitre "{4}-"
                                  hexdigitre "{4}-"
                                  hexdigitre "{4}-"
                                  hexdigitre "{12}")))

(defn is-uuid? [x] (and (string? x) (re-matches uuid-regexp x)))

;; Code to partially evaluate LHS expressions so we can show partial
;; matches
(defn more-specific? [seen comb]
  (let [len (count comb)]
    (some (fn [sval]
            (loop [s sval c comb]
              (cond (empty? s) (empty? c)
                    (empty? c) true
                    (= (first s) (first c)) (recur (rest s) (rest c))
                    :else false)))
          seen)))

(defn already-executed? [history end-idx rule comb]
  (loop [srecs history idx 0]
    (if (>= idx end-idx)
      false
      (let [inst (first (:instantiations (first srecs)))]
        (or (and inst
                 (= (:rule inst) rule)
                 (= (into {} (filter identity comb))
                    (into {} (:wmes inst))))
            (recur (rest srecs) (inc idx)))))))

(defn state-record-wmes [srec]
  (apply concat (vals (:wmes srec))))

(defn get-lhs [left-sides rule]
  (left-sides rule))

(defn bucketize [wmes wme-types]
  (loop [types wme-types result []]
    (if (empty? types)
      result
      (recur (rest types) (conj result ((first types) wmes))))))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn find-wme [id wmes]
  (some #(= (:__id %) id) wmes))

(defn short-wme [wme]
  (format "(%d)%s" (:__id wme) (:type wme)))

;; Evaluate a left hand side and return partial matches
(defn eval-lhs [history rules end-idx rule]
  (let [wmes (state-record-wmes (get history end-idx))
        {fun :fun var-types :var-types} (get-lhs rules rule)
        typed (group-by :type wmes)
        sorted-wme-types (bucketize typed var-types)
        combs (cart (map #(conj % nil) sorted-wme-types))]
    (loop [combs (sort-by #(count (filter identity %)) > combs)
           seen []
           matches []]
      (if (empty? combs)
        matches
        (let [comb (first combs)]
          (if (and (some identity comb)
                   (not (more-specific? seen comb))
                   (not (already-executed? history end-idx rule comb)))
            (let [eres (try (eval `(apply ~fun '~comb))
                            (catch Exception _ true))]
              (if eres
                (recur (rest combs)
                       (conj seen comb)
                       (conj matches
                             (str/join
                              ", "
                              (map short-wme
                                   (filter identity
                                           (sort-by :__id comb))))))
                (recur (rest combs) (conj seen comb) matches)))
            (recur (rest combs) seen matches)))))))

(defn out-of-bounds? [history index]
  (or (< index 0) (>= index (count history))))

(def output-width 80)

(defn too-big? [k v]
  (let [fieldlen (- output-width (+ (count (name k)) (count ": ")))]
    (> (count (str v)) fieldlen)))

;; Generate a string representing a group of wmes, instantiations, etc.
(defn group-string [grouped previous id]
  (let [new-marker #(when (every? (fn [pre] (not= (id pre) (id %))) previous) "*")
        item-str #(str (id %) (new-marker %))]
    (when-not (empty? grouped)
      (str/join "\n"
                (vec (map (fn [[key vals]]
                            (str " "
                                 key
                                 " ("
                                 (str/join ", " (map item-str vals))
                                 ")"))
                          grouped))))))

(defn instantiations-string [history index]
  (when-not (out-of-bounds? history index)
    (group-string (vec (group-by :rule (:instantiations (get history index))))
                  (if (> index 0)
                    (:instantiations (get history (dec index)))
                    [])
                  :id)))

(defn wmes-string [history index]
  (when-not (out-of-bounds? history index)
    (let [wmes-by-type (vec (:wmes (get history index)))]
    (group-string (sort wmes-by-type)
                  (if (> index 0)
                    (mapcat second (vec (:wmes (get history (dec index)))))
                    [])
                  :__id))))

(defn display-str [history index]
  (str "\n"
       (str/triml
        (str (when-let [lr (:rule (get history (dec index)))]
               (str "\n\nLast Rule: " lr))
             (when-let [insts (instantiations-string history index)]
               (str "\n\nInstantiations:\n" insts))
             (when-let [wmes (wmes-string history index)]
               (str "\n\nWmes:\n" wmes))
             "\n\n(" index ")==> "))))

(defn link-string [link history index]
  (or (when-let [srec (get history index)]
        (loop [wmes (state-record-wmes srec)]
          (when-let [wme (first wmes)]
            (if (= link (:id wme))
              (str "----> " (short-wme wme))
              (recur (rest wmes))))))
      (str "----> (" link ") <dead link>")))

(defn short-inst [inst]
  (format "(%d)%s" (:id inst) (:rule inst)))

(defn display-inst [inst rules pp]
  (when pp
    (pprint/pprint (rules (subs (str (:rule inst)) 1)))
    (printf "\n"))
  (printf "INST - %s\n\npriority: %s\n    wmes: %s\n\n"
          (short-inst inst)
          (:priority inst)
          (str/join ", " (map short-wme (sort-by :type (:wmes inst))))))

(defn field-val-fun [history index]
  (fn [k v]
    (let [base-v
          (cond (and (is-uuid? v) (not= k :id)) (link-string v history index)
                (seq? v) (str (vec v))
                (string? v) (str "\"" v "\"")
                :else (str v))]
      (if (too-big? k base-v)
        (str (str/trimr
              (str/replace
               (binding [clojure.pprint/*print-right-margin* 76]
                 (with-out-str
                   (println)
                   (pprint/pprint v)))
               #"[\n]"
               #(str % "   ")))
             "\n")
        (str base-v "\n")))))

(defn body-display-string [item history index]
  (let [field-val-rep (field-val-fun history index)]
    (cond (string? item)
          [(field-val-rep "" item)]

          (or (instance? engine.runtime.Wme item)
              (and (map? item) (:type item) (:__id item)))
          (map (fn [[k v]] (format "%s: %s" (name k) (field-val-rep k v)))
               (sort-by str (vec (dissoc item :type :__id))))

          (map? item)
          (map (fn [[k v]] (format "%s: %s" (name k) (field-val-rep k v)))
               (sort-by str (vec item)))

          :else
          [(field-val-rep "" (str item))])))

(defn empty-wme-for-disp? [wme] (and (:__id wme) (= (count wme) 2)))

(defn display-wme [history index]
  (fn [wme]
    (println
     (apply str
            "WME - "
            (short-wme wme)
            (if (empty-wme-for-disp? wme) "\n" "\n\n")
            (body-display-string wme history index)))))

;; Actions available in the viewer
(defn usage [idx]
  (pprint/cl-format
   true
   "Usage: ~%~:{  '~a':~%     ~{~<~%     ~1,81:; ~a~>~}~%~}~%(~s)==> "
   (map
    (fn [[key val]] [key (str/split val #"[\s]")])
    [["<" "go to beginning"]
     [">" "go to end"]
     ["?" "display this help"]
     ["." "exit the viewer"]
     ["<cr>"
      (str "if at top level, move forward one firing; "
           "otherwise return to top level")]
     ["<number>[,<number>]*"
      "display insts or wmes with <number>s as ids"]
     ["ar" "display all rule firings for the run"]
     ["b" "back up one firing"]
     ["e <exp>"
      (str "evaluate expression referencing an individual wme as: :<id> "
           "and all wmes as :0")]
     ["g <step id>" "go to step number: <step id>"]
     ["h" "display command history"]
     ["pi <str>" (str "display partial rule instantiations for rules "
                      "with name containing <str>")]
     ["r" "display rule firings leading to this point"]
     ["ref <wme id>"
      (str "display any wmes that reference the "
           "specified wme via a UUID link")]
     ["rs <str>" "display rule with name containing <str>"]
     ["sc <wme id>" "find the firing that created the specified wme"]
     ["sd <wme id>" "find the firing that deleted the specified wme"]
     ["ss <str>"
      (str "find the next firing containing a wme whose string "
           "representation includes <str>")]
     ["st <type fragment>"
      (str "find the next firing containing a wme whose "
           "type name includes the <type fragment>")]
     ["sr <rule fragment>"
      (str "find the next firing for a rule whose "
           "name includes the <rule fragment>")]
     ["si <type fragment>"
      (str "find the next firing whose instantiation "
           "references a wme with type containing the <type fragment>")]
     ["save <filename>"
      "save the history to a file (when running as \":record true\")"]
     ["w" "display all wmes for current firing"]
     ["w <type fragment>"
      (str "display all wmes for current firing with types "
           "containing <type fragment>")]
     ["ws <str>"
      (str "display all wmes for current firing whose string representations "
           "contain <str>")]])
   idx))

(defn fully-instantiate [x]
  (into {} (walk/postwalk #(if (seq? %) (vec %) %) x)))

(defn real-typename [typeref]
  (subs typeref 0 (dec (count typeref))))

(defn filter-fun [typeref]
  (let [typename #(name (:type %))]
    (if (.endsWith ^String typeref ".")
      (let [actual-typeref (real-typename typeref)]
        (fn [wme] (= (typename wme) actual-typeref)))
      (fn [wme] (.contains ^String (typename wme) typeref)))))

(def ^:dynamic *item-map* {})
(def ^:dynamic *wme-items* [])

(defn instantiate-exp [exp]
  (let [vals (atom {})
        changed (atom true)
        current (atom exp)]
    (loop []
      (when @changed
        (reset! changed false)
        (reset! current
                (walk/postwalk
                 (fn [x]
                   (cond (and (symbol? x)
                              (not (special-symbol? x))
                              (not (re-matches #"^.*[#]$" (name x))))
                         (cond (resolve x) x
                               (@vals x) (@vals x)
                               :else (do (reset! changed true)
                                         (print (str x " = "))
                                         (flush)
                                         (let [val (read-string
                                                    (read-line))]
                                           (swap! vals assoc x val)
                                           val)))

                         (keyword? x)
                         (let [val (try (Integer/parseInt (name x))
                                        (catch Exception _ false))]
                           (if val
                             (do (reset! changed true)
                                 (if (= val 0)
                                   'engine.core/*wme-items*
                                   `(engine.core/*item-map* ~val)))
                             x))
                         :else x))
                 @current))
        (recur)))
    @current))

;; Main viewer that provides an interactive repl
(defn view [history-spec]
  (let [[history rules left-sides] (if (string? history-spec)
                                     (with-in-str (slurp history-spec) (read))
                                     history-spec)
        index (atom 0)
        at-top (atom true)
        disp #(do (reset! at-top true)
                  (print (display-str history @index))
                  (flush))
        items (atom {})
        command-history (atom [])
        done (atom false)
        add-to-history (fn [line & actual-line]
                         (swap! command-history conj
                                [@index line (if actual-line
                                               (first actual-line)
                                               line)]))
        next-command (atom nil)
        prompt #(do (print (str "(" @index ")==> ")) (flush))]
    (doseq [state-record history]
      (doseq [inst (:instantiations state-record)]
        (swap! items #(assoc % (:id inst) inst)))
      (doseq [wme (state-record-wmes state-record)]
        (swap! items #(assoc % (:__id wme) wme))))
    (disp)
    (loop [input-line (read-line)]
      (when input-line
        (try
          (condp re-matches (str/trim input-line)
            ;; go to beginning
            #"([<]+)" :>> (fn [& _] (reset! index 0) (disp))
            ;; go to end
            #"([>]+)" :>> (fn [& _] (reset! index (dec (count history))) (disp))
            ;; show a set of wmes based on ids
            #"[\d]+([,][\s]*[\d]+)*" :>>
            #(let [first-time (atom true)]
               (do (println)
                   (let [items (vec (map (fn [num]
                                           (@items (read-string (str/trim num))))
                                         (str/split (first %) #"[,]")))]
                     (add-to-history
                      (str/join ", "
                                (map (fn [item]
                                       (str "(" (or (:__id item) (:id item)) ")"
                                            (or (:type item) "INST")))
                                     items))
                      input-line)
                     (doseq [item items]
                       (if @first-time
                         (reset! first-time false)
                         (println "----------------\n"))
                       (if (:__id item)
                         ((display-wme history @index) item)
                         (display-inst item rules (= (count items) 1)))))
                   (reset! at-top false)
                   (prompt)))
            ;; go to a specific step
            #"[g][\s]+([\d]+)" :>>
            #(do (reset! index (read-string (second %)))
                 (disp))
            ;; show history
            #"[h]" :>>
            (fn [& _]
              (loop [commands @command-history item 1]
                (when (seq commands)
                  (let [[idx disp-command _] (first commands)]
                    (println (str item ": (" idx ") - " disp-command))
                    (recur (rest commands) (inc item)))))
              (reset! at-top false)
              (prompt))
            ;; specific history
            #"[h][\s]+([^\s]+.*)" :>>
            (fn [command-index-match]
              (let [[idx _ command]
                    (@command-history
                     (dec (read-string
                           (second command-index-match))))]
                (reset! index idx)
                (reset! next-command command)))
            ;; evaluate an expression
            #"[e][\s]+([^\s]+.*)" :>>
            #(do (pprint/pprint
                  (let [wmes (state-record-wmes (get history @index))]
                    (binding [*wme-items* wmes
                              *item-map* (into {} (map (fn [w] [(:__id w) w])
                                                       wmes))]
                      (let [exp (instantiate-exp (read-string (second %)))]
                        (add-to-history (str "e " exp))
                        (walk/postwalk
                         (fn [res]
                           (if (seq? res)
                             (doall res)
                             res))
                         (eval exp))))))
                 (reset! at-top false)
                 (prompt))
            ;; show wmes
            #"[w]" :>>
            (fn [& _]
              (println)
              (let [first-time (atom true)]
                (doseq [item (sort-by
                              :type
                              (state-record-wmes (get history @index)))]
                  (if @first-time
                       (reset! first-time false)
                       (println "----------------\n"))
                  ((display-wme history @index) item)))
              (add-to-history input-line)
              (reset! at-top false)
              (prompt))
            ;; show wmes by type
            #"[w][\s]+([^\s]+)" :>>
            #(let [first-time (atom true)
                   typ (second %)]
               (println)
               (doseq [item (filter (filter-fun typ)
                                    (state-record-wmes (get history @index)))]
                 (if @first-time
                   (reset! first-time false)
                   (println "----------------\n"))
                 ((display-wme history @index) item))
               (add-to-history input-line)
               (reset! at-top false)
               (prompt))
            ;; show wmes that match a string
            #"[w][s][\s]+([^\s]+)" :>>
            #(let [first-time (atom true)
                   st (second %)]
               (println)
               (doseq [item (filter
                             (fn [wme]
                               (.contains ^String
                                          (str (fully-instantiate wme))
                                          st))
                             (state-record-wmes (get history @index)))]
                 (if @first-time
                   (reset! first-time false)
                   (println "----------------\n"))
                 ((display-wme history @index) item))
               (add-to-history input-line)
               (reset! at-top false)
               (prompt))
            ;; show wmes that reference an id
            #"[r][e][f][\s]+([\d]+)" :>>
            (fn [id-val]
              (let [id (read-string (second id-val))]
               (let [srec (get history @index)
                     wmes (state-record-wmes srec)]
                 (add-to-history input-line)
                 (if-let [ref-id (:id (@items id))]
                   (let [referring
                         (filterv #(some (fn [[k v]]
                                           (and (not= k :id) (= v ref-id)))
                                         %)
                                  wmes)]
                     (if (empty? referring)
                       (do (println "No referencing wmes")
                           (disp))
                       (do (reset! at-top false)
                           (let [first-time (atom true)]
                             (doseq [item (sort-by :type referring)]
                               (if @first-time
                                 (reset! first-time false)
                                 (println "----------------\n"))
                               ((display-wme history @index) item))))))
                   (do (println "No referencing wmes")
                       (disp))))))
            ;; show all rules that fired
            #"[a][r]" :>>
            (fn [& _]
              (loop [idx 0]
                (when (< idx (count history))
                  (let [srec (get history idx)
                        inst (first (:instantiations srec))
                        rule (:rule inst)]
                    (when rule (printf "%s) %s\n" idx rule))
                    (recur (inc idx)))))
              (add-to-history input-line)
              (reset! at-top false)
              (prompt))
            ;; show rules that fires up to this point
            #"[r]" :>>
            (fn [& _]
              (loop [idx 0]
                (when (< idx @index)
                  (let [srec (get history idx)
                        inst (first (:instantiations srec))
                        rule (:rule inst)]
                    (when rule (printf "%s) %s\n" idx rule))
                    (recur (inc idx)))))
              (add-to-history input-line)
              (reset! at-top false)
              (prompt))
            ;; display a rule whose name includes a string
            #"[r][s][\s]+([^\s]+)" :>>
            #(let [first-time (atom true)
                   st (second %)]
               (doseq [[name rule]
                       (filter (fn [[k _]] (.contains ^String k st))
                               (vec rules))]
                 (println)
                 (if @first-time
                   (reset! first-time false)
                   (println "----------------\n"))
                 (pprint/pprint rule))
               (add-to-history input-line)
               (reset! at-top false)
               (println)
               (prompt))
            ;; show partial instantiations
            #"[p][i][\s]+([^\s]+)" :>>
            #(let [first-time (atom true)
                   st (second %)]
               (doseq [[name rule]
                       (filter (fn [[k _]] (.contains ^String k st))
                               (vec rules))]
                 (let [result
                       (eval-lhs history left-sides @index name)]
                   (when-not (empty? result)
                     (println)
                     (if @first-time
                       (reset! first-time false)
                       (println "----------------\n"))
                     (doseq [res result]
                       (println (str name ":\n"))
                       (println res)))))
               (add-to-history input-line)
               (reset! at-top false)
               (println)
               (prompt))
            ;; go back one step
            #"[b]" :>> (fn [& _]
                         (when (>= @index 1) (swap! index dec))
                         (disp))
            ;; go to top
            #"" :>> (fn [& _]
                      (when (and @at-top (< @index (dec (count history))))
                        (swap! index inc))
                      (disp))
            ;; go to wme creation step
            #"[s][c][\s]+([\d]+)" :>>
            #(let [id (read-string (second %))]
               (add-to-history input-line)
               (loop [idx 0]
                 (if (>= idx (count history))
                   (println (str "No such wme: " id))
                   (let [srec (get history idx)
                         wmes (state-record-wmes srec)]
                     (if (and (find-wme id wmes)
                              (or (= idx 0)
                                  (not (find-wme
                                        id
                                        (state-record-wmes
                                         (get history (dec idx)))))))
                       (reset! index idx)
                       (recur (inc idx))))))
               (disp))
            ;; go to wme deletion step
            #"[s][d][\s]+([\d]+)" :>>
            #(let [id (read-string (second %))]
               (add-to-history input-line)
               (loop [idx @index]
                 (if (>= idx (count history))
                   (println (str "No such wme: " id))
                   (let [srec (get history idx)
                         wmes (state-record-wmes srec)]
                     (if (and (not (find-wme id wmes))
                              (or (= idx 0)
                                  (find-wme
                                   id
                                   (state-record-wmes
                                    (get history (dec idx))))))
                       (reset! index idx)
                       (recur (inc idx))))))
               (disp))
            ;; find next firing with a wme containing string
            #"[s][s][\s]+([^\s]+)" :>>
            #(let [st (second %)]
               (add-to-history input-line)
               (loop [idx @index]
                 (if (>= idx (count history))
                   (println (str "No wme containing string: " st))
                   (let [srec (get history idx)
                         wmes (state-record-wmes srec)]
                     (if (some (fn [wme]
                                 (.contains ^String
                                            (str (fully-instantiate wme))
                                            st))
                               wmes)
                       (reset! index idx)
                       (recur (inc idx))))))
               (disp))
            ;; find next firing containing wme type
            #"[s][t][\s]+([^\s]+)" :>>
            #(let [typ (second %)]
               (add-to-history input-line)
               (loop [idx @index]
                 (if (>= idx (count history))
                   (println (str "No wme of type: " (real-typename typ)))
                   (let [srec (get history idx)
                         wmes (state-record-wmes srec)]
                     (if (some (filter-fun typ) wmes)
                       (reset! index idx)
                       (recur (inc idx))))))
               (disp))
            ;; find next firing containing rule whose name includes string
            #"[s][r][\s]+([^\s]+)" :>>
            #(let [chunk (second %)]
               (add-to-history input-line)
               (loop [idx @index]
                 (if (>= idx (count history))
                   (println (str "Rule containing '" chunk "' not found."))
                   (let [srec (get history idx)
                         inst (first (:instantiations srec))
                         rule (:rule inst)]
                     (ppwrap :rule [rule (name rule) chunk])
                     (if (and rule (.contains (name rule) chunk))
                       (reset! index idx)
                       (recur (inc idx))))))
               (disp))
            ;; find next firing whose instantiation contains a wme matching
            ;; string
            #"[s][i][\s]+([^\s]+)" :>>
            #(let [typ (second %)
                   done (atom false)]
               (add-to-history input-line)
               (loop [idx @index]
                 (if (>= idx (count history))
                   (println (str "Instantiation containing '"
                                 (real-typename typ) "' not found."))
                   (let [srec (get history idx)]
                     (loop [insts (:instantiations srec)]
                       (when (not (empty? insts))
                         (if (some (filter-fun typ) (:wmes (first insts)))
                           (do (reset! index idx)
                               (reset! done true))
                           (recur (rest insts)))))
                     (when-not @done (recur (inc idx))))))
               (disp))
            ;; explicitly save to file
            #"[s][a][v][e][\s]+([^\s]+)"
            :>> #(do (dump-recording history rules left-sides (second %)) (disp))
            #"[?]" :>> (fn [& _] (usage @index) (reset! at-top false))
            #"[.]" :>> (fn [& _] (reset! done true)))
          (catch Exception e
            (printf "%s\n\n" (with-out-str (st/print-stack-trace e)))
            (disp)))
        (when-not @done
          (let [nc @next-command]
            (reset! next-command nil)
            (recur (or nc (read-line)))))))))

(def -main view)
