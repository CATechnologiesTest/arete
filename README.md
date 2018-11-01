# Arete rule engine (version: 0.6.0)
A Clojure implementation of a simple forward chaining rule engine. An
engine is created by defining rules in one or more modules and
invoking engine.core/engine on keywords defining the modules. Each
working memory element in the engine is a Clojure map. The name
"Arete" is a pun on the greek word and "a-RETE" (i.e. not RETE), since
the engine is based more on the TREAT algorithm than RETE.

Example:

*In module foo.bar:*

    (ns foo.bar
      (:require [engine.core :refer :all]))

    (defrule rule1
      [?service :service]
      =>
      (println (:name ?service)))

*In another module:*

    (ns another
      (:require [engine.core :refer :all]))

    (def eng (engine :foo.bar)) ; loads rules from the foo.bar module
    (eng :run [{:type :service :name "s1"}])

    s1
    {:service [{:type :service :name "s1"}]}

The result of invoking :run [`<wme>`...] on an engine is to insert the
specified working memory elements, run rules until no more will fire
and then return a map of wme types to sequences of wmes.

## Background
This engine was originally created to help with translating between
different container orchestration formats (Kubernetes and Docker
Compose). Both formats were changing rapidly and they don't have
particularly similar structures. Rules made it easy to express global
constraints and to avoid a lot of complicated and fragile navigation
code. Though there are multiple Java-based rule engines, they are
quite heavyweight, and are mostly oriented more toward expressing
business rules than just providing an additional programming
paradigm. The only Clojure engine we found
([Clara Rules](http://www.clara-rules.org/)) is a great tool but is aimed at
different use cases. It shares with this engine, however, the
advantage that rules can be expressed directly in a Clojure program
without any new, separate language that needs to be parsed.

Since this engine was used for translation (and not, say, cluster
management) there is not much support built in for having an engine
instance run forever while taking new inputs and processing them. It
would actually be relatively easy to do and will probably happen at
some point if there's interest.

## Install
The Arete engine is available from clojars as `[arete "0.6.0"]` or simply
download the repo, install leiningen if necessary, and run `lein
uberjar`. The main class in the uberjar is the rule viewer for
debugging. The engine itself has no command line.

## Usage
Currently the commands supported by an engine are:

* (`<engine>` :run[-map] [`<wme>`...]) - Run to completion after inserting
  wmes. After running, clear the engine state so that a subsequent
  call will encounter a fresh engine. Returns a map of wme types to
  collections of wme instances.

* (`<engine>` :run-list [`<wme>`...]) - Run to completion after inserting
  wmes. After running, clear the engine state so that a subsequent
  call will encounter a fresh engine. Returns a list of wmes.

* (`<engine>` :cycle [`<wme>`...]) - Run to completion after inserting
  wmes. After running, leave the engine alone so subsequent calls add
  to the state rather than starting fresh.

* (`<engine>` :configure {`<setting>` `<value>`, ...}) - Turn on/off various
  settings for the engine:
  * :debug true/false - Whether or not the engine should generate
    debug messages
  * :log-rule-firings true/false - Whether or not to print the names
    of rules as they fire
  * :trace-set #{`<rule name>`, ...} - Set of rules whose execution
    should be traced
  * :stop-before #{`<rule name>`, ...} - Set of rules for which the
    engine should stop executing when reached (for testing)
  * :stop-after #{`<rule name>`, ...} - Set of rules for which the
    engine should stop executing after firing (for testing)
  * :enable-perf-mon true/false - Whether or not performance
    statistics should be gathered during the run. Requires that the
    code was compiled with the NO_PERF_COMPILE environment variable
    *unset*.
  * :record `<file name>` - Record rule firings into a file for
    debugging.

* (`<engine>` :timing) - Display timing gathered by _:enable-perf-mon_.

* (`<engine>` :wmes) - Return a map of the wmes in the engine as
  returned by _run_ and _run-map_.

* (`<engine>` :wme-list) - Return a list of the wmes in the engine as
  returned by _run-list_.

There is also a separate "viewer" that can be used to step through a
recorded rule session for debugging.

## Rule Syntax
Rules are very simple. Here is the complete syntax:

```
RULE ::= '(' 'defrule' <RULE_NAME> <CONFIG_MAP>? <LHS>? '=>' <RHS> ')'

RULE_NAME ::= *string*

CONFIG_MAP ::= '{' ':priority' <PRIORITY_VALUE> '}'

PRIORITY_VALUE ::= *integer* (can be any expression returning an
integer)

LHS ::= <CONDITION>+

CONDITION ::= <MATCH> | <NAND>

MATCH ::= '[' <OBJ_VAR> <TYPE> <TEST_EXP>* ']'

NAND ::= '[' (':not' | ':nand') <CONDITION>+ ']'

OBJ_VAR ::= '?' *string*

TYPE ::= *keyword*

TEST_EXP ::= *clojure expression referencing OBJ_VAR*

RHS ::= *clojure code*

```

Here is a rule showing the possible syntax:

``` clojure
(defrule testrule
  {:priority 28}
  [?f :foo (= (:val ?f) 6)]
  [:not [?b :bar]]
  [:nand
   [?baz :baz (> (:val ?baz) (:val ?f)) (not= (rem (:val ?baz) 2) 0)]
   [?quux :quux]]
  =>
  (remove! ?f)
  (insert! {:type :result :objs (collect! :baz #(= (:val %) 100))}))
```

There are three engine operations available for use within rule right
hand sides:

1. (insert! `<wme>`) - add a wme to the engine
2. (remove! `<wme>`) - remove a wme from the engine
3. (collect! `<fun>`) | (collect! <wme type> `<fun>`) - Collect all
instances for which `<fun>` returns true, limited to a particular wme
type if the second form is used.

It's sometimes tempting to try to use "collect!" in a rule
LHS. DON'T!!! It won't work.

## User-defined Conflict Resolution
The basic conflict resolution strategy of the engine is simple
priority. However, there is a declarative means of specifying
preferences. A rule module can contain a "deforder" expression
specifying how conflicts should be resolved:

``` clojure
(deforder (:with :x) (:without :y) :oldest)
```

The expression above says that any instantiation containing a wme of
type :x should be preferred over one that does not contain an ":x" and
if neither one contains an ":x", pick the one without a ":y" over one
that does contain it. Finally, if all (or no) instantiations contain
an ":x" and all (or no) instantiations contain a ":y", pick the
instantiation that was created first. The set of currently available
checks is:

* :with `<wme type>` - Prefer instantiations containing wme of type <wme
  type>
* :without `<wme type>` - Prefer instantiations not containing wme of
  type `<wme type>`
* :newest - Prefer the most recently created instantiation
* :oldest - Prefer the least recently created instantiation
* :from-module `<module>` - Prefer an instantiation from a rule in
  `<module>` over any from other modules

## Wme Type Hierarchy
Sometimes it's useful to write rules that operate on abstract
categories of wmes that are otherwise of different types. Maybe you
want to write a rule that deals with all "shapes" instead of specific
"circles", "squares", etc. This is supported in the engine by the use
of "defancestor" expressions. The following:

``` clojure
(defancestor [:deployment :daemonset :statefulset :cronjob] :controller)
```

says that any rule that matches a ":controller" should also match a
":deployment", ":daemonset", ":statefulset", or ":cronjob". The
ancestor relationship is transitive so any ancestor of ":controller"
would also be an ancestor of its descendents.

## Rule Viewer
The rule viewer allows post-mortem debugging via a recorded
session. If you configure the engine to record:

``` clojure
(eng :configure {:record "/tmp/out"})
```

you can run the viewer against the file after the fact. Here is a
simple set of rules implementing "factorial" that we can use to
demonstrate:

``` clojure
(ns engine.factorial
  (:require [engine.core :refer :all]))

(defrule fact-base
  [?arg :factarg (<= (:value ?arg) 0)]
  =>
  (remove! ?arg)
  (insert! {:type :factor :value 1}))

(defrule fact
  [?arg :factarg (> (:value ?arg) 0)]
  =>
  (remove! ?arg)
  (insert! (update ?arg :value dec))
  (insert! {:type :factor :value (:value ?arg)}))

(defrule combine
  [?factor1 :factor]
  [?factor2 :factor (not= ?factor1 ?factor2)]
  =>
  (remove! ?factor1)
  (remove! ?factor2)
  (insert! {:type :factor :value (* (:value ?factor1) (:value ?factor2))}))

(defrule result
  [?factor :factor]
  [:not [?factor2 :factor (not= ?factor ?factor2)]]
  [:not [? :factarg]]
  =>
  (remove! ?factor)
  (insert! {:type :fact-result :value (:value ?factor)}))
```

We'll run them by hand in the repl:

    lein repl

    engine.viewer=> (require '[engine.core :as e])
    nil
    engine.viewer=> (require '[engine.factorial])
    Compiling engine.factorial/fact-base
    Compiling engine.factorial/fact
    Compiling engine.factorial/combine
    Compiling engine.factorial/result
    nil
    engine.viewer=> (def eng (e/engine :engine.factorial))
    #'engine.viewer/eng
    engine.viewer=> (eng :configure {:record "/tmp/out"})
    #object[engine.core$engine$fn__1979 0x3400db7b "engine.core$engine$fn__1979@3400db7b"]
    engine.viewer=> (eng :run [{:type :factarg :value 6}])
    {:fact-result [{:type :fact-result, :value 720}]}
    ^D

Now let's run the viewer:

    java -jar target/arete-0.6.0-standalone.jar /tmp/out

    Instantiations:
     :engine.factorial/fact (3*)

    Wmes:
     :_start (1*)
     :factarg (2*)

    (0)==>

This shows us at step 0 with one rule instantiation and two wmes (the
_start wme used to trigger rules without left hand sides and our
factorial argument). The "*" after each number indicates that the wme
or instantiation was newly created. Let's type in the number of the
argument to get a better look:

    (0)==> 2

    WME - (2):factarg

    value: 6

    (0)==>

Not _too_ much to see here since it's a very simple wme. A '?' will tell us
all our options:

    (0)==> ?
    Usage:
      '<':
          go to beginning
      '>':
          go to end
      '?':
          display this help
      '.':
          exit the viewer
      '<cr>':
      if at top level, move forward one firing; otherwise return to top level
      '<number>[,<number>]*':
      display insts or wmes with <number>s as ids
      'ar':
          display all rule firings for the run
      'b':
          back up one firing
      'e <exp>':
          evaluate expression referencing an individual wme as: :<id> and all wmes
          as :0
      'g <step id>':
          go to step number: <step id>
      'h':
          display command history
      'pi <str>':
          display partial rule instantiations for rules with name containing <str>
      'r':
          display rule firings leading to this point
      'ref <wme id>':
          display any wmes that reference the specified wme via a UUID link
      'rs <str>':
          display rule with name containing <str>
      'sc <wme id>':
          find the firing that created the specified wme
      'sd <wme id>':
          find the firing that deleted the specified wme
      'ss <str>':
          find the next firing containing a wme whose string representation includes
          <str>
      'st <type fragment>':
          find the next firing containing a wme whose type name includes the <type
          fragment>
      'sr <rule fragment>':
          find the next firing for a rule whose name includes the <rule fragment>
      'si <type fragment>':
          find the next firing whose instantiation references a wme with type
          containing the <type fragment>
      'save <filename>':
          save the history to a file (when running as ":record true")
      'w':
          display all wmes for current firing
      'w <type fragment>':
          display all wmes for current firing with types containing <type fragment>
      'ws <str>':
          display all wmes for current firing whose string representations contain
          <str>

    (0)==>

Let's see all the rules that fired:

    (0)==> ar
    0) :engine.factorial/fact
    1) :engine.factorial/fact
    2) :engine.factorial/combine
    3) :engine.factorial/fact
    4) :engine.factorial/combine
    5) :engine.factorial/fact
    6) :engine.factorial/combine
    7) :engine.factorial/fact
    8) :engine.factorial/combine
    9) :engine.factorial/fact
    10) :engine.factorial/combine
    11) :engine.factorial/fact-base
    12) :engine.factorial/combine
    13) :engine.factorial/result
    (0)==>

Now let's go to the end:

    (0)==> >

    Last Rule: :engine.factorial/result

    Wmes:
     :_start (1)
     :fact-result (66*)

    (14)==>

Notice that once you're past the initial step, the previously fired
rule is also displayed. The rest of the viewer functionality will be
clear with a bit of experimentation.

## Implementation
The engine is loosely based on the TREAT algorithm, though the
handling of negation is different (probably worse...) However, it does
correctly handle negated conjunctions. For rules without negation, the
processing is quite efficient with very little allocation (only the
instantiations and maps to hold wmes). Negation requires maintaining a
much more elaborate tracking structure. Like TREAT, no intermediate
state is saved for beta tests (other than hashes of values so that we
can avoid cross-product performance).

No attempt is made at making this a purely functional implementation;
Java maps and other data structures are used throughout for maximum
efficiency. If immutable sessions or truth maintenance are important
for your application, check out "Clara Rules" instead.

### Details
To explain how the engine works, we'll go through a briew overview and
then build up from the simplest case. Most forward chaining rule
engines use some variation of the RETE algorithm. The RETE algorithm
was originally developed based on the insight that the firing of a
forward chaining rule typically leaves most of the working data
unchanged. This suggests that it's worthwhile to precompute matches
and hold on to them between firings since most of the work will not
need to be redone. The RETE algorithm takes this perspective to the
limit by precomputing and caching everything it can including the
results of comparisons between fields of distinct objects
(i.e. joins). As it turns out, though, there is a significant amount
of bookkeeping overhead associated with maintaining precomputed
joins. TREAT (and this engine) discard join results and recompute them
as necessary. A RETE engine feeds working memory elements into the top
of a discrimination network and "rule instantiations" come out the
bottom. TREAT places working memory elements into maps and then works
from the bottom up to find matches.  Compiling the rule:

``` clojure
(defrule two-objects
  [?ball :ball (> (:radius ?ball) 10)]
  [?cube :cube (= (:side ?cube) (:radius ?ball))]
  =>
  (println "Found match: " ?ball " and " ?cube))
```

will produce one map to hold entries for each object match:

``` clojure
{1 {:type :ball :__id 1 :radius 11}
 2 {:type :ball :__id 2 :radius 12}
 3 {:type :ball :__id 3 :radius 28}}

{4 {:type :cube :__id 4 :side 11}
 5 {:type :cube :__id 5 :side 12}
 6 {:type :cube :__id 6 :side 28}}
```

and two sets of functions, one for managing the alpha maps (adding and
removing wmes) and one to manage rule matching and instantiation. The
functions that manage the maps invoke the root function from the
second set for each associated rule.

``` clojure
;; Managing maps
(defn alpha-ball-1 [wme-var]
  (when <alpha-tests-succeed>
    (when (empty? <ball-map>)
      (swap! empty-count dec))
    (put <ball-map> (:__id wme-var) wme-var)
    (reset! <cur-ball-map> {(:__id wme-var) wme-var})
    (main-fun)
    (reset! <cur-ball-map> <ball-map>)))

(defn alpha-cube-1 [wme-var]
  (let [hash-val (<hash-fun> wme-var)]
    (when (empty? <cube-map>)
      (swap! empty-count dec))
    (let [existing (or (.get <cube-map> hash-val)
                         (let [m (make-map)]
                           (.put <cube-map> hash-val m)
                           m))]
        (.put ^HashMap existing (:__id ^Wme wme-var) wme-var))
    (reset! <cur-cube-map> {(:__id wme-var) wme-var})
    (main-fun)
    (reset! <cur-cube-map> <cube-map>)))

;; Matching and instantiation
(defn op-1 [hash-val result-fun]
  (doseq [cube (vals (get @?cube-cur-2 hash-val))]
    (result-fun cube)))

(defn upstream-2 [result-fun]
  (doseq [ball (vals @?ball-cur-1)]
    (result-fun ball)))

(defn upstream-1 [result-fun]
  (upstream-2 (fn [?ball]
                (op-1 (:radius ball)
                      (fn [?cube] (result-fun ?ball ?cube))))))

(defn main-fun []
  (when (= empty-count-1 0)
    (upstream-1 (fn [?ball ?cube]
                  (create-instantiation ...)))))
```

The map functions are pseudocode because they're constructed as
closures programmatically and many of the variables they reference are
defined in the functions that create them. The two map functions look
different because the cube function uses an extra level of hashing to
allow efficient comparison of ball radii with cube sides.

When a new working memory element is added (e.g. a ball), each alpha
(map) function associated with the wme type is called and, if it
results in adding a new element, the current map for the type is
replaced with a map containing only the new element. Then, the main
function of the rule is called and it performs joint matches which
will only include the one new wme for the type (since all other cross
matches will have already been done when the other values were
added). Afterward, the map is set back to the original map plus the
new element.

Negated object matches are far more complicated and will be discussed
in detail below.

Here is a very simple rule module:

``` clojure
(ns engine.examples
  (:require [engine.core :refer :all]))

(defrule note-red-ball
  [?ball :ball (= (:color ?ball) :red)]
  =>
  (println "Found red ball: " ?ball))
```

If we set the environment variable: "SHOW_RULES" and compile the
module we can see what the actual code for the rule looks like:

``` clojure
(clojure.core/fn
 []
 (clojure.core/let
  [note-red-ball-2953
   (clojure.core/atom nil)
   empty-count-2954
   (clojure.core/atom 1)
   ?ball-2955
   (engine.runtime/make-map)
   ?ball-cur-2956
   (clojure.core/atom ?ball-2955)
   op-2959
   (clojure.core/fn
    op-2959
    [beta__2253__auto__]
    (clojure.core/doseq
     [cur__2254__auto__ (clojure.core/vals @?ball-cur-2956)]
     (beta__2253__auto__ cur__2254__auto__)))
   upstream-2960
   op-2959
   note-red-ball-alpha-ball-2957
   (engine.runtime/alpha-fun
    (clojure.core/fn
     [?ball]
     (clojure.core/and (= (:color ?ball) :red)))
    ?ball-2955
    ?ball-cur-2956
    empty-count-2954
    note-red-ball-2953)
   note-red-ball-alpha-rem-ball-2958
   (engine.runtime/alpha-rem-fun ?ball-2955 empty-count-2954)]
  (.add
   engine.runtime/*empty-counts*
   [empty-count-2954 @empty-count-2954])
  (clojure.core/reset!
   note-red-ball-2953
   (clojure.core/fn
    []
    (clojure.core/when
     (clojure.core/= @empty-count-2954 0)
     (upstream-2960
      (clojure.core/fn
       body-2962
       [?ball]
       (engine.runtime/create-instantiation
        :engine.examples
        :engine.examples/note-red-ball
        0
        'note-red-ball-2953
        [?ball]
        '[]
        (clojure.core/fn
         []
         (println "Found red ball: " ?ball))))))))
  {:ball
   [[note-red-ball-alpha-ball-2957
     note-red-ball-alpha-rem-ball-2958
     ?ball-2955]]}))
```

If you look closely, you should be able to see the functions that are
analogous to the example ones show above.

### Negation
Completely general rule conditions require the ability to express
arbitrary boolean logic. This engine provides that support in the form
of a "negated conjunction" or "nand" match. It's well known that
either "nand" or "nor" by itself is sufficient to express any boolean
logic, so we add the ability to express nested nands on left hand
sides. Consider a loan application rule preventing too many lines of
credit from being allowed against a given mortgage. Along with other
rules preventing too much leverage, we have a rule that says no more
than two lines of credit per mortage:

``` clojure
(defrule max-lines-of-credit
  [?mortgage :mortgage]
  [?loc-request :loc-request (= (:mortgage ?loc-request) (:id ?mortgage))]
  [:nand
   [?loc1 :loc (= (:mortgage ?loc1) (:id ?mortgage))]
   [?loc2 :loc
    (= (:mortgage ?loc2) (:id ?mortgage))
    (not= (:id ?loc2) (:id ?loc1))]]
  =>
  (println "Too many lines of credit against: " (:id ?mortgage)))
```

Here is the compiled rule (with debug lines removed):

``` clojure
(clojure.core/fn
 []
 (clojure.core/let
  [max-lines-of-credit-3727
   (clojure.core/atom nil)
   empty-count-3728
   (clojure.core/atom 2)
   net-3744
   (clojure.core/atom nil)
   ?mortgage-3729
   (engine.runtime/make-map)
   ?mortgage-cur-3730
   (clojure.core/atom ?mortgage-3729)
   op-3733
   (clojure.core/fn
    op-3733
    [beta__2089__auto__]
    (clojure.core/doseq
     [cur__2090__auto__ (clojure.core/vals @?mortgage-cur-3730)]
     (beta__2089__auto__ cur__2090__auto__)))
   upstream-3734
   op-3733
   max-lines-of-credit-alpha-mortgage-3731
   (engine.runtime/alpha-fun-no-tests
    ?mortgage-3729
    ?mortgage-cur-3730
    empty-count-3728
    max-lines-of-credit-3727)
   max-lines-of-credit-alpha-rem-mortgage-3732
   (engine.runtime/alpha-rem-fun ?mortgage-3729 empty-count-3728)
   ?loc-request-3736
   (engine.runtime/make-map)
   ?loc-request-cur-3737
   (clojure.core/atom ?loc-request-3736)
   op-3740
   (clojure.core/fn
    op-3740
    [hash-val__2086__auto__ beta__2087__auto__]
    (clojure.core/doseq
     [cur__2088__auto__
      (clojure.core/vals
       (.get @?loc-request-cur-3737 hash-val__2086__auto__))]
     (beta__2087__auto__ cur__2088__auto__)))
   upstream-3741
   (clojure.core/fn
    upstream-3741
    [down3743]
    (upstream-3734
     (clojure.core/fn
      upstream-3741
      [?mortgage]
      (op-3740
       (:id ?mortgage)
       (clojure.core/fn
        subfun-3742
        [?loc-request]
        (down3743 ?mortgage ?loc-request))))))
   max-lines-of-credit-alpha-loc-request-3738
   (engine.runtime/alpha-hash-fun-no-tests
    (clojure.core/fn [?loc-request] (:mortgage ?loc-request))
    ?loc-request-3736
    ?loc-request-cur-3737
    empty-count-3728
    max-lines-of-credit-3727)
   max-lines-of-credit-alpha-rem-loc-request-3739
   (engine.runtime/alpha-hash-rem-fun
    ?loc-request-3736
    (clojure.core/fn [?loc-request] (:mortgage ?loc-request))
    empty-count-3728)
   empty-count-3746
   (clojure.core/atom 2)
   ?loc1-3747
   (engine.runtime/make-map)
   ?loc2-3755
   (engine.runtime/make-map)
   ?loc1-cur-3748
   (clojure.core/atom ?loc1-3747)
   ?loc2-cur-3756
   (clojure.core/atom ?loc2-3755)
   op-3751
   (clojure.core/fn
    op-3751
    [hash-val__2086__auto__ beta__2087__auto__]
    (clojure.core/doseq
     [cur__2088__auto__
      (clojure.core/vals
       (.get @?loc1-cur-3748 hash-val__2086__auto__))]
     (beta__2087__auto__
      engine.runtime/*outer-vars*
      cur__2088__auto__)))
   op-3759
   (clojure.core/fn
    op-3759
    [hash-val__2086__auto__ beta__2087__auto__]
    (clojure.core/doseq
     [cur__2088__auto__
      (clojure.core/vals
       (.get @?loc2-cur-3756 hash-val__2086__auto__))]
     (beta__2087__auto__
      engine.runtime/*outer-vars*
      cur__2088__auto__)))
   upstream-3745
   (clojure.core/fn
    upstream-3745
    [down__2121__auto__]
    (clojure.core/case
     (clojure.core/get engine.runtime/*nand-modes* 'net-3744)
     :pass
     (upstream-3741
      (clojure.core/fn
       neg-3764
       [?mortgage ?loc-request]
       (clojure.core/binding
        [engine.runtime/*outer-vars* [?mortgage ?loc-request]]
        (@net-3744)
        (clojure.core/let
         [record__2122__auto__
          (engine.runtime/get-sub-nand-record
           engine.runtime/*nand-records*
           'net-3744
           engine.runtime/*outer-vars*)]
         (if
          record__2122__auto__
          (do
           (engine.runtime/put-sub-nand-record
            engine.runtime/*nand-records*
            'net-3744
            engine.runtime/*outer-vars*
            (engine.runtime/->Nand
             'net-3744
             engine.runtime/*outer-vars*
             (clojure.core/atom #{})
             (clojure.core/atom :live)))
           (down__2121__auto__ ?mortgage ?loc-request)))))))
     :sub
     (clojure.core/let
      [nand-records__2123__auto__
       (.get engine.runtime/*nand-records* 'net-3744)]
      (clojure.core/doseq
       [nr__2124__auto__
        (clojure.core/vals nand-records__2123__auto__)]
       (clojure.core/binding
        [engine.runtime/*outer-vars* (:wmes nr__2124__auto__)]
        (@net-3744)
        (clojure.core/let
         [record__2122__auto__
          (engine.runtime/get-sub-nand-record
           engine.runtime/*nand-records*
           'net-3744
           engine.runtime/*outer-vars*)]
         (if
          record__2122__auto__
          (do
           (engine.runtime/put-sub-nand-record
            engine.runtime/*nand-records*
            'net-3744
            engine.runtime/*outer-vars*
            (engine.runtime/->Nand
             'net-3744
             engine.runtime/*outer-vars*
             (clojure.core/atom #{})
             (clojure.core/atom :live)))
           (clojure.core/apply
            down__2121__auto__
            engine.runtime/*outer-vars*)))))))
     :rem
     (clojure.core/let
      [nand-records__2123__auto__
       engine.runtime/*nand-records*
       nr__2124__auto__
       (engine.runtime/get-sub-nand-record
        nand-records__2123__auto__
        'net-3744
        engine.runtime/*outer-vars*)]
      (clojure.core/let
       [wmes__2125__auto__ (:wmes nr__2124__auto__)]
       (clojure.core/apply down__2121__auto__ wmes__2125__auto__)))))
   upstream-3752
   (clojure.core/fn
    upstream-3752
    [down3754]
    (op-3751
     (clojure.core/let
      [[?mortgage ?loc-request] engine.runtime/*outer-vars*]
      (:id ?mortgage))
     (clojure.core/fn
      subfun-3753
      [[?mortgage ?loc-request] ?loc1]
      (down3754 [?mortgage ?loc-request] ?loc1))))
   upstream-3760
   (clojure.core/fn
    upstream-3760
    [down3762]
    (upstream-3752
     (clojure.core/fn
      upstream-3760
      [[?mortgage ?loc-request] ?loc1]
      (op-3759
       (:id ?mortgage)
       (clojure.core/fn
        subfun-3761
        [[?mortgage ?loc-request] ?loc2]
        (clojure.core/when
         (clojure.core/and (not= (:id ?loc2) (:id ?loc1)))
         (down3762 [?mortgage ?loc-request] ?loc1 ?loc2)))))))
   max-lines-of-credit-alpha-loc-3749
   (engine.runtime/alpha-hash-fun-no-tests
    (clojure.core/fn [?loc1] (:mortgage ?loc1))
    ?loc1-3747
    ?loc1-cur-3748
    empty-count-3746
    (clojure.core/atom
     (clojure.core/fn
      []
      (engine.runtime/set-nand-mode 'net-3744 :sub)
      (@max-lines-of-credit-3727)
      (engine.runtime/set-nand-mode 'net-3744 :pass))))
   max-lines-of-credit-alpha-loc-3757
   (engine.runtime/alpha-hash-fun-no-tests
    (clojure.core/fn [?loc2] (:mortgage ?loc2))
    ?loc2-3755
    ?loc2-cur-3756
    empty-count-3746
    (clojure.core/atom
     (clojure.core/fn
      []
      (engine.runtime/set-nand-mode 'net-3744 :sub)
      (@max-lines-of-credit-3727)
      (engine.runtime/set-nand-mode 'net-3744 :pass))))
   max-lines-of-credit-alpha-rem-loc-3750
   (engine.runtime/alpha-hash-rem-fun
    ?loc1-3747
    (clojure.core/fn [?loc1] (:mortgage ?loc1))
    empty-count-3746)
   max-lines-of-credit-alpha-rem-loc-3758
   (engine.runtime/alpha-hash-rem-fun
    ?loc2-3755
    (clojure.core/fn [?loc2] (:mortgage ?loc2))
    empty-count-3746)]
  (.add
   engine.runtime/*empty-counts*
   [empty-count-3728 @empty-count-3728])
  (.add
   engine.runtime/*empty-counts*
   [empty-count-3746 @empty-count-3746])
  (clojure.core/reset!
   net-3744
   (clojure.core/fn
    []
    (clojure.core/when
     (clojure.core/= @empty-count-3746 0)
     (upstream-3760
      (clojure.core/fn
       neg-conj-body-3763
       [[?mortgage ?loc-request] ?loc1 ?loc2]
       (engine.runtime/create-neg-instantiation
        "max-lines-of-credit"
        [?mortgage ?loc-request]
        [?loc1 ?loc2]
        'net-3744
        'max-lines-of-credit-3727
        0))))))
  (.put engine.runtime/*net-funs* 'net-3744 max-lines-of-credit-3727)
  (.put engine.runtime/*nand-modes* 'net-3744 :pass)
  (clojure.core/reset!
   max-lines-of-credit-3727
   (clojure.core/fn
    []
    (clojure.core/when
     (clojure.core/= @empty-count-3728 0)
     (upstream-3745
      (clojure.core/fn
       body-3765
       [?mortgage ?loc-request]
       (engine.runtime/create-instantiation
        :engine.examples
        :engine.examples/max-lines-of-credit
        0
        'max-lines-of-credit-3727
        [?mortgage ?loc-request]
        '[[net-3744 2]]
        (clojure.core/fn
         []
         (println
          "Too many lines of credit against: "
          (:id ?mortgage)))))))))
  {:mortgage
   [[max-lines-of-credit-alpha-mortgage-3731
     max-lines-of-credit-alpha-rem-mortgage-3732
     ?mortgage-3729]],
   :loc-request
   [[max-lines-of-credit-alpha-loc-request-3738
     max-lines-of-credit-alpha-rem-loc-request-3739
     ?loc-request-3736]],
   :loc
   [[max-lines-of-credit-alpha-loc-3749
     max-lines-of-credit-alpha-rem-loc-3750
     ?loc1-3747]
    [max-lines-of-credit-alpha-loc-3757
     max-lines-of-credit-alpha-rem-loc-3758
     ?loc2-3755]]}))
```

    The wmes flow into the rule this way:

    mortgage    loc  loc    *
       \         \     \   /
        \         \     \ /
         \         \     .
          \         \   /
           \         \ /
            \         .
             \       /
              \     /
               \   /
                \ /
                 * {+pass, sub, rem}
                  \
                   \

The branch going up to the right represents the nested "nand" with two
line of credit matches. The "*" is the nand node. It takes action any
time execution begins from the bottom of the graph and begins in
"pass" state. If a new mortgage wme is added, the wme is stored in the
mortgage alpha node for the rule and execution is started from the
bottom. When execution reaches the nand node (in "pass" mode) it
passes execution up the left side and does the following for any
successful upstream matches:

1. Bind *outer-vars* to all the object match variables in any outer
network. In this case, that's just one variable: ?mortgage.
2. Run the entire inner nand subnetwork and collect any resulting
negative instantiations.
3. If there are any resulting instantiations, store them in a new nand
record for the nand node keyed by the wmes; otherwise, call the
downward function with the wmes.

If an outer wme is removed, the nand node will not get triggered as
all the instantiations, nand-records, and other related data will get
removed directly without invoking the network.

If a wme is added to the inner network, we have to look for matches
with each wme combination that made it to the nand node. So, we
iterate through the nand records and, for each one:

1. Set the nand node to "sub" mode
2. Bind *outer-vars* to the wmes.
3. Run the inner network and if there are any resulting instantiations:

If there is already a nand record for the wmes, add the
instantiation to its instantiation list and stop; otherwise, create a
new nand-record, add the instantiation, and remove all positive
instantiations downstream whose wmes include the wmes present at the
nand node as a prefix.

If a wme is removed from the inner network, we remove all
instantiations and set the nand node to "rem" mode and, when control
reaches it from the bottom, we retrieve all nand records that match
and no longer have any instantiations. The wmes contained in these
nand records are passed to the downstream function.
