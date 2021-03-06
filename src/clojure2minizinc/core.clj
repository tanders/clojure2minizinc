;; !! TODO: def annotation arg for fn variable, array, and constraint (int, bool, float, set and solve are done)
;; TODO: revise with meanwhile available MiniZinc spec
;;
;; Add support for direct output to FlatZinc. See FlatZinc reference.
;; Declarations must occur in a certain order: zero or more external predicate declarations (i.e., a non-standard predicate that is supported directly by the target solver); zero or more parameter declarations; zero or more variable declarations; zero or more constraints; a solve goal. I would allow for parallel processing, and those different declarations would be somehow tagged to sort them later.
;; I would translate various existing MiniZinc statements/expressions directly into the corresponding FlatZinc, e.g., for ...
;;
;; OLD TODO: revise with FlatZinc spec at http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf
;; OK - ! Add support for search annotations as documented in the spec (but allow for more than given there)
;; OK - Document all basic constraints with that spec
;; OK - Double-check all param and var declarations are there etc.

;; ns-cheatsheet.clj: https://gist.github.com/ghoseb/287710
(ns clojure2minizinc.core
  "Clojure2minizinc provides an interface between MiniZinc and
  Clojure. The clojure2minizinc user models in Clojure constraint
  satisfaction or optimisation problems over Boolean, integer, real
  number, and/or set variables. clojure2minizinc translates them into
  MiniZinc, they are solved in the background by a compatible solver,
  and the result is read back into Clojure. clojure2minizinc code can
  be very similar to the corresponding MiniZinc code, but in addition
  the full power of Clojure is at hand."
  ;; make explicit shadowing a range of core clojure functions etc
  (:refer-clojure :exclude [> >= <= < = == != -> + - * / mod assert concat min max 
                            int float set and or not nth
                            string?
                            count range sort]) 
  (:require [clojure.core :as core]
            [clojure.spec :as spec]
            [clojure.spec.test :as spectest]
            ;; [clojure.spec.gen :as gen]
            ;; TODO: consider replacing with org.clojars.hozumi/clj-commons-exec, see Clojure Cookbook
            [clojure.java.shell :as shell]
            ;; http://clojuredocs.org/clojure_core/1.3.0/clojure.pprint
            [clojure.pprint :as pprint]
            ;; http://raynes.github.io/fs/  https://github.com/Raynes/fs
            [me.raynes.fs :as fs]
            ;; [clojure.walk :as walk] ;; not used yet
            [clojure.java.io :as io] ;; only required for testing here
            ;; https://github.com/clojure/math.combinatorics/
            ;; https://clojure.github.io/math.combinatorics/
            ;; !! Warning: WARNING: #'clojure.core/update replaced by #'clojure.math.combinatorics/update
            ;; Only loading cartesian-product? Warning remains..
            [clojure.math.combinatorics :refer [cartesian-product] :as combi
             :exclude [update]]
            ;; [clojure.math.combinatorics :as combi]
            [clojure.string :as str]
            ;; [clojure.inspector :as inspector]
            ))

;; (require '[clojure2minizinc.core :as mzn])

;;;
;;; Customization
;;;

;; Path to shell apps
;; (def *mzn2fzn* "Path to the mzn2fzn executable" "/Applications/minizinc-1.6/bin/mzn2fzn")
;; !! NOTE: should this really be a dynamic var? Cannot be updated outside a thread...
(def ^:dynamic *fd-solver* "Path to default constraint solver for finite domain (integers)" 
  "minizinc")

(def on-windows?
  "Whether this code is running on Windows (true) or another OS
  (false). For now, in case of false a UNIX incl. Mac OS is assumed."
  (str/includes?
   (str/lower-case (System/getProperty "os.name"))
   "win"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utils
;;;

;; Note: contains? does not was the name may suggest.
;; http://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure#3249401
(defn in? 
  "true if `collection` contains `elm`"
  [collection elm]  
  (some #(core/= elm %) collection))

(comment 
 (in? '(100 101 102) 101)
 (in? '(100 101 102) 1)

 (in? '(100 101 102) nil)
 (in? '(100 nil 102) nil)
 (in? '(100 false 102) false)
 )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defining the store for storing all information about a CSP
;;;

;;;
;;; Flatzinc expects that the declarations of a model occur in the following order
;;;
;;; 1. zero or more external predicate declarations (i.e., a non-standard predicate that is supported directly by the target solver);
;;; 2. zero or more parameter declarations;
;;; 3. zero or more variable declarations;
;;; 4. zero or more constraints;
;;; 5. a solve goal
;;;
;;; I add:
;;; 0. include statements
;;; 0b. predicate and function definitions
;;; 

;; Sending all constraints to a single store instead of returning them from functions like constraint allows, e.g., to store minizinc vars in arbitrary clojure data structures and freely traversing such data structure for applying constraints to these, without worrying how to collect the constraint information.
;; Note: Must be public so that other packages can indirectly write to it (macro clj2mnz includes *mzn-store* in resulting code)
(def ^{:dynamic true} ^:no-doc *mzn-store*  ; :private true
  "A thread-local store for collecting all information about a CSP."
  false
  )

(defn ^:no-doc tell-store! 
  "[Aux function] Extends *mzn-store* by the given minizinc declaration
  of the given kind (a keyword such as `:include') and returns declaration. 
  (only extends *mzn-store* at thread-local level, otherwise does nothing)."
  [kind mzn]
  (if *mzn-store*
    (do (set! *mzn-store* (assoc *mzn-store* kind (conj (kind *mzn-store*) mzn)))
        mzn)
    (throw (Exception. (str "Constraint, variable or parameter added outside clj2mzn; cannot tell `"
                            (if (core/string? mzn)
                              mzn
                              (:mzn-string mzn))
                            "`." )))))

(spec/fdef tell-store!
           :args (spec/cat :kind #{:include
                                   ;; :predicate
                                   :parameter
                                   :variable
                                   ;; :constraint
                                   ;; predicates and constraints 
                                   :other
                                   :solve
                                   :output}
                           :mzn any?))
(spectest/instrument `tell-store!)
;; (spectest/unstrument `tell-store!)


(comment  
  (binding [*mzn-store* {:include []
                         :predicate []
                         :parameter []
                         :variable []
                         :constraint []
                         :solve []
                         :output []}]
    (tell-store! :constraint 1)
    (tell-store! :constraint 2)
    (tell-store! :variable 3)
    (println *mzn-store*)
    )
  )

;; TODO: change default to nil, so I do not have to check whether it is true in include
(def ^{:dynamic true} ^:no-doc *included-files*  ; :private true
  "A thread-local store for collecting which files have already been included. Used for automatic inclusion of global constraint defs."
  false)

;; (defn- add-included-file!
;;   "Extends *included-files* by given file and tells store to include that file, but only if that file was not included already. (Only extends *included-files* at thread-local level, otherwise does nothing)."
;;   [file]
;;   (if (core/and *included-files*
;;                 (core/not (in? *included-files* file)))
;;     (do (println (format "add-included-file!: %s, %s, %s" file *included-files* *mzn-store*))
;;         (tell-store! (include file))
;;         (set! *included-files* (conj *included-files* file))
;;         file)
;;     file))

;; Extends *included-files* by given file and tells store to include that file, but only if that file was not included already. (Only extends *included-files* at thread-local level, otherwise throws exception).
(defn include 
  "Include the given file. Does automatic book keeping whether file was
  already included, and includes it only once."
  [file]
  (if *included-files*    
    (if (core/not (in? *included-files* file))
      (do ; (println (format "add-included-file!: %s, %s, %s" file *included-files* *mzn-store*))
        (tell-store! :include (format "include \"%s\";" file))
        (set! *included-files* (conj *included-files* file))
        file)
      file)
    (throw (Exception. (str "Include called outside clj2mzn; cannot include " file "." )))))

(comment
  (binding [*included-files* #{}
            *mzn-store* {:include []
                         :predicate []
                         :parameter []
                         :variable []
                         :constraint []
                         :solve []
                         :output []}]
    (include "test.mzn")
    (include (io/as-file "test2.mzn"))
    (include "test.mzn")
    (include (io/as-file "test2.mzn"))
    ;; (println *included-files*)
    (println *mzn-store*)
    )
  )



(def ^{:dynamic true} ^:no-doc *defined-predicates*  
  "A thread-local store for collecting which predicates have been defined."
  #{})




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data structure definitions 
;;; (I could perhaps only use strings, but additional explicit information important for certain types)
;;;
;;;
;;; QUESTION: Should I use plain maps instead of records? I would only need to have some type feature and define functions like aVar?, aRecord? etc.
;;; Because this code will be performance sensitive for realtime use, I best use a records according to https://cemerick.com/2011/07/05/flowchart-for-choosing-the-right-clojure-type-definition-form/
;;; 
;;; More discussion:
;;; Potential disadvantages: are records faster?
;;; Potential advantages: maps are very flexible, and literal data, well supported Clojure data type with lots of functions predefined...
;;; See also http://grokbase.com/t/gg/clojure/127py2gr5h/any-downside-of-record-compared-to-map
;;;
;;;
;;;


;; NOTE: ->aVar and map->aVar are created automatically, and it is included in the doc, because I cannot set ^:no-doc to it 
(defrecord ^:no-doc aVar [name mzn-string])

;; NOTE: I would prefer making this a private function (and also aVar? make-anArray etc.), but it is required to be public (because used in macros?) 
(defn ^:no-doc make-aVar 
  "[Aux function] Returns an aVar record."
  ([name]
   (make-aVar name nil))
  ([name mzn-string]
   (aVar. name mzn-string)))

(defn ^:no-doc aVar? 
  "Returns true if x is aVar record."
  [x]
  (core/= (type x) clojure2minizinc.core.aVar))

(comment
  (def myVar (make-aVar 'x (format "var %s: %s;" (-- 1 3) (name 'x))))
  (:name myVar)
  (:mzn-string myVar)
  (aVar? myVar)

  (map->aVar {:name 'test :mzn-string "hi there"})
  (->aVar 'test "hi there")
  
  )

;; NOTE: ->anArray and map->anArray are created automatically, and it is included in the doc, because I cannot set ^:no-doc to it 
(defrecord ^:no-doc anArray [name mzn-string boundaries])
(defn ^:private index-set->boundaries 
  "Retrieves the max and min of index-set. 

  Example:
  (index-set->boundaries (-- 0 10)) ; > {:min 0 :max 10}"
  [index-set]
  (let [coll (map read-string (clojure.string/split index-set #"\.\."))]
    {:min (core/nth coll 0) :max (core/nth coll 1)}))
(defn ^:no-doc make-anArray 
  "[Aux function] Returns an anArray record."
  [name mzn-string index-set]
  ;; NOTE: no type-checking of index-set
  (anArray. name mzn-string
            (cond 
             ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
             (aVar? index-set) (index-set->boundaries (:name index-set))
             (core/string? index-set) (index-set->boundaries index-set)
             ;; TODO: consider replacing list? with the more general seq?, just in case (to include cons's, lazy seq's etc.
             (list? index-set) (map #(index-set->boundaries
                                      (cond 
                                       ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
                                       (aVar? %) (:name %)
                                       (core/string? %) %)) 
                                    index-set))))
(defn ^:no-doc anArray? 
  "Returns true if x is anArray."
  [x]
  (core/= (type x) clojure2minizinc.core.anArray))

(comment
  ;; access boundaries of given index-set
  (index-set->boundaries (-- 0 10))
  (make-anArray 'test "this is a test" (list (-- 0 10) (-- 0 10))) 

  (def a (array (-- 0 10) :int))
  (anArray? a)

  )


(defn  ^:no-doc name-or-val
  "If arg is aVar its name is returned, otherwise x."
  [x]
  (if (core/or (aVar? x) (anArray? x))
    (:name x) x))


;; quasi forward declaration, so that fns can be used in other fns before defined
(def literal-set)
(def literal-array)

;; TODO: support more Clojure data types, which are then automatically translated into the corresponding literal MiniZinc type
;; - OK bool
;; - set of ints
;; - vector (and list) of bool/ints/floats/strings/sets of ints into array of bool/ints/floats/strings/sets of ints
;; - nested vectors/lists
;; 
;; !! Problem: How to distinguish between strings "automatically" created from more complex expressions (e.g., "x = y") and strings intended as MiniZinc string literals? Perhaps I should call quote-string (or named something like MiniZinc string) when the MiniZinc string literals are created?
;; Idea: fn string (meaning literate-string, currently called quote-string) for marking literal MiniZinc strings 
;; For convenience I may want some other easy syntax for strings later, but that will be difficult.
;; 
;; -> Once I decided on a clean design, mark difference also in tutorial 
;; Syntax for literal strings values differs:  "my string"  |  (string "my string")  
;; (detail also for nested array/vector)
;; Clojure integers, flaots and sets of integers are automatically translated into corresponding MiniZinc values
;; Clojure vectors/seqs of supported values are automatically translated into MiniZinc arrays
;;
;; !! Perhaps def fn literal that translates all Clojure values into the corresponding MiniZinc value. Internally calls literal-array, literal-set etc.
;;
;; TODO: should this function be renamed perhaps?
;; 
(defn- expr
  "Translates a Clojure value into a MiniZinc value (e.g., a string
  with a MiniZinc expression). If x is aVar or similar record, it
  returns its name. Otherwise it returns the value that corresponds to
  x (e.g., a string remains that string etc.).

  BUG: arbitrary precision Clojure values not tested."
  [x]
  ;; (pprint/pprint (list literal-tests (some #(% x) literal-tests))) 
  (cond (core/or (aVar? x) (anArray? x)) (:name x)
        (core/or (number? x)                            ; !! ratios also translated
                 (core/= (type x) java.lang.Boolean)) x  
        (core/or (core/string? x)
                 (keyword? x)
                 (symbol? x)) (name x)
        (core/set? x) (apply literal-set x)             ; !! set with any elements allowed
        ;; !! literal-array calls expr for all its elements already
        (core/or (vector? x) 
                 (seq? x)) (apply literal-array x) ;; BUG: not working for nested vectors/lists
        ;; (some #(% x) literal-tests) x
        :else (throw (Exception. 
                      (pprint/cl-format nil
                                        "expr: not allowed as literal MiniZinc expr: ~S of type ~S" 
                                        x (type x))))))

(comment

  (def myVar (make-aVar 'x (format "var %s: %s;" (-- 1 3) (name 'x))))

  (expr "myVar")
  (expr myVar)
  (expr 1)
  (expr 3.41)
  (expr :test)
  
  (expr false)
  (expr [1 2 3])
  (expr [[1.0 2.0][3.0 4.0]])
  (expr #{1 2 3})
  ;; Not really convenient, but correct results
  (expr (map string ["foo" "bar"]))
  (expr [:foo :bar]) 

  (expr [#{1 2 3} #{4 5 6}])
 
  (let [x (variable (-- 1 3))]
    (expr x))

  (let [x (variable (-- 1 3))
        y (variable (-- 1 3))
        z (variable (-- 1 3))]
    (expr [x y z]))

  ;; exception
  (expr {:test 1})


  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MiniZinc parameters (quasi constants)
;;;

;;;
;;; Destructuring of arg list with optional and keyword args using clojure.spec
;;;

(spec/def ::param-type #(#{"int" "float" "bool" "set of int"} (name %)))

(spec/def ::par-args
  (spec/cat :param-type ::param-type :more (spec/? (spec/cat :par-name symbol? :init-value (spec/? any?)))
            :keys (spec/keys* :opt-un [::ann])))

(comment 
  (spec/conform ::par-args ["set of int" 'x 1 :ann 'test])
  (spec/conform ::par-args [:int 'x 1])
  (spec/conform ::par-args [:int 'x])
  (spec/conform ::par-args [:int])
  (spec/conform ::par-args [:int 'x :ann 'test])
  (spec/conform ::par-args [:int :ann 'test])
  )


;; inspired by http://stackoverflow.com/questions/17901933/flattening-a-map-by-join-the-keys
(defn- flatten-map
  "[Aux] Expects a (nested) map and returns a flat map. Nested values use only their nested keys -- higher-level keys of nested maps are ignored."
  [form]
  (into {} (mapcat (fn [[k v]]
                     (if (map? v)
                       (flatten-map v)
                       [[k v]]))
                   form)))

(comment
  (flatten-map (spec/conform ::par-args
                             ;; args vector
                             [:int 'x 1 :ann 'test]))
)


(defn- par
  "Declares a parameter (quasi a constant).

  Arguments:

  - param-type: a string, symbol or keyword; can be int, float, bool and 
    'set of int'
  - par-name: optional par name (a symbol); default is
    a gensym-ed name
  - init-value: optional initialisation 
  - :ann : an annotation
  "
  {:arglists '([param-type :ann]
               [param-type par-name :ann]
               [param-type par-name init-value :ann])}
  [& all-args]
  (let [{:keys [param-type par-name init-value ann]} (flatten-map (spec/conform ::par-args all-args))
        par-name2 (if (core/not par-name)
                    (gensym (name param-type))
                    par-name)]
    (tell-store! :parameter
                 (make-aVar par-name2 
                            (str (name param-type) ": " (name par-name2)
                                 (when init-value (str " = " init-value))
                                 (when ann (str " :: " ann))
                                 ";")
                            ))))

(comment
  
  (par :int 'x 1 :ann 'test)
  (par :int 'x :ann 'test)
  (par :int)
  
  (par :int 'test 1)
  (par :int)

  (par :float 'x 1.0)
  (par :float 'x)
  (par :float)

  (par :bool 'x 'true)
  (par :bool 'x)
  (par :bool)

  (par "set of int" 'MySet (-- 1 'max))

  (:mzn-string (par :int 'x 1))

  )


(defn int 
  "Declares an initeger parameter (quasi a constant).

  Arguments:

  - par-name: optional par name (a symbol); default is
    a gensym-ed name
  - init-value: optional initialisation 
  - :ann : an annotation

  Note: use [[variable]] for creating an integer variable."
  {:arglists '([:ann]
               [par-name :ann]
               [par-name init-value :ann])}
  [& all-args]
  (apply par :int all-args))

(defn float 
  "Declares a float parameter (quasi a constant).

  Arguments:

  - par-name: optional par name (a symbol); default is
    a gensym-ed name
  - init-value: optional initialisation 
  - :ann : an annotation

  Note: use [[variable]] for creating a float variable."
  {:arglists '([:ann]
               [par-name :ann]
               [par-name init-value :ann])}
  [& all-args]
  (apply par :float all-args))

(defn bool
  "Declares a bool parameter (quasi a constant).

  Arguments:

  - par-name: optional par name (a symbol); default is
    a gensym-ed name
  - init-value: optional initialisation 
  - :ann : an annotation

  Note: use [[variable]] for creating a bool variable."
  {:arglists '([:ann]
               [par-name :ann]
               [par-name init-value :ann])}
  [& all-args]
  (apply par :bool all-args))


(defn string?
  "Returns true if x is a MiniZinc string."
  [x]
  ;; x is a string, surrounded by double quotes, and there are no other double quotes inside the string. 
  (core/and (core/string? x)
            (core/= (first x) \")
            (core/empty? (filter #(core/= % \")
                              (butlast (drop 1 x))))
            (core/= (last x) \")))

(comment
  (string? "\"test\"") ; true
  (string? "test") ; false
  (string? "\"te\"st\"") ; false
  (string? 1) ; false
  )


(defn string 
  "Creates a MiniZinc string."
  [x]
  (if (clojure2minizinc.core/string? x)
    x
    (cond (core/string? x) (str "\"" x "\"")
          (core/or (keyword? x) 
                   (symbol? x)) (str "\"" (name x) "\"")
                   :else (throw (Exception. 
                                 (pprint/cl-format nil
                                                   "string: not allowed as MiniZinc string: ~S" x))))))

(defn literal-set
  "Specifies a set of explicitly given integers (can be MiniZinc expressions) as elements."
  [& exprs]
  (format "{%s}" (apply str (interpose ", " (core/sort (map expr exprs))))))

(comment
  (literal-set 1 2 3)
  ;; Minor BUG: order of set elements scrambled. This is used in expr for Clojure sets -- can this be a problem? 
  (apply literal-set #{1 2 3})
  )


(defn literal
  "Translates Clojure values into corresponding MiniZinc strings. Supported Clojure types are Booleans, integers, floats, strings, sets of ints, and vectors of above values."
  [x]
  (cond  (core/or (core/boolean? x)
                  (core/int? x)
                  (core/float? x)) (str x)
         (core/string? x) (string x)
         (core/set? x) (apply literal-set x)
         (core/vector? x) (apply literal-array (map literal x))
         :else (throw (Exception. 
                       (pprint/cl-format nil
                                         "literal: not allowed as literal MiniZinc expr: ~S of type ~S" 
                                         x (type x))))))
  

(comment
  (literal true)
  (literal 42)
  (literal 3.14)
  (literal "test")
  (literal #{1 2 3})
  (literal [1 2 3])
  (literal [1.1 2.2])
  (literal ["foo" "bar"])
  (literal [true false])

  ;; exception
  (literal {:test 1})
  )


;; see http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf p. 4
;; TODO: add distinction between parameter and variable declaration
;; etc -- in short, cover all type-inst variants in the flatzinc-spec, see link above
(defn set
  "Declares a set of integers parameter (quasi a constant).

  Arguments:

  - par-name: optional par name (a symbol); default is a gensym-ed name
  - init-value: optional initialisation, a range or a set
  - :ann : an annotation

  Note: use [[variable]] for creating a set variable.

  Examples: 

  ; A set of integers with automatically generated name and no specified value
  (set)
  ; A named integer set
  (set 'MySet)
  ; A named set of integers with the given range
  (set 'MySet (-- 1 10))
  ; A named set of integers with the given value
  (set 'MySet #{1 3 5})"
  {:arglists '([:ann]
               [par-name :ann]
               [par-name init-value :ann])}
  [& all-args]
  (let [{:keys [param-type par-name init-value ann]} (flatten-map (spec/conform ::par-args
                                                                                (core/cons "set of int" all-args)))]
    (par "set of int"
         (if-not par-name (gensym "Set") par-name)
         (cond (core/string? init-value) init-value
               (core/set? init-value) (apply literal-set init-value))
         :ann ann)))


(comment
  (int)
  (int 'test)
  (int 'test 3)
  (bool)

  (set)
  (set 'MySet)
  (set 'MySet (-- 1 10))
  (set 'MySet #{1 3 5})

  (literal-set (-- 1 10))
  )



;; No explicit support for mapping needed, as I already have the mappable clojure data structure as input to literal-array
(defn literal-array 
  "Specifies a one- or two-dimensional array that contains the given
  MiniZinc expressions as elements. Two-dimensional arrays are defined
  by a list of expressions." 
  [& exprs]
  (if (every? #(core/or (vector? %) (seq? %)) exprs)
    (str "[|" (apply str (flatten (interpose " | " (map (fn [sub-exprs]
                                                          (interpose ", " (map expr sub-exprs)))
                                                        exprs)))) "|]")    
    (format "[%s]" (apply str (interpose ", " (map expr exprs))))))

(comment
  (seq? '(1 2 3))

  (literal-array 1 2 3)
  (literal-array (int) (int) (int))
  (literal-array [(int) (int)][(int) (int)])
  (apply literal-array [(int) (int) (int)])


  (let [x (variable (-- 1 3))
        y (variable (-- 1 3))
        z (variable (-- 1 3))]
    (apply literal-array [x y z]))

  )


;;;
;;; Type-insts
;;;


(comment

  ;; !! NOTE: currently excluded from base type-insts
  ;; TODO: add these extra later (e.g., can they be elements in set or array?)
  :string
  :ann
  :opt
  )


(comment
  ;; clojure.spec def for type-insts
  ;; Not quite working yet, therefore commented out again for now
  
;;
;; !! NOTE: Some of this is already implicitly defined in :pre conditions of some functions, e.g., par -- make that uniform
;; fn variable calls mk-type-inst-string, so it now allows for too many cases
;;

;; see MiniZinc spec, Sec 6.4. Type-inst Expressions Overview, p. 8f
(spec/def ::base-ti-expr-tail-kw #{:bool :int :float})

(spec/def ::set-ti-expr (spec/cat :set #{:set} :of (spec/? #{:of})
                                  :base-ti-expr-tail ::base-ti-expr-tail))
;; (spec/conform ::set-ti-expr [:set #{1 2 3}])

(spec/def ::literal-set core/set?)

;; range
(def range-regex #"^[0-9]+\s*[.]{2}\s*[0-9]+")
(spec/def ::range-str (spec/and core/string? #(re-matches range-regex %)))
;; (spec/valid? ::range-str (-- 1 10))
;; (spec/valid? ::range-str "1 .. 10")

(spec/def ::var-par-kw #{:var :par})

(spec/def ::base-ti-expr-tail (spec/or :base-ti-expr-tail-kw ::base-ti-expr-tail-kw
                                       :literal-set ::literal-set
                                       :range-str ::range-str))

(spec/def ::base-ti-expr-tail-or-set (spec/or :base-ti-expr-tail ::base-ti-expr-tail
                                              :set-ti-expr (spec/spec ::set-ti-expr)))

(spec/def ::base-ti-expr (spec/or :base-ti-expr-tail-or-set ::base-ti-expr-tail-or-set
                                  :var-par-ti-expr (spec/spec (spec/cat :var-par ::var-par-kw
                                                                        :base-ti-expr-tail ::base-ti-expr-tail-or-set))))

(spec/def ::array-ti-expr (spec/cat :array #{:array}
                                    :index-type (spec/alt :int #{:int}
                                                          :literal-set ::literal-set
                                                          :range-str ::range-str)
                                    :of (spec/? #{:of}) 
                                    :base-ti-expr (spec/spec ::base-ti-expr)))
;; (spec/valid? ::array-ti-expr [:array :int [:var :int]])


(spec/def ::ti-expr (spec/or :base-ti-expr ::base-ti-expr
                             :array-ti-expr ::array-ti-expr))

)

(comment

  (spec/valid? ::ti-expr :bool)
  (spec/valid? ::ti-expr [:set (-- 1 3)])

  (spec/valid? ::ti-expr :bla)

  ;; (spec/valid? ::ti-expr (list [:set (-- 1 3)])) 

  (spec/valid? (spec/cat :my-type-inst ::ti-expr) (list [:set (-- 1 3)]))

  
  (gen/generate (spec/gen ::range-str))

  (gen/sample (spec/gen ::base-ti-expr-tail))
  
  (spec/exercise ::base-ti-expr-tail-kw 5)
  

  )



;; Must be public, because it is called by macro predicate
(defn ^:no-doc mk-type-inst-string 
  "[Aux function] Translates a Clojure type-inst specification into a type-inst string
 in MiniZinc syntax. `my-type-inst` is either a single specification or 
a vector of specifications.

  Examples:

  (mk-type-inst-string :bool)
  ;; ->  \"bool\"
  (mk-type-inst-string (-- 1 3))
  ;; ->  \"1..3\"
  (mk-type-inst-string #{1 3 5})
  ;; ->  \"{1, 3, 5}\"
  (mk-type-inst-string [:set (-- 1 3)])
  ;; ->  \"set of 1..3\"
  (mk-type-inst-string [:var #{1 3 5}])
  (mk-type-inst-string [:var :set #{1 3 5}])
  (mk-type-inst-string [:int #{1 3 6 8}])
  (mk-type-inst-string [:par :int])
  (mk-type-inst-string [:var :int])
  (mk-type-inst-string [:set :float])
  (mk-type-inst-string [:var :set :int])

  The keyword :of is syntactic sugar for readability, e.g., in set 
  declarations. The following two type-insts are equivalent.

  (mk-type-inst-string [:var :set :int])
  (mk-type-inst-string [:var :set :of :int]) 

  The index type of an array first specifies the array’s index set.
  The type of the contained values is specified with a nexted type-inst.

  (mk-type-inst-string [:array (-- 1 3) [:var :int]])
  (mk-type-inst-string [:array :int [:var :int]])
"
  ;; {:pre [(if (vector? my-type-inst) 
  ;;          ((first my-type-inst) #{:var :set :int :float})
  ;;          true)]}
  [my-type-inst]
  (cond
    ;; array -- format largely similar to array function args 
    (core/and (vector? my-type-inst)
              (core/= :array (first my-type-inst)))
    (str "array["
         (let [x (second my-type-inst)]
           (if (keyword? x)
             (name x)
             x))
         "] of "
         (mk-type-inst-string (core/nth my-type-inst 2)))
    ;; composite spec
    (vector? my-type-inst)
    (apply str (interpose " " (map mk-type-inst-string my-type-inst)))
    ;; individual spec
    :else (cond (core/= :var my-type-inst) "var"
                (core/= :set my-type-inst) "set of"
                (core/= :of my-type-inst) "" ;; :of can be inserted optionally for readability
                (set? my-type-inst) (apply literal-set my-type-inst)
                (core/= :variable my-type-inst) "var" ;; for consistency with fn variable
                :else (name my-type-inst))))


(comment
  ;; clojure.spec def for type-insts -- testing interface of mk-type-inst-string
  ;; Not quite working yet, therefore commented for now
  
(spec/fdef mk-type-inst-string
           ;; possibly this is no spec/cat, but what then?
           ;; :args (spec/cat :my-type-inst ::ti-expr)
           :args (fn [my-spec]
                   (print my-spec)
                   (spec/valid? (spec/cat :my-type-inst ::ti-expr) my-spec))
           :ret core/string?)
;; TMP:
(spectest/instrument `mk-type-inst-string)
;; (spectest/unstrument `mk-type-inst-string)

  (spec/valid? (spec/cat :my-type-inst ::ti-expr) (list [:var :set #{1 3 5}]))
  (spec/valid? (spec/cat :my-type-inst ::ti-expr) '(:bool))

)

(comment
  
  (mk-type-inst-string :bool)
  ;; ->  "bool"
  (mk-type-inst-string (-- 1 3))
  ;; ->  "1..3"
  (mk-type-inst-string #{1 3 5})
  ;; ->  "{1, 3, 5}"
  (mk-type-inst-string [:set (-- 1 3)])
  ;; ->  "set of 1..3"
  (mk-type-inst-string [:var #{1 3 5}])
  ;; ->  
  (mk-type-inst-string [:var :set #{1 3 5}])
  ;; ->  
  (mk-type-inst-string [:int #{1 3 6 8}])
  ;; ->  
  (mk-type-inst-string [:par :int])
  ;; ->  
  (mk-type-inst-string [:var :int])
  ;; ->  
  (mk-type-inst-string [:var :set :of :int])
  ;; ->  
  (mk-type-inst-string [:set :float])
  ;; ->  
  (mk-type-inst-string [:array (-- 1 3) [:var :int]])
  ;; ->  
  (mk-type-inst-string [:array :int [:var :int]])
  ;; ->  

  (def xx 10)
  (mk-type-inst-string [:array (-- 1 xx) [:var :int]])

  ;; Case causing errors
  (mk-type-inst-string :bla)
  
  )



(comment ; previous versions

  (defn- mk-type-inst-string [my-type-inst]
    "Translates a Clojure type-inst specification into a type-inst string in MiniZinc syntax. `my-type-inst` is either a single specification or a vector of specifications.

Examples:

  (mk-type-inst-string :bool)
  ;; ->  \"bool\"
  (mk-type-inst-string (-- 1 3))
  ;; ->  \"1..3\"
  (mk-type-inst-string #{1 3 5})
  ;; ->  \"{1, 3, 5}\"
  (mk-type-inst-string [:set (-- 1 3)])
  ;; ->  \"set of 1..3\"
  (mk-type-inst-string [:var #{1 3 5}])
  (mk-type-inst-string [:var :set #{1 3 5}])
  (mk-type-inst-string [:int #{1 3 6 8}])
  (mk-type-inst-string [:par :int])
  (mk-type-inst-string [:var :int])
  (mk-type-inst-string [:var :set :of :int])
  (mk-type-inst-string [:set :float])

Note that the index type of an array is enclosed in a string with the array keyword.

  (mk-type-inst-string '[(:array (-- 1 3)) :of :var :int])
  (mk-type-inst-string '[(:array :int) :of :var :int])

The keyword :of is optional for readability. 
"
    ;; {:pre [(if (vector? my-type-inst) 
    ;;          ((first my-type-inst) #{:var :set :int :float})
    ;;          true)]}
    (if (vector? my-type-inst)
      (apply str (interpose " " (map mk-type-inst-string my-type-inst)))
      (cond (core/= :var my-type-inst) "var"
            ;; array test
            (clojure.core/and (list? my-type-inst)
                              ;; argument agains eval: no lexical context
                              (core/= :array (first my-type-inst)))
            ;; how to translate arrays
            (str "array["
                 (let [x (second my-type-inst)]
                   (cond (keyword? x) (name x)
                         (list? x)
                         ;; BUG: elements in (rest x) not resolved
                         (apply (resolve (first x)) (rest x))))
                 "] of")
            (core/= :set my-type-inst) "set of"
            (core/= :of my-type-inst) "" ;; :of can be inserted optionally for readability
            (set? my-type-inst) (apply literal-set my-type-inst)
            (core/= :variable my-type-inst) "var" ;; for consistency with fn variable
            :else (name my-type-inst))))


  
  (defn- mk-type-inst-string [type-inst]
    ;; {:pre [(if (vector? type-inst) 
    ;;          ((first type-inst) #{:var :set :int :float})
    ;;          true)]}
    (if (vector? type-inst)
      (apply str (interpose " " (map mk-type-inst-string type-inst)))
      (cond (core/= :var type-inst) "var"
            (core/and (list? type-inst)
                      ;; argument agains eval: no lexical context
                      (core/= :array (first type-inst))) (str "array[" (eval (second type-inst)) "] of")
            (core/= :set type-inst) "set of"       
            (set? type-inst) (apply literal-set type-inst)
            :else (name type-inst))))
  )




;; Semi BUG: somewhat questionable: the [dimension] of the set (e.g., "1..10") is temporarily stored as aVar name to make it easily accessible for the array construction. Later the set-of-int is not used at all. Possibly better to completely avoid this potential cause of confusion, i.e., not to use a set for the array construction (or to clean up the internal use of sets here). 
(defn array   
  "Declares a one- or multi-dimensional array of constant values or  
  decision variables of various types.

  Arguments:

  - index-set: The explicitly declared indices of the array. Either an
    integer range (declared with function --), a set variable
    initialised to an integer range, or for multi-dimensional arrays a
    list of integer ranges and/or MiniZinc sets. 
  - type-inst: Specifies the parameter type or variable domain  
    The type of the variables contained in the array (a string, symbol
    or keyword; can be int, float, bool, string and \"set of int\"). 
  - array-name (optional): a name for the array (a string, symbol or
    keyword). Default is a \"gensym-ed\" name. 
  - init-value (optional): a vector of MiniZinc-supported
    values. Defaults to nil. 

  Examples:
  
  ; array of bools at indices 1-10 (not decision variables!)
  (array (-- 1 10) :bool)               
  ; array of ints at indices 1-10
  (array (-- 1 10) :int)               
  ; array of sets of integers 
  (array (-- 1 10) [:set :int])        
  ; array of integers in given range  
  (array (-- 1 10) (-- -1 2))           
  ; array of floats in range
  (array (-- 1 10) (-- 2.0 4.0))        
  ; array of subsets of set range
  (array (-- 1 10) [:set (-- -1 2)])    

  ; array of int variables
  (array (-- 1 10) [:var :int])        
  ; array of int variables with given domain  
  (array (-- 1 10) [:var (-- 1 3)])    
  ; array of int variables with given domain
  (array (-- 1 10) [:var #{1 3 5}])     
  ; array of float variables with given domain
  (array (-- 1 10) [:var (-- 1.0 3.0)]) 
  ; array of set variables with domain
  (array (-- 1 10) [:var :set (-- 1 3)]) 
  ; array of set variables with domain   
  (array (-- 1 10) [:var :set #{1 3 5}]) 

  ; two-dimensional array of int variables
  (array [(-- 1 10) (-- 1 10)]  [:var :int (-- 1 3)]) 

  ; array explicitly named x 
  (array (-- 1 10) :int 'x)              

  ; array of ins with init value
  (array (-- 1 3) :int 'x [5 6 7])      

  BUG: literal arrays not supported as init val.
  BUG: Annotations not yet supported.
  "
  ([index-set type-inst] (array index-set type-inst (gensym "array")))
  ([index-set type-inst array-name] (array index-set type-inst array-name nil))
  ([index-set type-inst array-name init-value]
   ;; {:pre [(#{"int" "float" "bool" "string" "set of int"} (name type-inst))]}
   ;; (println (pprint/cl-format nil "type-inst: ~S, init-value: ~S, array-name ~S" type-inst init-value array-name))
   (tell-store!
    ;; telling a parameter or variable?
    (if (core/and (vector? type-inst)
                  (in? type-inst :var))
        :variable
        :parameter)
    (make-anArray 
     (name array-name) 
     (format "array[%s] of %s: %s;" 
             ;; index-set
             (cond 
               ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
               (aVar? index-set) (:name index-set)
               (core/string? index-set) index-set
               ;; TODO: consider replacing list? with the more general seq?, just in case (to include cons's, lazy seq's etc.
               (core/or (list? index-set) 
                        (vector? index-set)) (apply str
                                                    (interpose ", "
                                                               (map #(cond 
                                                                       ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
                                                                       (aVar? %) (:name %)
                                                                       (core/string? %) %
                                                                       :else (throw 
                                                                              (Exception. 
                                                                               (pprint/cl-format nil "Not allowed as array index-set: ~S of type ~S" 
                                                                                                 % (type %)))))
                                                                    index-set)))
               :else (throw (Exception. 
                             (pprint/cl-format nil "Not allowed as array index-set: ~S of type ~S" 
                                               index-set (type index-set)))))
             (mk-type-inst-string type-inst) ;; TODO: unfinished: only simple type-inst naming type supported
             (if init-value
               (str (name array-name) " = " (apply literal-array init-value))
               (name array-name)))
     index-set))))


(comment

  (array (-- 1 10) :bool)
  (array (-- 1 10) :int 'test) ;"array[0..10] of var int: test;"
  (array (-- 1 10) (-- 1 3))
  (array (-- 1 10) (-- 1.0 3.0))
  (array (-- 1 10) [:set :int])
  (array (-- 1 10) [:set (-- 1 3)])
  (array (-- 1 10) [:var :int])
  (array (-- 1 10) [:var (-- 1 3)])
  (array (-- 1 10) [:var (-- 1.0 3.0)])
  (array (-- 1 10) [:var #{1 3 5}])
  (array (-- 1 10) [:var :set (-- 1 3)])
  (array (-- 1 10) [:var :set #{1 3 5}])
  (array (list (-- 1 10) (-- 1 10))  [:var :int (-- 1 3)])
  (array (-- 1 3) :int 'x [5 6 7])
  ;; BUG: literal arrays should be supported as well
  (array (-- 1 3) :int 'x (literal-array 5 6 7))

  )



(defn nth 
  "Accesses the array element at the given index, or indices in case of a multi-dimensional array"
  [my-array & indices]
  (format "%s[%s]" (expr my-array) (apply str (interpose ", " (map expr indices)))))

(comment
  (def a (array (-- 0 10) :int))
  (nth a 1)
  (nth a 1 2)
  )

;; TODO: 
;; - Should return nested sequence for multi-dimensional array. (see BUG report in doc below)
(defn array->clj-seq
  "Transforms a one or more dimensional MiniZinc array into a Clojure
  list (of MiniZinc code strings representing the array elements), so
  that MiniZinc functions can  be applied to individual MiniZinc
  elements (e.g., by mapping). 

  BUG: multi-dimensional array should return nested sequence to clearly
  highlight the dimensions. Currently, simply a flat sequence with all
  elements (the cartesian product) is returned." 
  [my-array]
  (let [bounds (:boundaries my-array)]
    (cond 
     ;; multi-dimensional array
     (seq? bounds) (map (fn [args] (apply nth my-array args))
                        (apply combi/cartesian-product
                               (map (fn [bnds] (core/range (:min bnds) (core/+ 1 (:max bnds))))
                                    bounds)))
     ;; one-dimensional array
     (map? bounds) (map (fn [i] (nth my-array i))
                        (core/range (:min bounds) (core/+ 1 (:max bounds)))))))

(comment
  (array->clj-seq (array (-- 2 4) :bool))
  (array->clj-seq (array (list (-- 0 10) (-- 2 4)) :bool))  

  ;; Higher-order programming in MiniZinc
  ;; mapping a MiniZinc record, applying some constraint to each of its elements :)
  (map (fn [element] (constraint (< (+ element 1) 10)))
       (array->clj-seq (array (-- 1 3) :bool)))
  ;; not working yet -- + not yet defined for arbitrary number of arguments
  (apply + (map (fn [element] (+ element 1))
                (array->clj-seq (array (-- 1 3) :bool))))
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calls to MiniZinc fns etc
;;;

(defn call-operator 
  "A MiniZinc operator call supporting arities of 2 or more. For
  higher arities multiple operators are used (e.g., a ternary plus is
  translated into x + y + z)." 
  [operator x y & more]
  (str "(" (apply str (interpose (str " " operator " ") (map expr (cons x (cons y more))))) ")"))

(comment
  (call-operator '+) ; error as intended
  (call-operator '+ 1) ; error as intended
  (call-operator '+ 1 2)
  (call-operator '+ 1 2 3)
  )

(defn call-unary-operator 
  "A MiniZinc unary operator call."
  [operator x]
  (str operator (expr x)))

(comment
  (call-unary-operator '+ 1)
  )

(defn call-fn 
  "A MiniZinc function call supporting a function arity of 1 or more." 
  [fn & args]
  (format (str fn "(%s)") (apply str (interpose ", " (map expr args)))))

(comment
  (call-fn 'foo)
  (call-fn 'foo 'bar)
  (call-fn 'foo 'x 'y 'z)
  )

;; http://www.minizinc.org/downloads/doc-1.6/mzn-globals.html
;; !! BUG: include must be added at top-level -- it cannot happen within a predicate def or local body
(defn call-global-constraint
  "Includes <fn>.mzn and then calls fn, like [[call-fn]]."
  [fn & args]
  (include (str fn ".mzn"))
  (apply call-fn fn args))

(comment
  (str 'alldifferent ".mzn")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Aggregation functions for arithmetic arrays are
;;

;; (comment
;; ;; syntax to generate  
;;    ;; array comprehension = `[` 〈expr〉 `|` 〈generator-exp>* [where 〈bool-exp〉] `]`
;;    ;; generator = 〈identifier〉* in 〈array-exp〉  ;  identifier is an iterator 
;;   )


(defn ^:no-doc aggregation-format 
  "[Aux for aggregation functions like forall] This function is only
  public, because it is needed in a public macro." 
  [set-or-array vars where-expr exp]
  (format (case set-or-array 
            :array "[%s | %s]"
            :set "{%s | %s}"
            ;; TMP: this exception is overkill
            (throw (Exception. 
                    (pprint/cl-format nil "arregate arg set-or-array (must only be :array or :set): ~S" 
                                      set-or-array))))
          (str exp)
          (str (apply str (interpose ", " (map :mzn-string vars))) 
               (when where-expr (format " where %s" where-expr)))))

(comment
(def a (array (-- 0 10) :int))
(def i (make-aVar (name 'i) (format "%s in %s" (name 'i) (-- 1 10))))
(def j (make-aVar (name 'j) (format "%s in %s" (name 'j) (-- 1 10))))
(aggregation-format 
 :array
 (list i j)
 (< i j)
 (= (nth a j) 0))
)


;; TODO: only use gensymed or auto-gensymed variables in all macros 
(defmacro aggregate
  "List (array) and set comprehension. Generates a MiniZinc array/set
  containing the possible combinations of locally declared MiniZinc
  parameters (generators) used in the expression (exp).  

  Example:

    (aggregate [i (-- 1 3)] 
      (* i i))
    ; means [1*1, 2*2, 3*3]  (in MiniZinc syntax)

  Note that every generator (e.g., `(-- 1 3)`) declares always only a
  single MiniZinc parameter (`i` in the example above). This follows
  standard Clojure conventions (e.g., similar to `let` and friends),
  while MiniZinc allows for declaring multiple parameters together. 

A where-expression can be added after the generators, which acts as a
  filter. Only elements satisfying this Boolean expression are used to
  construct elements in the output array/set. 

  Example:

    (def a (array (-- 1 3) :int))
    (aggregate [i (-- 1 3)
                j (-- 1 3) 
                :where (< i j)]
      (!= (nth a i) (nth a j)))  
    ; means [a[1] != a[2], a[2] != a[3]] (in MiniZinc syntax)

  The optional arguments `set-or-array` specifies whether result is
  array or set. It must be either `:array` or `:set`, default is
  `:array`. 
  
  Example:

    (aggregate [i (-- 1 3)]
      (= (nth a i) 0)
      :set)
  "
  {:style/indent [1 [[:defn]] :form]}
  ([generators exp] `(aggregate ~generators ~exp :array)) 
  ([generators exp set-or-array]
     (let [all-var-val-pairs (partition 2 generators)
           last-pair (last all-var-val-pairs)
           where-expr (when (core/= (first last-pair) :where) 
                        (second last-pair))
           var-val-pairs (if where-expr
                           (butlast all-var-val-pairs)
                           all-var-val-pairs)]
       `(let ~(vec (mapcat (fn [[par-name range]]
                             (list par-name `(make-aVar ~(name par-name) (format "%s in %s" ~(name par-name) ~range))))
                           var-val-pairs))
          (aggregation-format 
           ~set-or-array
           ~(cons 'list (map first var-val-pairs))
           ~where-expr
           ~exp)))))

(comment
  (def a (array (-- 0 10) :int))

  (aggregate [i (index_set a)]
    (= (nth a i) 0))

  (aggregate [i (-- 1 3)
              j (-- 1 3) 
              :where (< i j)]
    (!= (nth a i) (nth a j)))

  (aggregate [i (index_set a)]
    (= (nth a i) 0)
    :set)
  )



(defmacro forall
   "Universal quantification with list comprehension support: Logical
  conjunction of aggregated Boolean expressions. When applied to an
  empty list, forall returns true. 

  See [[aggregate]] for list comprehension syntax and examples."
  {;; :forms '[(forall [generators*] exp)]
   :arglists '([[generators*] exp])
   :style/indent [1 [[:defn]] :form]}
  [generators exp]
  `(format "forall(%s)" (aggregate ~generators ~exp)))


(comment
  (def a (array (-- 1 10) :int 'a))
  (forall [i (-- 1 10)]
          (= (nth a i) 0))
  ;; => "forall([(a[i] = 0) | i in 1..10])" 

  (forall [i (index_set a)]
          (= (nth a i) 0))

  ;; a only 1 dimensional, but for test this is sufficient :)
  (forall [i (-- 0 10)
           j (-- 0 10)]
          (= (nth a i j) 0))

  (forall [i (-- 1 3)
           j (-- 1 3) 
           :where (< i j)]
      (!= (nth a i) (nth a j)))

  )



(defmacro exists
  "Existential quantification (logical disjunction of Boolean
  expressions). When applied to an empty list, exists returns false.

  Unary: MiniZinc function exists.

  Binary: exists with list comprehension support.

  Example:
  (exists [i (-- 1 3)] (= (nth a i) 0))

  See [[aggregate]] for list comprehension syntax and examples."
  {:arglists '([[generators*] exp]
               [exp])
   ;; :forms '[(exists [generators*]? exp)]   
   :style/indent [1 [[:defn]] :form]}
  ([x] (call-fn 'exists x))
  ([generators exp]
   `(format "exists(%s)" (aggregate ~generators ~exp))))

(comment
  (def a (array (-- 1 10) :int 'a))
  (exists [i (-- 1 3)] (= (nth a i) 0))
  (exists a)
  )

(defmacro xorall
  "N-ary exclusive disjunction with list comprehension support: odd
  number of aggregated Boolean expressions holds.

  See [[aggregate]] for list comprehension syntax and examples."
  {; :forms '[(xorall [generators*] exp)]
   :arglists '([[generators*] exp])
   :style/indent [1 [[:defn]] :form]}
  [generators exp]
  `(format "xorall(%s)" (aggregate ~generators ~exp)))

(defmacro iffall
  "N-ary bi-implication with list comprehension support: even number
  of aggregated Boolean expressions holds.

  See [[aggregate]] for list comprehension syntax and examples."
  {; :forms '[(iffall [generators*] exp)]
   :arglists '([[generators*] exp])
   :style/indent [1 [[:defn]] :form]}
  [generators exp]
  `(format "iffall(%s)" (aggregate ~generators ~exp)))

(defmacro sum
  "Summation

  Unary: MiniZinc function sum.
  
  Binary: sum with list comprehension support: adds aggregated
  expressions. If aggregated expressions are empty returns 0. 

  See [[aggregate]] for list comprehension syntax and examples."
  {; :forms '[(sum [generators*] exp)]
   :arglists '([[generators*] exp]
               [exp])
   :style/indent [1 [[:defn]] :form]}
  ([x] (call-fn 'sum x))
  ([generators exp]
     `(format "sum(%s)" (aggregate ~generators ~exp))))

(defmacro product
  "Multiplication

  Unary: MiniZinc function Multiplication.

  Binary: product with list comprehension support: multiplies
  aggregated expressions. If aggregated expressions are empty returns
  1.
  
  See [[aggregate]] for list comprehension syntax and examples."
  {; :forms '[(product [generators*] exp)]
   :arglists '([[generators*] exp]
               [exp])
   :style/indent [1 [[:defn]] :form]}
  ([x] (call-fn 'product x))
  ([generators exp]
     `(format "product(%s)" (aggregate ~generators ~exp))))

(defmacro min
  "Minimal value

  Unary: MiniZinc function min.

  Binary: min with list comprehension support: least element in
  aggregated expressions. If aggregated expressions are empty gives
  MiniZinc error.

  See [[aggregate]] for list comprehension syntax and examples."
  {; :forms '[(min [generators*] exp)]
   :arglists '([[generators*] exp]
               [exp])
   :style/indent [1 [[:defn]] :form]}
  ([x] (call-fn 'min x))
  ([generators exp]
     `(format "min(%s)" (aggregate ~generators ~exp))))

(defmacro max
  "Maximal value

  Unary: MiniZinc function max.

  Binary: max with list comprehension support: greatest element in
  aggregated expressions. If aggregated expressions are empty gives
  MiniZinc error.

  See [[aggregate]] for list comprehension syntax and examples."
  {; :forms '[(max [generators*] exp)]
   :arglists '([[generators*] exp]
               [exp])
   :style/indent [1 [[:defn]] :form]}
  ([x] (call-fn 'max x))
  ([generators exp]
     `(format "max(%s)" (aggregate ~generators ~exp))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MiniZinc vars
;;;

;; TODO: find out whether there is a way in MiniZinc to restrict the domain of an integer to only a given list of integers (i.e., "cut holes" into the domain)
(defn --
  "Expects a minimum an a maximum value (ints or floats) and returns a
  domain specification for a decision variable (ints or floats)."
  [min max]
  (format  "%s..%s" (expr min) (expr max))
  ;; (pprint/cl-format nil "~S..~S" (expr min) (expr max))
  )

(comment
  (def myInt (int 3))
  (-- 0 2)
  (-- 0 myInt)

  (-- 1 3)
  (-- 1.0 3.0)
  )

;; I cannot shadow special forms, and therefore a function cannot be called var. Thus use function name "variable", even though MiniZinc keyword is "var".
(defn variable
    "Declares a decision variable (bool, int, float, or set of int)
  with the given domain, and an optional variable name (string, symbol
  or keyword). 

  Use [[array]] for creating an array of variables. 

  Examples:

  ; a Boolean variable (no further domain specification supported)
  (variable :bool)             
  ; an integer variable with maximum supported domain size 
  (variable :int)             
  ; an integer variable with domain [-1, 10]  
  (variable (-- -1 10))       
  ; an integer variable with the domain {1, 3, 6, 8}
  (variable #{1 3 6 8})        
  ; a float variable with domain [1.0, 10.0]
  (variable (-- 1.0 10.0))     
  ; a set of integers with the given domain (set is subset of domain)
  (variable [:set (-- 1 3)])  
  ; a set of integers with the given domain (set is subset of domain)  
  (variable [:set #{1 3 6 8}])
  ; same as (variable (-- -1 10))
  (variable [:int (-- 1 3)])   

  ; an integer variable named x (instead of an automatically assigned name)
  (variable (-- 1 10) 'x)    

  BUG: Annotations not yet supported.
"
    ([type-inst] (variable type-inst (gensym "var")))
    ([type-inst var-name]
       (let [dom-string (mk-type-inst-string type-inst)
             name-string (name var-name)]
         (tell-store! :variable
                      (make-aVar name-string (format "var %s: %s;" dom-string name-string))))))
    

(comment
  (binding [*mzn-store* ()]
    (variable (-- 1 3) 'x))
  (binding [*mzn-store* ()]
    (variable (-- 1 3) :x))

  (variable :bool)
  (variable (-- 1 3))
  (variable (-- 1.0 3.0))
  (variable (-- 1 3) 'myVar)
  ;; specify domain as list of ints
  (variable '(1 3 6 8))
  ;; set variable
  (variable [:set (-- 1 3)])
  (variable [:set (-- 1 3)] 'mySet)
  (variable [:set #{1 3 6 8}])
  ;; for completeness/consistency, but contains superflous information
  (variable [:int #{1 3 6 8}])
  (variable [:float (-- 1.0 3.0)])
  (variable [:bla (-- 1.0 3.0)]) ; causes exception
  ;; alternative notation -- more clean solution -- can be defined when notation above is supported
  (set-variable (-- 1 3))
  (set-variable (-- 1 3) 'mySet)
  (set-variable #{1 3 6 8})
  )



;; TMP: test
(comment
  ;; dynamic binding test and updating state of dynamically changed var
  ;; book "The Joy of Clojure", Sec. 11.9 Vars and dynamic binding 
  ;; see also http://stackoverflow.com/questions/11730828/clojure-and-dynamic
  ;; see http://clojure.org/vars , http://stackoverflow.com/questions/1523240/let-vs-binding-in-clojure
  (def ^:dynamic *mzn-store* false) ;; variable that allows for dynamic scope
  (defn print-my-var []
    (println "*mzn-store* is currently: " *mzn-store*))
  (defn binding-play [] 
    (print-my-var)
    ;; scope of variable overwritten in thread created by binding
    ;; I will call this in top-level def for CSPs
    ;; initialise empty store
    (binding [*mzn-store* ()]  
      (print-my-var)
      ;; change thread local binding (root binding of a var cannoth be changed that way, only with def could it be redefined)
      ;; E.g., add constraint or variable to store
      (set! *mzn-store* (cons 1 *mzn-store*)) ;; changes global binding, not local binding within this thread! 
      (print-my-var))
    (print-my-var))
  (binding-play)

  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constraints -- aux defs
;;;

(defn constraint 
  "Expects a constraint expression (a string) and turns it into a
  constraint statement.

  BUG: Annotations not yet supported."
  [constraint-expr]
  (tell-store! :other (format "constraint %s;" (expr constraint-expr))))

;; (defmacro ^:private def-unary-operator
;;   "Defines a function that outputs the code for a MiniZinc unary operator."
;;   [op-name operation doc-string]
;;   `(defn ~op-name
;;      ~doc-string
;;      [arg#]
;;      (format ~(str operation " %s")  (expr arg#))))

;; (defmacro ^:private def-binary-operator
;;   "Defines a function that outputs the code for a MiniZinc binary operator."
;;   [op-name operation doc-string]
;;   `(defn ~op-name
;;      ~doc-string
;;      [lh# rh#]
;;      (format ~(str "(%s " operation " %s)") (expr lh#) (expr rh#))))

;; (defmacro ^:private def-binary-and-n-ary-operator
;;   "Defines a function that outputs the code for a MiniZinc binary operator."
;;   [op-name operation doc-string]
;;   `(defn ~op-name
;;      ~doc-string
;;      ([lh# rh#]
;;         (format ~(str "(%s " operation " %s)") (expr lh#) (expr rh#)))
;;      ([arg1# arg2# & args#]
;;         (reduce ~op-name arg1# (cons arg2# args#)))))

;; ;; BUG: parentheses surrounding expressions are surrounding accumulating expressions like (((a + b) + c) + d)
;; (defmacro ^:private def-unary-and-n-ary-operator
;;   "Defines a function that outputs the code for a MiniZinc unary and binary operator."
;;   [op-name operation doc-string]
;;   `(defn ~op-name
;;      ~doc-string
;;      ([arg#]
;;         (format ~(str operation " %s") (expr arg#)))
;;      ([lh# rh#]
;;         (format ~(str "(%s " operation " %s)") (expr lh#) (expr rh#)))
;;      ([arg1# arg2# & args#]
;;         (reduce ~op-name arg1# (cons arg2# args#)))))

;; (defmacro ^:private def-unary-function
;;   "Defines a function that outputs the code for a MiniZinc function."
;;   [fn-name fn doc-string]
;;   `(defn ~fn-name
;;      ~doc-string
;;      [arg#]
;;      (format ~(str fn "(%s)")  (expr arg#))))

;; (defmacro ^:private def-binary-function
;;   "Defines a function that outputs the code for a MiniZinc function."
;;   [fn-name fn doc-string]
;;   `(defn ~fn-name
;;      ~doc-string
;;      [arg1# arg2#]
;;      (format ~(str fn "(%s, %s)")  (expr arg1#) (expr arg2#))))

;; (defmacro ^:private def-n-ary-function
;;   "Defines a function that outputs the code for a MiniZinc function."
;;   [fn-name fn doc-string]
;;   `(defn ~fn-name
;;      ~doc-string
;;      ([arg1# arg2#]
;;         (format ~(str fn "(%s, %s)")  (expr arg1#) (expr arg2#)))
;;      ([arg1# arg2# & args#]
;;         (reduce ~fn-name arg1# (cons arg2# args#)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constraint defs
;;;


;; (def-unary-and-n-ary-operator + + 
;;   "+ constraint")
;; (def-unary-and-n-ary-operator - -
;;   "- constraint")

;; NOTE: def longer than necessary (macro call to define fn can be more concise), but this way the function args are documented more cleanly
(defn + 
  "Addition, or unary operator +"
  ([x] (call-unary-operator '+ x))
  ([x y & more] (apply call-operator '+ x y more)))
(defn -
  "Subtraction, or unary operator -"
  ([x] (call-unary-operator '- x))
  ([x y & more] (apply call-operator '- x y more)))

(comment
  (+) ;; error, as desired
  (+ (variable (-- 1 3)))
  (+ 1 2)
  (+ 1 2 3 4 5)
  )

;; (def-binary-operator <-> <->
;;   "Logical equivalence constraint")
;; (def-binary-operator -> ->
;;   "Logical implication constraint")
;; (def-binary-operator <- <-
;;   "")
(defn <-> 
  "Logical equivalence"
  [x y]
  (call-operator '<-> x y))
(defn -> 
  "Logical implication"
  [x y] 
  (call-operator '-> x y))
(defn <- 
  "Reverse logical implication"
  [x y] 
  (call-operator '<- x y))

;; (def-unary-operator not not 
;;   "Logical negation constraint")
(defn not 
  "Logical negation"
  [x] 
  (call-unary-operator 'not x))

;; (def-binary-and-n-ary-operator or "\\/"
;;   "Logical or constraint")
;; (def-binary-operator xor xor
;;   "Logical xor constraint")
;; (def-binary-and-n-ary-operator and "/\\"
;;   "Logical and constraint")

(defn or
  "Logical or"
  [x y & more] 
  (apply call-operator "\\/" x y more))
(defn xor
  "Logical xor"
  [x y & more] 
  (apply call-operator 'xor x y more))
(defn and
  "Logical and"
  [x y & more] 
  (apply call-operator "/\\" x y more))

;; (def-binary-operator < <
;;   " constraint")
;; (def-binary-operator > >
;;   " constraint")
;; (def-binary-operator <= <=
;;   " constraint")
;; (def-binary-operator >= >=
;;   " constraint")
;; (def-binary-operator = =
;;   " constraint")
;; (def-binary-operator == ==
;;   " constraint")
;; (def-binary-operator != !=
;;   "Not equal constraint")

(defn <
  "Less than"
  [x y & more] 
  (apply call-operator '< x y more))
(defn >
  "Greater than"
  [x y & more] 
  (apply call-operator '> x y more))
(defn <=
  "Less than or equal"
  [x y & more] 
  (apply call-operator '<= x y more))
(defn >=
  "Greater than or equal"
  [x y & more] 
  (apply call-operator '>= x y more))
(defn =
  "Equal"
  [x y & more] 
  (apply call-operator '= x y more))
(defn ==
  "Equal"
  [x y & more] 
  (apply call-operator '== x y more))
(defn !=
  "Not equal"
  [x y & more] 
  (apply call-operator '!= x y more))

;; (def-binary-operator in in
;;   " constraint")
;; (def-binary-operator subset subset
;;   " constraint")
;; (def-binary-operator superset superset
;;   " constraint")
;; (def-binary-operator union union
;;   " constraint")
;; (def-binary-operator diff diff
;;   " constraint")
;; (def-binary-operator symdiff symdiff
;;   " constraint")
;; (def-binary-operator intersect intersect
;;   " constraint")

(defn in
  "Set membership"
  [x s] 
  (call-operator 'in x s))
(defn subset
  "Subset"
  [s1 s2] 
  (call-operator 'subset s1 s2))
(defn superset
  "Superset"
  [s1 s2] 
  (call-operator 'superset s1 s2))
(defn union
  "Set union"
  [s1 s2] 
  (call-operator 'union s1 s2))
(defn diff
  "Set difference"
  [s1 s2] 
  (call-operator 'diff s1 s2))
(defn symdiff
  "Set symmetric difference"
  [s1 s2] 
  (call-operator 'symdiff s1 s2))
(defn intersect
  "Set intersection"
  [s1 s2] 
  (call-operator 'intersect s1 s2))

;; (def-binary-and-n-ary-operator ++ ++
;;   "Concatenates strings and arrays.")
;; (def-binary-and-n-ary-operator * *
;;   " constraint")
;; (def-binary-and-n-ary-operator / /
;;   " constraint")
;; (def-binary-and-n-ary-operator div div
;;   " constraint")

(defn ++
  "String and array concatenation"
  [x y & more] 
  (apply call-operator '++ x y more))
(defn *
  "Multiplication"
  [x y & more] 
  (apply call-operator '* x y more))
(defn /
  "Floating point division"
  [x y & more] 
  (apply call-operator '/ x y more))
(defn div
  "Integer devision, result rounded towards zero"
  [x y & more] 
  (apply call-operator 'div x y more))

;; (def-binary-operator mod mod
;;   " constraint")
(defn mod
  "Modulo operation (remainder of division)"
  [x y] 
  (call-operator 'mod x y))

;; (def-unary-function abort abort
;;   " function constraint")
(defn abort 
  "Abort evaluation, printing the given string."
  [my-string] 
  (call-fn 'abort (string my-string)))
;; (def-unary-function abs abs
;;   "absolute value constraint")
(defn abs 
  "Absolute value"
  [x] 
  (call-fn 'abs x))
;; (def-unary-function acos acos
;;   "arccosine constraint")
;; (def-unary-function acosh acosh
;;   "hyperbolic arccosine constraint")
(defn acos 
  "Arccosine"
  [x] 
  (call-fn 'acos x))
(defn acosh 
  "Hyperbolic arccosine"
  [x] 
  (call-fn 'acosh x))
;; (def-unary-function array_intersect array_intersect
;;   " function constraint")
;; (def-unary-function array_union array_union
;;   " function constraint")
;; (def-unary-function array1d array1d
;;   " function constraint")
;; (def-unary-function array2d array2d
;;   " function constraint")
;; (def-unary-function array3d array3d
;;   " function constraint")
;; (def-unary-function array4d array4d
;;   " function constraint")
;; (def-unary-function array5d array5d
;;   " function constraint")
;; (def-unary-function array6d array6d
;;   " function constraint")
(defn array_intersect
  "Intersection of an array of sets"
  [x] 
  (call-fn 'array_intersect x))
(defn array_union
  "Union of an array of sets"
  [x] 
  (call-fn 'array_union x))
(defn array1d
  "Initialise an array of one dimension from given array a.

  See example for [[array2d]]."
  [x a] 
  (call-fn 'array1d x a))
(defn array2d
  "Initialise a 2D array from given array a.
  
  Example:
  
  `(array2d (-- 1 3) (-- 1 2) [1 2 3 4 5 6])` is equivalent to 
  `(literal-array [1 2][3 4][5 6])` which is the MiniZinc array
  `[|1, 2 |3, 4 |5, 6|])`"
  [x1 x2 a] 
  (call-fn 'array2d x1 x2 a))
(defn array3d
  "Initialise a 3D array from given array a.

  See example for [[array2d]]."
  [x1 x2 x3 a] 
  (call-fn 'array3d x1 x2 x3 a))
(defn array4d
  "Initialise a 4D array from given array a.

  See example for [[array2d]]."
  [x1 x2 x3 x4 a] 
  (call-fn 'array4d x1 x2 x3 x4 a))
(defn array5d
  "Initialise a 5D array from given array a.

  See example for [[array2d]]."
  [x1 x2 x3 x4 x5 a] 
  (call-fn 'array5d x1 x2 x3 x4 x5 a))
(defn array6d
  "Initialise a 6D array from given array a.

  See example for [[array2d]]."
  [x1 x2 x3 x4 x5 x6 a] 
  (call-fn 'array6d x1 x2 x3 x4 x5 x6 a))

;; (def-unary-function asin asin
;;   "arcsine constraint")
;; (def-unary-function asinh asinh
;;   "hyperbolic arcsine constraint")
;; (def-unary-function atan atan
;;   "arctangent constraint")
;; (def-unary-function atanh atanh
;;   "hyperbolic arctangent constraint")
(defn asin
  "Arcsine"
  [x] 
  (call-fn 'asin x))
(defn asinh
  "Hyperbolic arcsine"
  [x] 
  (call-fn 'asinh x))
(defn atan
  "Arctangent"
  [x] 
  (call-fn 'atan x))
(defn atanh
  "Hyperbolic arctangent"
  [x] 
  (call-fn 'atanh x))

;; (def-unary-function bool2int bool2int
;;   " function constraint")
;; (def-unary-function card card
;;   " function constraint")
;; (def-unary-function ceil ceil
;;   " function constraint")
;; (def-unary-function concat concat
;;   " function constraint")
;; (def-unary-function cos cos
;;   "cosine constraint")
;; (def-unary-function cosh cosh
;;   "hyperbolic cosine constraint")

(defn bool2int
  "Boolean to integer conversion: 1 for true and 0 otherwise"
  [b] 
  (call-fn 'bool2int b))
(defn card
  "Set cardinality"
  [s] 
  (call-fn 'card s))
(defn ceil
  "Rounds float towards infinitiy"
  [x] 
  (call-fn 'ceil x))
(defn concat
  "Concatenate an array of strings. Equivalent to folding '++' over
  the array, but may be implemented more efficiently."
  [x] 
  (call-fn 'concat x))
(defn cos
  "Cosine"
  [x] 
  (call-fn 'cos x))
(defn cosh
  "Hyperbolic cosine"
  [x] 
  (call-fn 'cosh x))

;; (def-unary-function dom dom
;;   " function constraint")
;; (def-unary-function dom_array dom_array
;;   " function constraint")
;; (def-unary-function dom_size dom_size
;;   " function constraint")

(defn dom
  "Domain reflection: a safe approximation to the possible values of x
  (int)." 
  [x] 
  (call-fn 'dom x))
(defn dom_array
  "Domain reflection: a safe approximation to the union of all
  possible values of the expressions appearing in the array x (ints)."
  [x] 
  (call-fn 'dom_array x))
(defn dom_size
  "Domain reflection: domain size, equivalent to (card (dom x))"
  [x] 
  (call-fn 'dom_size x))

;; NOTE: starred form of exists*, sum*, product*, max*, and min* to avoid conflict with macros of otherwise same name
;; (def-unary-function exist* exist
;;   " function constraint")
;; (def-unary-function fix fix
;;   " function constraint")
;; (def-unary-function exp exp
;;   "exponentiation of e constraint")
;; (def-unary-function floor floor
;;   " function constraint")

(defn fix
  "Check if the argument’s value is fixed at this point in
  evaluation. If not, abort; if so, return its value. This is most
  useful in output items when decision variables should be fixed -- it
  allows them to be used in places where a fixed value is needed, such
  as if-then-else conditions."
  [x] 
  (call-fn 'fix x))
(defn exp
  "Exponentiation of e"
  [x] 
  (call-fn 'exp x))
(defn floor
  "Rounds float towards negative infinitiy"
  [x] 
  (call-fn 'floor x))

;; (def-unary-function index_set index_set
;;   " function constraint")
;; (def-unary-function index_set_1of2 index_set_1of2
;;   " function constraint")
;; (def-unary-function index_set_2of2 index_set_2of2
;;   " function constraint")
;; (def-unary-function index_set_1of3 index_set_1of3
;;   " function constraint")
;; (def-unary-function index_set_2of3 index_set_2of3
;;   " function constraint")
;; (def-unary-function index_set_3of3 index_set_3of3
;;   " function constraint")

(defn index_set
  "Reflection function: the index set (set of indices) of a one
  dimensional array."
  [x] 
  (call-fn 'index_set x))
(defn index_set_1of2
  "Reflection function: the first index set of a 2D array."
  [x] 
  (call-fn 'index_set_1of2 x))
(defn index_set_2of2
  "Reflection function: the second index set of a 2D array."
  [x] 
  (call-fn 'index_set_2of2 x))
(defn index_set_1of3
  "Reflection function: the first index set of a 3D array."
  [x] 
  (call-fn 'index_set_1of3 x))
(defn index_set_2of3
  "Reflection function: the second index set of a 3D array."
  [x] 
  (call-fn 'index_set_2of3 x))
(defn index_set_3of3
  "Reflection function: the third index set of a 3D array."
  [x] 
  (call-fn 'index_set_3of3 x))

;; (def-unary-function int2float int2float
;;   "Function to coerce integers to floating point numbers")
;; (def-unary-function is_fixed is_fixed
;;   " function constraint")
;; (def-unary-function join join
;;   " function constraint")

(defn int2float
  "coerce int to float"
  [x] 
  (call-fn 'int2float x))
(defn is_fixed
  "As [[fix]], but return false if the argument’s value is not fixed."
  [x] 
  (call-fn 'is_fixed x))
(defn join
  "Concatenates an array of strings a, putting a seperator string s
  beween adjacent strings. Returns the empty string if the array is
  empty."
  [s a] 
  (call-fn 'join s a))

;; (def-unary-function lb lb
;;   " function constraint")
;; (def-unary-function lb_array lb_array
;;   " function constraint")

(defn lb
  "Domain reflection: a safe approximation to the lower bound value of
  x (int, float, or set of int)." 
  [x] 
  (call-fn 'lb x))
(defn lb_array
  "Domain reflection: a safe approximation to the lower bound of all
  expressions appearing in the array x (of int, float, or set of
  int)." 
  [x] 
  (call-fn 'lb_array x))

;; (def-unary-function length length
;;   "Returns the length of an array.")
;; (def-unary-function ln ln
;;   "natural logarithm constraint")
;; (def-unary-function log log
;;   " function constraint")
;; (def-unary-function log2 log2
;;   "logarithm base 2 constraint")
;; (def-unary-function log10 log10
;;   "logarithm base 10 constraint")

(defn length
  "Length of an array"
  [x] 
  (call-fn 'length x))
(defn ln
  "Natural logarithm"
  [x] 
  (call-fn 'ln x))
(defn log
  "General logarithm"
  [base x] 
  (call-fn 'log base x))
(defn log2
  "Logarithm base 2"
  [x] 
  (call-fn 'log2 x))
(defn log10
  "Logarithm base 10"
  [x] 
  (call-fn 'log10 x))

;; (def-unary-function max* max
;;   " function constraint")
;; (def-unary-function min* min
;;   " function constraint")
;; (def-unary-function product* product
;;   " function constraint")
;; (def-unary-function round round
;;   " function constraint")

(defn round
  "rounds float towards the the nearest integer"
  [x] 
  (call-fn 'round x))

;; (def-unary-function set2array set2array
;;   " function constraint")
;; (def-unary-function show show
;;   " function constraint")
;; (def-unary-function show_int show_int
;;   " function constraint")
;; (def-unary-function show_float show_float
;;   " function constraint")

(defn set2array
  "coerce set to array"
  [x] 
  (call-fn 'set2array x))
(defn show
  "To-string conversion. Converts any value to a string for output
  purposes. The exact form of the resulting string is
  implementation-dependent."
  [x] 
  (call-fn 'show x))
(defn show_int
  "Formatted to-string conversion for integers. Converts the integer
  `x` into a string right justified by the number of characters
  `justification` (int), or left justified if that argument is
  negative. If `x` is not fixed, the form of the string is
  implementation-dependent."
  [justification x] 
  (call-fn 'show_int justification x))
(defn show_float
  "Formatted to-string conversion for floats. Converts the float `x`
  into a string right justified by the number of characters given by
  `justification` (int), or left justified if that argument is
  negative. The number of digits to appear after the decimal point is
  given by `digits` (int). It is a run-time error for `digits` to be
  negative. If `x` is not fixed, the form of the string is
  implemenation-dependent."
  [justification digits x] 
  (call-fn 'show_float justification digits x))


;; (def-unary-function sin sin
;;   "sine constraint")
;; (def-unary-function sinh sinh
;;   "hyperbolic sine constraint")
;; (def-unary-function sqrt sqrt
;;   "square root constraint")
;; ;; (def-unary-function sum* sum
;; ;;   " function constraint")
;; (def-unary-function tan tan
;;   "tangent constraint")
;; (def-unary-function tanh tanh
;;   "hyperbolic tangent constraint")

(defn sin
  "Sine"
  [x] 
  (call-fn 'sin x))
(defn sinh
  "Hyperbolic sine"
  [x] 
  (call-fn 'sinh x))
(defn sqrt
  "Square root"
  [x] 
  (call-fn 'sqrt x))
(defn tan
  "Tangent"
  [x] 
  (call-fn 'tan x))
(defn tanh
  "Hyperbolic tangent"
  [x] 
  (call-fn 'tanh x))

;; (def-unary-function trace trace
;;   " function constraint")
(defn trace
  "Return x (any type). As a side-effect, an implementation may print
  the string s."
  [s x] 
  (call-fn 'trace s x))

;; (def-unary-function ub ub
;;   " function constraint")
;; (def-unary-function ub_array ub_array
;;   " function constraint")

(defn ub
  "Domain reflection: a safe approximation to the upper bound value of
  x (int, bool, float or set of int)."
  [x] 
  (call-fn 'ub x))
(defn ub_array
  "Domain reflection: a safe approximation to the upper bound of all
  expres- sions appearing in the array x x (of int, float, or set of
  int)."
  [x] 
  (call-fn 'ub_array x))

;; (def-binary-function pow pow
;;   "power constraint")

(defn pow
  "power: x^expt"
  [x expt] 
  (call-fn 'pow x expt))

(defn assert 
  "Constraint to guard against certain errors (e.g., to double-check
  input from data files). 

  mz-expr      (string) a MiniZinc expression returning a Boolean value
  error-msg    (string) an error message printed in case mz-expr is false

  BUG: mzn2fzn (version 1.6.0) detects inconsistency, but does not
  print the error message."
  [mz-expr, error-msg]
  (format "assert(%s, \"%s\")" (expr mz-expr) (expr error-msg)))

(comment

  ;; BUG: assert is still core/assert here, even though that has been excluded in ns macro
  ;; assert seems to be not a special form, so I should be able to overwrite it
  (print (assert (>= 'x 0) "some comment"))

  (def x (variable (-- -1 1) 'x))
  (def y (variable (-- -1 1) 'y))

  (not x)

  (+ x)
  (+ x y)
  (+ x 2)

  (<-> x y)
  (or x y)
  (print (and x y))

  (!= x 2)

  (pow 2 3)
  (print (pow 2 3))

  (constraint (!= x y))

  (print (output-map {:x x :y y}))

  (print 
   (clj2mnz
    (let [a (variable (-- -1 1)) 
          b (variable (-- -1 1))]
      (constraint (!= a b))
      (solve :satisfy)
      (output-map {:a a :b b})
      (pprint/pprint *mzn-store*)
      )))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global constraint defs
;;;

(defn alldifferent 
  "alldifferent(array[int] of var int: x)
  alldifferent(array[int] of var set of int: x)

  Constrains the array of objects x to be all different."
  [x]
  (call-global-constraint 'alldifferent x))

(comment
  (let [x (variable (-- 1 3))
        y (variable (-- 1 3))
        z (variable (-- 1 3))]
    (alldifferent [x y z]))
  )

(defn all_different 
  "Same as [[alldifferent]]."
  [x]
  (alldifferent x))

(defn alldifferent_except_0 
  "alldifferent_except_0(array[int] of var int: x) 

  Constrains the elements of the array x to be all different except
  those elements that are assigned the value 0."
  [x]
  (call-global-constraint 'alldifferent_except_0 x))

(defn all_disjoint 
  "all_disjoint(array[int] of var set of int: x)

  Ensures that every pair of sets in the array x is disjoint."
  [x]
  (call-global-constraint 'all_disjoint x))

(defn all_equal 
  "all_equal(array[int] of var int:          x) 
  all_equal(array[int] of var set of int:   x)

  Constrains the array of objects x to have the same value."
  [x]
  (call-global-constraint 'all_equal x))

(defn among 
  "among(var int: n, array[int] of var int: x, set of int: v)

  Requires exactly n variables in x to take one of the values in v."
  [n x v]
  (call-global-constraint 'among n x v))

(defn at_least 
  "at_least(int: n, array[int] of var int:        x, int:        v)
  at_least(int: n, array[int] of var set of int: x, set of int: v)

  Requires at least n variables in x to take the value v."
  [n x v]
  (call-global-constraint 'at_least x))

(defn atleast 
  "Same as [[at_least]]."
  [n x v]
  (at_least n x v))

(defn at_most
  "at_most(int: n, array[int] of var int:        x, int:        v)
  at_most(int: n, array[int] of var set of int: x, set of int: v)

  Requires at most n variables in x to take the value v."
  [n x v]
  (call-global-constraint 'at_most n x v))

(defn atmost
  "Same as [[at_most]]."
  [n x v]
  (at_most n x v))

(defn at_most1 
  "at_most1(array[int] of var set of int: s)

  Requires that each pair of sets in s overlap in at most one element."
  [s]
  (call-global-constraint 'at_most1 s))

(defn atmost1 
  "Same as [[at_most1]]."
  [s]
  (at_most1 s))

(defn bin_packing
  "bin_packing(int: c, array[int] of var int: bin, array[int] of int: w)

  Requires that each item i be put into bin bin[i] such that the sum of
  the weights of each item, w[i], in each bin does not exceed the
  capacity c. 
  Aborts if an item has a negative weight or if the capacity is negative.
  Aborts if the index sets of bin and w are not identical."
  [c bin w]
  (call-global-constraint 'bin_packing c bin w))

(defn bin_packing_capa 
  "bin_packing_capa(array[int] of int: c, array[int] of var int: bin, array[int] of int:w)

  Requires that each item i be put into bin bin[i] such that the sum
  of the weights of each item, w[i], in each bin b does not exceed the
  capacity c[b]. 
  Aborts if an item has negative weight.
  Aborts if the index sets of bin and w are not identical."
  [c bin w]
  (call-global-constraint 'bin_packing_capa c bin w))

(defn bin_packing_load 
  "bin_backing_load(array[int] of var int: l, array[int] of var int: bin, array[int] of int: w)

  Requires that each item i be put into bin bin[i] such that the sum
  of the weights of each item, w[i], in each bin b is equal to the
  load l[b]. 
  Aborts if an item has negative weight.
  Aborts if the index sets of bin and w are not identical."
  [l bin w]
  (call-global-constraint 'bin_packing_load l bin w))

(defn circuit
  "circuit[array[int] of var int: x)

  Constraints the elements of x to define a circuit where x[i] = j
  mean that j is the successor of i."
  [x]
  (call-global-constraint 'circuit x))

(defn count_eq
  "count_eq(array[int] of var int: x, var int: y, var int: c)

  Constrains c to be the number of occurrences of y in x.
  Also available by the name count." 
  [x y c]
  (call-global-constraint 'count_eq x y c))

(defn count
  "Same as [[count_eq]]."
  [x y c]
  (count_eq x y c))

(defn count_geq
  "count_geq(array[int] of var int: x, var int: y, var int: c)

  Constrains c to greater than or equal to the number of occurrences
  of y in x."
  [x y c]
  (call-global-constraint 'count_geq x y c))

(defn count_gt
  "count_gt(array[int] of var int: x, var int: y, var int: c)

  Constrains c to strictly greater than the number of occurrences of y
  in x." 
  [x y c]
  (call-global-constraint 'count_gt x y c))

(defn count_leq
  "count_leq(array[int] of var int: x, var int: y, var int: c)

  Constrains c to less than or equal to the number of occurrences of y
  in x."
  [x y c]
  (call-global-constraint 'count_leq x y c))

(defn count_lt
  "count_lt(array[int] of var int: x, var int: y, var int: c)

  Constrains c to strictly less than the number of occurrences of y in x."
  [x y c]
  (call-global-constraint 'count_lt x y c))

(defn count_neq
  "count_neq(array[int] of var int: x, var int: y, var int: c)

  Constrains c to not be the number of occurrences of y in x."
  [x y c]
  (call-global-constraint 'count_neq x y c))

(defn cumulative
  "cumulative(array[int] of var int: s, array[int] of var int: d, array[int] of var int: r, var int: b)

  Requires that a set of tasks given by start times s, durations d,
  and resource requirements r, never require more than a global
  resource bound b at any one time. 
  Aborts if s, d, and r do not have identical index sets.
  Aborts if a duration or resource requirement is negative."
  [s d r b]
  (call-global-constraint 'cumulative s d r b))

(defn decreasing
  "decreasing(array[int] of var bool:       x)
  decreasing(array[int] of var float:      x)
  decreasing(array[int] of var int:        x)
  decreasing(array[int] of var set of int: x)
  
  Requires that the array x is in (non-strictly) decreasing order."
  [x]
  (call-global-constraint 'decreasing x))

(defn diffn
  "diffn(array[int] of var int: x,  array[int] of var int: y,
      array[int] of var int: dx, array[int] of var int: dy)

  Constrains rectangles, given by their origins x,y and sizes dx,dy, to be non-overlapping."
  [x y dx dy]
  (call-global-constraint 'diffn x y dx dy))

(defn disjoint
  "disjoint(var set of int: s, var set of int: t)
  
  Requires that sets s and t do not intersect."
  [s t]
  (call-global-constraint 'disjoint s t))

(defn distribute
  "distribute(array[int] of var int: card, array[int] of var int: value, array[int] of var int: base)

  Requires that card[i] is the number of occurrences of value[i] in base.
  In this implementation the values in value need not be distinct.
  Aborts if card and value do not have identical index sets."
  [card value base]
  (call-global-constraint 'distribute card value base))

(defn element
  "element(var int: i, array[int] of var bool:       x, var bool:       y)
  element(var int: i, array[int] of var float:      x, var float:      y)
  element(var int: i, array[int] of var int:        x, var int:        y)
  element(var int: i, array[int] of var set of int: x, var set of int: y)
  
  The same as x[i] = y or (= (nth x i) y). That is, y is the ith
  element of the array x. The difference to nth is that i can be a
  variable."
  [i x y]
  (call-global-constraint 'element i x y))

(defn exactly
  "exactly(int: n, array[int] of var int:        x, int:        v)
  exactly(int: n, array[int] of var set of int: x, set of int: v)

  Requires exactly n variables in x to take the value v."
  [n x v]
  (call-global-constraint 'exactly n x v))

(defn global_cardinality
  "global_cardinality(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts)

  Requires that the number of occurrences of cover[i] in x is counts[i].
  Aborts if cover and counts do not have identical index sets."
  [x cover counts]
  (call-global-constraint 'global_cardinality x cover counts))

(defn global_cardinality_closed
  "global_cardinality_closed(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts)

  Requires that the number of occurrences of cover[i] in x is counts[i].
  The elements of x must take their values from cover.
  Aborts if cover and counts do not have identical index sets."
  [x cover counts]
  (call-global-constraint 'global_cardinality_closed x cover counts))

(defn global_cardinality_low_up
  "global_cardinality_low_up(array[int] of var int: x, array[int] of int: cover, array[int] of int: lb, array[int] of int: ub)

  Requires that for all i, the value cover[i] appears at least lb[i]
  and at most ub[i] times in the array x."
  [x cover lb ub]
  (call-global-constraint 'global_cardinality_low_up x cover lb ub))

(defn global_cardinality_low_up_closed
  "global_cardinality_low_up_closed(array[int] of var int: x, array[int] of int: cover, array[int] of int: lb, array[int] of int: ub)

  Requires that for all i, the value cover[i] appears at least lb[i]
  and at most ub[i] times in the array x. 
  The elements of x must take their values from cover."
  [x cover lb ub]
  (call-global-constraint 'global_cardinality_low_up_closed x cover lb ub))

(defn increasing
  "increasing(array[int] of var bool:       x)
  increasing(array[int] of var float:      x)
  increasing(array[int] of var int:        x)
  increasing(array[int] of var set of int: x)
  
  Requires that the array x is in (non-strictly) increasing order."
  [x]
  (call-global-constraint 'increasing x))

(defn int_set_channel
  "int_set_channel(array[int] of var int: x, array[int] of var set of int: y)

  Requires that x[i] = j if and only if i is an element of y[j]."
  [x y]
  (call-global-constraint 'int_set_channel x y))

(defn inverse
  "inverse(array[int] of var int: f, array[int] of var int: invf)

  Constrains two arrays to represent inverse functions of each
  other. All the values in each array must be within the index set of
  the other array." 
  [f invf]
  (call-global-constraint 'inverse f invf))

(defn inverse_set
  "inverse_set(array[int] of var set of int: f, array[int] of var set of int: invf)

  Constrains the two arrays f and invf so that a j is an element of
  f[i] if and only if i is an element of invf[j]. All the values in
  each array's sets must be within the index set of the other array."
  [f invf]
  (call-global-constraint 'inverse_set f invf))

(defn lex_greater
  "lex_greater(array[int] of var bool:       x, array[int] of var bool:       y)
  lex_greater(array[int] of var float:      x, array[int] of var float:      y)
  lex_greater(array[int] of var int:        x, array[int] of var int:        y)
  lex_greater(array[int] of var set of int: x, array[int] of var set of int: y)

  Requires that the array x is strictly lexicographically greater than
  array y. Compares them from first to last element, regardless of indices."
  [x y]
  (call-global-constraint 'lex_greater x y))

(defn lex_greatereq
  "lex_greatereq(array[int] of var bool:       x, array[int] of var bool:       y)
  lex_greatereq(array[int] of var float:      x, array[int] of var float:      y)
  lex_greatereq(array[int] of var int:        x, array[int] of var int:        y)
  lex_greatereq(array[int] of var set of int: x, array[int] of var set of int: y)
  
  Requires that the array x is lexicographically greater than or equal to array y.
  Compares them from first to last element, regardless of indices." 
  [x y]
  (call-global-constraint 'lex_greatereq x y))

(defn lex_less
  "lex_less(array[int] of var bool:       x, array[int] of var bool:       y)
  lex_less(array[int] of var float:      x, array[int] of var float:      y)
  lex_less(array[int] of var int:        x, array[int] of var int:        y)
  lex_less(array[int] of var set of int: x, array[int] of var set of int: y)
  
  Requires that the array x is strictly lexicographically less than
  array y. Compares them from first to last element, regardless of indices."
  [x y]
  (call-global-constraint 'lex_less x y))

(defn lex_lesseq
  "lex_lesseq(array[int] of var bool:       x, array[int] of var bool:       y)
  lex_lesseq(array[int] of var float:      x, array[int] of var float:      y)
  lex_lesseq(array[int] of var int:        x, array[int] of var int:        y)
  lex_lesseq(array[int] of var set of int: x, array[int] of var set of int: y)
  
  Requires that the array x is lexicographically less than or equal to array y.
  Compares them from first to last element, regardless of indices."
  [x y]
  (call-global-constraint 'lex_lesseq x y))

(defn lex2
  "lex2(array[int, int] of var int: x)

  Require adjacent rows and adjacent columns in the the array x to be
  lexicographically ordered. Adjacent rows and adjacent columns may be
  equal."
  [x]
  (call-global-constraint 'lex2 x))

(defn link_set_to_booleans
  "link_set_to_booleans(var set of int: s, array[int] of var bool: b)

  The array of booleans b is the characteristic representation of the set s.
  Aborts if the index set of b is not a superset of the possible values of s."
  [s b]
  (call-global-constraint 'link_set_to_booleans s b))

(defn maximum
  "maximum(var int:   m, array[int] of var int:   x)
  maximum(var float: m, array[int] of var float: x)

  Constrains m to be the maximum of the values in x. (The array x must
  have at least one element.)"
  [m x]
  (call-global-constraint 'maximum m x))

(defn member
  "member(array[int] of var bool:       x, var bool:       y)
  member(array[int] of var float:      x, var float:      y)
  member(array[int] of var int:        x, var int:        y)
  member(array[int] of var set of int: x, var set of int: y)
  member(var set of int:               x, var int:        y)

  Requires that y occurs in the array or set x."
  [x y]
  (call-global-constraint 'member x y))

(defn minimum
  "minimum(var float: m, array[int] of var float: x)
  minimum(var int:   m, array[int] of var int:   x)
  
  Constrains m to be the minimum of the values in x. (The array x must
  have at least one element.)"
  [m x]
  (call-global-constraint 'minimum m x))

(defn nvalue
  "nvalue(var int: n, array[int] of var int: x)

  Requires that the number of distinct values in x is n."
  [x]
  (call-global-constraint 'nvalue x))

(defn partition_set
  "partition_set(array[int] of var set of int: s, set of int: universe)

  Partitions universe into disjoint sets."
  [s universe]
  (call-global-constraint 'partition_set s universe))

(defn range
  "range(array[int] of var int: x, var set of int: s, var set of int: t)

  Requires that the image of function x (represented as an array) on set of values s is t.
  Aborts if ub(s) is not a subset of the index set of x."
  [x s t]
  (call-global-constraint 'range x s t))

(defn regular
  "regular(array[int] of var int: x, int: Q, int: S, array[int,int] of int: d, int: q0, set of int: F)

  The sequence of values in array x (which must all be in the range
  1..S) is accepted by the DFA of Q states with input 1..S and
  transition function d (which maps ⟨1..Q, 1..S⟩ to 0..Q) and initial
  state q0 (which must be in 1..Q) and accepting states F (which all
  must be in 1..Q). State 0 is reserved to be an always failing
  state.
  Aborts if Q < 1.
  Aborts if S < 1.
  Aborts if the transition function d is not in [1..Q, 1..s].
  Aborts if the start state, q0, is not in 1..Q.
  Aborts if F is not a subset of 1..Q."
  [x Q S d q0 F]
  (call-global-constraint 'regular x Q S d q0 F))

(defn roots
  "roots(array[int] of var int: x, var set of int: s, var set of int: t)

  Requires that x[i] is an element of t for all i element of s.
  Aborts if ub(s) is not a subset of the index set of x."
  [x s t]
  (call-global-constraint 'roots x s t))

(defn sliding_sum
  "sliding_sum(int: low, int: up, int: seq, array[int] of var int: vs)

  Requires that in each subsequence vs[i], ..., vs[i + seq - 1] the
  sum of the values belongs to the interval [low, up]."
  [low up seq vs]
  (call-global-constraint 'sliding_sum low up seq vs))

(defn sort
  "sort(array[int] of var int: x, array[int] of var int: y)

  Requires that the multiset of values in x is the same as the
  multiset of values in y but y is in sorted order. 
  Aborts if the cardinality of the index sets of x and y is not equal." 
  [x y]
  (call-global-constraint 'sort x y))

(defn strict_lex2
  "strict_lex2(array[int, int] of var int: x)

  Require adjacent rows and adjacent columns in the the array x to be
  lexicographically ordered. Adjacent rows and adjacent columns cannot
  be equal."
  [x]
  (call-global-constraint 'strict_lex2 x))

(defn subcircuit
  "subcircuit(array[int] of var int: x)

  Constrains the elements of x to define a subcircuit where x[i] = j
  means that j is the successor of i and x[i] = i means that i is not
  in the circuit."
  [x]
  (call-global-constraint 'subcircuit x))

(defn sum_pred
  "sum_pred(var int: i, array[int] of set of int: sets, array[int] of int: c, var int: s)

  Requires that the sum of c[i1]...c[iN] equals s, where i1..iN are
  the elements of the ith set in sets. This constraint is usually
  named sum, but using that would conflict with the MiniZinc built-in
  function of the same name."   
  [i sets c s]
  (call-global-constraint 'sum_pred i sets c s))

(defn table
  "table(array[int] of var bool: x, array[int, int] of bool: t)
table(array[int] of var int:  x, array[int, int] of int:  t)

  Represents the constraint x is element of t where we consider each
  row in t to be a tuple and t as a set of tuples.
  Aborts if the second dimension of t does not equal the number of
  variables in x. 
  The default decomposition of this constraint cannot be flattened if
  it occurs in a reified context."
  [x t]
  (call-global-constraint 'table x t))

(defn value_precede
  "value_precede(int: s, int: t, array[int] of var int: x)
  value_precede(int: s, int: t, array[int] of var set of int: x)

  Requires that s precede t in the array x.
  For integer variables this constraint requires that if an element of
  x is equal to t, then another element of x with a lower index is
  equal to s.
  For set variables this constraint requires that if an element of x
  contains t but not s, then another element of x with lower index
  contains s but not t."
  [s t x]
  (call-global-constraint 'value_precede s t x))

(defn value_precede_chain
  "value_precede_chain(array[int] of int: c, array[int] of var int: x)
  value_precede_chain(array[int] of int: c, array[int] of var set of int: x)

  Requires that the [[value_precede]] constraint is true for every pair
  of adjacent integers in c in the array x."
  [c x]
  (call-global-constraint 'value_precede_chain c x))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Annotation
;;;

(comment

  ;; ;; Several expressions/statements to support
  ;; ;; annotations have ann type

  ;; "Annotations allow a modeller to specify non-declarative and solver-specific information that is beyond the core language. Annotations do not change the meaning of a model, however, only how it is solved."
  
  ;; ;; -> Does that mean I have to revise pretty much all definitions to allow for them?
  
  ;; ;; annotation declaration
  ;; annotation bitdomain(int:nwords);

  ;; ;; annotations added to various other language constructs with :: operator
  ;; ;; Annotations can be attached to variables (on their declarations), expresssions, type-inst synonyms, enum items, solve items and on user defined operations.
  ;; search annotations
  ;; constraint annotations
  ;; added to variable or parameter declarations (e.g., to indicate special kind of variables)
  ;; int: x::foo;
  ;; x = (3 + 4)::bar("a", 9)::baz("b");


  ;; Annotation has quasi function syntax: `name(arg1, arg2, ...)`, which can be also only the `name`  

  ;; Possibly tricky: how to quasi integrate into existing calls with :: operator -- do I need to rewrite lots of definitions?
  ;; -> only if they introduce `\;` at end, and I would have to add annotation before that

  ;; !! Most important are solver annotations for now, perhaps I only add those?


  ;; for annotation declaration 
  (defn annotation
    []
    )


  ;; [:int_search q :first_fail :indomain_min :complete]

  ;; (annotation )



  )

;; !! TODO: def annotation arg for variable, array, constraint
(defn __
  "Speficies an annotation -- the :: operator in MiniZinc. 

  Note that many functions (bool, int, float, set, array, variable, constraint, solve)
  support annotations as arguments. Use this function for annotating inner 
  expressions.

  Note that solvers are free to ignore any or all annotations in a model."
  [expr annotation-name & args]
  (str expr " :: " (if args
                     (apply call-fn annotation-name args)
                     annotation-name)))

(comment
  (__ (__ (+ 1 2) 'foo (string 'a) :b) 'bar)
  (__ (+ 1 2) 'test_ann)
  (__ (+ 1 2) (search-annotation :int 'x :first_fail :indomain_min :complete))
  
  )

;; TODO: add support for seq_search, e.g., by extra fn
;; TODO: What about type float? Not mentioned in MiniZinc not FlatZinc doc
(defn search-annotation
  "Defines standard search annotation without extensions such as proposed, 
  e.g., by [1]).
  
  Note that solvers are free to ignore annotations in a model, but it is 
  recommended that solvers at least recognise these search annotations,
  in particular the starred options.
  
  Arguments:

  - type: the type of the `variables`; either :int, :bool, or :set
  - variables: a one dimensional array of variables 
  - varchoice: a variable choice annotation; either 
    - :input_order -- * choose in order from the array
    - :first_fail -- * choose the variable with the smallest domain size
    - :anti_first_fail -- choose the variable with the largest domain
    - :smallest -- choose the variable with smallest value in its domain
    - :largest -- choose the variable with largest value in its domain
    - :occurrence -- choose the variable with the largest number of attached
      constraints
    - :most_constrained -- choose the variable with the smallest domain, 
      breaking ties using the number of constraints
    - :max_regret -- choose the variable with the largest difference between
      the two smallest values in its domain
  - constrainchoice: a choice of how to constrain a variable
    - :indomain_min -- * assign the variable its smallest domain value
    - :indomain_max -- * assign the variable its largest domain value
    - :indomain_middle -- assign the variable its domain value closest
      to the mean of its current bounds
    - :indomain_median -- assign the variable its median domain value
    - :indomain -- nondeterministically assign values to the variable in 
      ascending order
    - :indomain_random -- assign the variable a random value from its domain
    - :indomain_split -- bisect the variables domain excluding the upper half.
    - :indomain_reverse_split -- bisect the variables domain excluding the 
      lower half.
    - :indomain_interval -- if the variable’s domain consists of several 
      contiguous intervals, reduce the domain to the first interval. Otherwise 
      just split the variable’s domain.
  - strategy: a search strategy; solvers should at least support :complete 
    (i.e., exhaustive search)

  Of course, not all assignment strategies make sense for all search 
  annotations (e.g., bool search and indomain split).

  
  Example:

  ;; x is an array of int vars
  (search-annotation :int x :first_fail :indomain_min :complete)


  References:

  [1] Schrijvers, T. et al. (2013) 'Search combinators'. Constraints. 18(2), 269–305."
  ;; [type variables varchoice constrainchoice strategy]
  [type variables & {:keys [varchoice constrainchoice strategy]
                     :or {varchoice :first_fail constrainchoice :indomain_median strategy :complete}}]
  (let [search-type (str (name type) "_search")]
    (call-fn search-type variables varchoice constrainchoice strategy)))

(comment
  (search-annotation :int 'x)
  (search-annotation :int 'x
                     :varchoice :occurrence
                     :constrainchoice :indomain_random)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Solver
;;;

(defn solve 
  "Solve items specify what kind of solution is being looked for. 

  Arguments:

  - solver -- determines whether the model represents a constraint satisfaction 
    problem (:satisfy) or an optimisation problem (either :maximize, or 
    :minimize). In the latter case, argument `expr` must be given.
  - expr -- expression to be minimized/maximized; can have integer or 
    float type.
  - :ann -- a search annotation created with `search-annotation` or a 
    similar function.

  Examples:

  (solve :satisfy)
  (solve :maximize (+ x y))  
  (solve :satisfy
         :ann (search-annotation :int x))

  "
  ;; No standard notation for :arglists
  ;; Inspiration for this simple form from http://grokbase.com/t/gg/clojure/11cxesejc1/arglists-question
  {:arglists '([solver expr? :ann])}
  [& all-args]
  (let [solver (first all-args)
        second-arg (second all-args)
        expr (if (core/and second-arg (core/not= second-arg :ann))
               second-arg
               nil)
        ann (if expr
              (:ann (apply hash-map (rest (rest all-args))))
              (:ann (apply hash-map (rest all-args))))
        solver-call (str/join " " (map str [(name solver) expr]))]
    (core/assert (if expr
                   (#{:maximize :minimize} solver)
                   (#{:satisfy} solver)))
    (tell-store! :solve
                 (if ann
                   (format "solve :: %s\n  %s;" ann solver-call)
                   (format "solve %s;" solver-call)))))

  
(comment
  (solve :satisfy)
  (solve :maximize (+ 'x 'y))
  (solve :satisfy
         :ann (search-annotation :int 'x))
  (solve :maximize (+ 'x 'y)
         :ann (search-annotation :int 'x
                                 :varchoice :occurrence
                                 :constrainchoice :indomain_random))

  ;; error
  (solve :foo)   
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Output
;;;


;; NB: discussion of mzn_show.pl at http://www.hakank.org/minizinc/ :
;; Since version 1.0 MiniZinc don't support the output [] anymore in the external solvers (e.g. all except the minizinc solver)

;; TMP: def until a more general function output that translates arbitrary data structures containing variables is defined.
;; ??? TODO: replace by more general variant that supports arbitrary Clojure data strutures.
;; NOTE: “fancy” output can be (very) slow (http://web.it.kth.se/~cschulte/events/SweConsNet-2012/hakan.pdf)
(defn output-map
  "Expects a map containing MiniZinc variables and returns a string
  formatted for MiniZinc to output a Clojure map for Clojure to read."
  [my-map]
  (tell-store!
   :output
   (str "output [\"{\", " 
        ;; BUG: of REPL? Strings containing parentheses can cause blocking.
        ;; waiting for a response at https://groups.google.com/forum/#!forum/clojure-tools
        (apply str (doall (map (fn [[key val]] (str "\" " key " \"" ", show(" (expr val) "), ")) my-map))) 
        "\"}\\n\"];")))

(comment
  (def x (variable (-- 1 3) 'x))
  (def y (variable (-- 4 6) 'y))
  (print (output-map {:x x :y y}))
  )

(defn output-vector
  "Expects a vector of MiniZinc variables and returns a string
  formatted for MiniZinc to output a Clojure vector for Clojure to
  read."
  [my-vec]
  (tell-store!
   :output
   (str "output [\"[\", " 
        ;; BUG: of REPL? Strings containing parentheses can cause blocking.
        ;; waiting for a response at https://groups.google.com/forum/#!forum/clojure-tools
        (apply str (interpose ", " (map (fn [x] (format "show(%s)" (expr x))) my-vec))) 
        " \"]\\n\"];")))

(comment
  (def x (variable (-- 1 3) 'x))
  (def y (variable (-- 4 6) 'y))
  (print (output-vector [x y]))
  )

(defn output-var 
  "Outputs a single MiniZinc variable. For example, a one-dimensional
  MiniZinc array can be read into a Clojure vector directly."
  [my-var]
  (tell-store! :output (format "output [ show(%s) ];" (expr my-var))))

;; TODO: output for multi-dimensional vector

;; TODO: make this somehow more smart by allowing for vars etc
;; e.g., at least allow for any number of args
(defn output 
  "Expects an output definition (a string) and turns it into an output statement (surround by brackets etc.)

BUG: this fn is currently far too inflexible."
  [mzn-string]
  (tell-store! :output (format "output [ %s ];" (expr mzn-string)))) ; \"\\n\"

(comment
  (print (output "x = show(a)"))
  )

(comment
  ;; TODO: finish definition
  ;; TODO: then revise definition such that it always results in a string expressing a clojure value such as a map.
  ;; Idea: input is also a map, where the values at keys are variables, or some other clojure data structure containing variables. This data structure is then expressed as a string.
  (defn output-clj
    "Output items are for nicely presenting the results of the model
  execution. Output expects any number of strings or variables."
    [arg]
    arg)

  ;; TODO: old test
  (output "this" "is" "a" "test")

  (def x (variable (domain 1 3) 'x))
  (def y (variable (domain 4 6) 'y))

  (type x)
  (aVar? x)
  (:name x)

  (name :x)

  (walk/walk expr identity
             {:x x :y y})


  (walk/walk #(name-or-val %)
             identity
             {:x x :y y})
  )


(defn map2minizinc 
  "Utility function for creating data files (*.dzn files) that map
  keys (MiniZinc variable names) to values.
  
  BUG: only few value types supported."
  [mzn-map]
  (apply str (map (fn [[key val]] (str (format "%s = %s" (expr key) (expr val)) "; "))
                  mzn-map)))

(comment
  (map2minizinc {:x 1 :y 2 :z 3})
  (map2minizinc {:x [1, 2] :y (literal-array [1 2][3 4]) :z ["a" "b" "c"]})

  ;; BUG: expr must support more types or map2minizinc must translate them 
  (expr [1, 2])
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Communicating with MiniZinc 
;;;

;; TODO: this will likely not work as a function, because given function calls are evaluated outside the necessary dynamic scope. Nevertheless, try before using a macro instead
;; TODO: Possibly I late embed this in minizinc below? Then it needs to become a macro itself.
;; TODO: should macros be defined elsewhere as caution?
(let [process-store-vector (fn [x]  
                             (str (cond (core/or (aVar? x) (anArray? x))
                                        (:mzn-string x)
                                        (core/string? x) x
                                        :else (throw (Exception. (pprint/cl-format nil "~S not supported. MiniZinc statements must be strings or variables defined with function clojure2minizinc.core/variable." x)))) 
                                  "\n"))]
  (defmacro clj2mnz 
    "Translates a constraint problem defined in Clojure into the
  corresponding MiniZinc code. Expects any number of
  variable/parameter declarations, any number of constraints, one
  output, and one solver declaration, all in any order."
    [& constraints]
    `(binding [*mzn-store* {:include []
                            ;; :predicate []
                            :parameter []
                            :variable []
                            ;; :constraint []
                            ;; predicates and constraints 
                            :other []
                            :solve []
                            :output []}
               *included-files* #{}
               *defined-predicates* #{}]
       ~@constraints
       ;; TODO: map is lazy -- make sure dynamic scope is not left
       (apply str (map (fn [key#] (apply str (doall (map ~process-store-vector (key# *mzn-store*)))))
                       [:include :parameter :variable :other :solve :output]))
       ;; (apply str (doall (map process-store-vector *mzn-store*)))
       )))

(comment
  ;; minimum CSP
  ;; TODO: add output
  ;; TODO: try also macroexpand 
  (print
   (clj2mnz
    (let [a (variable (-- 1 3) 'a) ;; mzn var naming redundant, but ensures var name in *.mzn file
          b (variable (-- 1 3) 'b)]
      (output-map {:a a :b b})
      (solve :satisfy)
      (constraint (!= a b))
      (pprint/pprint *mzn-store*)
      )))

  )



;; ?? TODO: incorporate clj2mnz into minizinc (turning minizinc into a macro)? Perhaps having it separate is a good idea? Makes call more structured. 
;; TODO: Add solver arguments: parallel (unrecognized for mzn-g12fd), random-seed, solver-backend, flatzinc-flags (?), keep-files, ... 
(defn minizinc 
  "Calls a MiniZinc solver on a given MiniZinc program and returns a
  list of one or more solutions.
  
  Arguments are
  
  - mzn             (string) a MiniZinc program, which can be created
    with other functions of clojure2minizinc wrapped into clj2mnz 
  - :print-cmd?      (boolean) whether or not to print the UNIX
    command call of the solver (for debugging) 
  - :print-mzn?      (boolean) whether or not to print resulting
    MiniZinc data and model (for debugging) 
  - :print-solution? (boolean) whether or not to print the result
    output by MiniZinc directly instead of reading it into a Clojure
    value (e.g., for debugging). Prints the map resulting from
    clojure.java.shell/sh. 
  - :solver          (string) solver to call
  - :mznfile         (string or file) MiniZinc file path to generate
    and use in the background 
  - :data            (string) Content for a MiniZinc data file (*.dzn
    file). Can conveniently be created with map2minizinc  
  - :num-solutions   (int) An upper bound on the number of solutions to output
  - :all-solutions   (boolean) If true, return all solutions
  - :options         (collection of strings) Arbitrary options given
    to the solver in UNIX shell syntax, e.g., [\"-a\"] for all
    solutions. 

  BUG: MiniZinc sets output cannot be read yet.
  BUG: resulting temporary MiniZinc file is not deleted after Clojure quits."
  [mzn & {:keys [solver mznfile data
                 print-cmd?
                 print-mzn?
                 print-solution? 
                 num-solutions all-solutions?
                 options] 
          :or {solver *fd-solver*
               ;; BUG: tmp file not deleted later
               ;; TODO: delete directly after done witgh fn .delete (see Clojure Cookbook, Sec. 4.10)
               mznfile (doto (java.io.File/createTempFile "clojure2minizinc" ".mzn") .deleteOnExit)
               data false
               print-cmd? false
               print-mzn? false
               print-solution? false
               num-solutions 1
               all-solutions? false
               options []}}]
  (when print-mzn? 
    (do (when data (do (println "% data:") (println data) (println "% model:")))  (println mzn)))
  (spit mznfile mzn)
  ;; mznfile split into filename (base-name) and dirname (parent), so that shell/sh first moves into that dir, because otherwise I got errors from *fd-solver*
  (let [sh-args-1 (core/concat [solver]
                               [(if all-solutions?
                                  "--all-solutions"
                                  ;; I could not get long parameter names working 
                                  (format "-n%s" num-solutions))]
                               options
                               [(fs/base-name mznfile)])
        sh-args (core/concat sh-args-1
                             (if data
                               [(format "-D%s" data)]
                               [])
                             [:dir (fs/parent mznfile)])
        ;; dummy (pprint/pprint sh-args)
        result (apply shell/sh sh-args)]
    (when print-cmd? 
      (do (println (format "cd %s" (fs/parent mznfile)))
          (println (apply str (interpose " " (core/concat sh-args-1
                                                          (if data 
                                                            [(format "-D'%s'" data)]
                                                            [])))))))
    (if print-solution?
      (pprint/pprint result)
      (if (core/= (:exit result) 0)
        (do 
          ;; TODO: this is not yet a clean solution
          ;; In case there is an error as part of a warning then show that. How can I show a warning and still return the final result?
          (if (core/not= (:err result) "")
            (throw
             (ex-info (format "MiniZinc: %s" (:err result)) {:type :minizinc-warning :cause :minizinc-warning})
             ;; (Exception. (format "MiniZinc: %s" (:err result)))
             ))
          ;; BUG: MiniZinc sets output cannot be read yet. Should I define a bespoke parser generator?
          ;; Old TODO: Consider replacing read-string with clojure.edn/read-string or read 
          ;; See, e.g., Clojure Cookbook, sec. 4.14: "use read to read large data structures from a stream"
          ;; and later secs. e.g., 4.15, 4.16, 4.17
          (map read-string
               (if on-windows?
                 ;; Take Windows vs. UNIX line break differences into account
                 (clojure.string/split (:out result) #"(\r\n----------\r\n|==========\r\n)")
                 (clojure.string/split (:out result) #"(\n----------\n|==========\n)"))))
        (throw (ex-info (format "MiniZinc: %s" (:err result)) {:type :minizinc-error :cause :minizinc-error})
               ;; (Exception. (format "MiniZinc: %s" (:err result)))
               )))))



(comment
  ;; mini example
  (minizinc
   (clj2mnz
    (let [a (variable (-- -1 1)) 
          b (variable (-- -1 1))]
      (constraint (!= a b))
      (solve :satisfy)
      (output-map {:a a :b b})
      ;; (pprint/pprint *mzn-store*)
      ))
   ;; :print-mzn? true
   :num-solutions 3
   ;; :all-solutions? true
   )
  )


(comment

  (defn read-mzn-result
    []
    (cond true 1))
  
  
  (read-string "[15, 16, 18, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21]")
  ;; NOTE: Reading a set does not work
  (read-string "{15,16,18}")

  
  )

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Abstractions like predicates 
;;;

;; TODO: 
;; - Make the doc more easy to comprehend.
;; - Consider renaming
;; !? BUG: Should clj2mnz be really called within def?
(defmacro def-submodel
  "Abstracts a part of a MiniZinc model as a Clojure function. Such
  submodels can be used as a substitute for MiniZinc predicates and
  functions, but calls to any submodel are resolved in the actual
  MiniZinc code. A submodel call returns a (MiniZinc code) string. 

  BUG: Unfinished def -- does not yet support the full set of
  arguments of clojure.core/defn. (e.g., no doc string.)"
  [name args & body]
  ;; *mzn-store* is false outside of a call to mz/clj2mnz
  `(def ~name
     (fn ~args
       (clj2mnz
        ;; !! NOTE: I do not know/check whether a contraint or something else is told.
        ;; OK here, but needs to be revised for flatzinc output
        (tell-store! :other ~@body)))))



(defmacro predicate
  "A MiniZinc predicate: defines a Clojure function that can
  be called as a constraint, and generates the necessary MiniZinc code. 

  `args-with-type-insts` is quasi a vector of typed arguments consisting 
  of pairs of the variable name and a type-inst. Each type-inst is either a 
  single specification or a vector of specifications.

  `name` is the name of the resulting Clojure function and MiniZinc 
  predicate, and `body` is expressions that the predicate executes. 


  Example of a predicate definition and its call:

  (predicate my_less_than
   \"Less than constraint\"
    [x [:var :int]
     y [:var :int]]
    (< x y))
  (constraint (my_less_than a b))


  Type-inst examples:

  :bool
  (-- 1 3)
  #{1 3 5}
  [:var :int]
  [:var #{1 3 5}]
  [:set :int]
  [:set (-- 1 3)]
  [:var :set :int]

  The keyword :of is syntactic sugar for readability, e.g., in set 
  declarations. The following two type-insts are equivalent.

  [:var :set :int]
  [:var :set :of :int]

  The index type of an array first specifies the array’s index set.
  The type of the contained values is specified with a nexted type-inst.

  [:array (-- 1 3) [:var :int]]
  [:array :int [:var :int]]"
  {; :forms '[(predicate name doc-string? args-with-type-insts body*)]
   :arglists '([name doc-string? args-with-type-insts body*])
   :style/indent [1 [[:defn]] :form]}
  [& all-args]
  (let [;; access arguments from all-args
        name (first all-args)
        ;; name-kw (keyword name)
        doc-string? (core/string? (second all-args))
        doc-string (if doc-string? (second all-args) "Not documented.")
        all-args (if doc-string? (next all-args) all-args)
        args-with-type-insts (second all-args)
        body (next (next all-args))
        ;; partition only once
        partitioned-args (partition 2 args-with-type-insts)
        ;; Vector of arguments without type-insts
        args (apply vector (map #'first partitioned-args))
        ;; Argument strings: avoid evaluation of args in body of predicate 
        args-str (map str args)]
    `(let [;; Args in mzn syntax
           mzn-args-string# (apply str (interpose ", " (map (fn [[my-var# type-inst#]]
                                                              (str (mk-type-inst-string type-inst#) ": " my-var#))
                                                            '~partitioned-args)))]
       ;; Constraint function
       (defn ~name ~doc-string ~args
         ;; function class ensures that predicate definition is included in resulting mzn code
         (when (core/not (in? *defined-predicates* '~name))
           (set! *defined-predicates* (conj *defined-predicates* '~name))
           ;; generation of predicate definition code:
           ;; broken into multiple tell-store! calls to preserve order in case tell-store!
           ;; is called in `body` (if it is a single call then nested calls e.g. in `local`
           ;; would be executed earlier resulting in a scrambled order of code lines.
           ;; BUG: How to ensure the middle tell-store! does not call `constraint` or some other function causing this codeline ending up outside the predicate
           (tell-store! :other (str "predicate " '~name "(" mzn-args-string# ") ="))
           (tell-store! :other (apply (fn ~args (str/join "\n  " (list ~@body))) '~args-str))
           (tell-store! :other ";")
           ;; (tell-store! 
           ;;  (str "predicate " '~name "(" mzn-args-string# ") =\n" 
           ;;       "  " (apply (fn ~args (str/join "\n  " (list ~@body))) '~args-str)
           ;;       ";"))
           )
         ;; Return function call code
         (str '~name "("
              (str/join ", " (map name-or-val ~args))
              ")")) 
       ;; Return name of predicate
       '~name)))

;; !? TODO: add type-inst spec in :bindings, once that is working
(spec/def ::predicate (spec/cat :name symbol?
                                :doc-string (spec/? core/string?)
                                :bindings (spec/spec (spec/+ (spec/cat :var symbol? :type-inst any?)))
                                :body (spec/+ any?)))
(spec/fdef predicate :args ::predicate)
(spectest/instrument `predicate)
;; (spectest/unstrument `predicate)



(comment

  (in? (core/conj *defined-predicates* 'test-pred) 'test-pred)

  (print
   (clj2mnz
    (predicate my_less_than
      [x [:var :int]
       y [:var :int]]
      (< x y))
    (let [x (variable (-- -1 1)) 
          y (variable (-- -1 1))]
      (constraint (my_less_than 2 x))
      (solve :satisfy)
      (output-map {:x x :y y}))))

  ;; predicate body with more than one expr
  (print
   (clj2mnz 
    (predicate my_less_than
      "This is a test doc-string"
      [x [:var :int]
       y [:var :int]]
      (< x y)
      (= x 1))
    (constraint (my_less_than 1 2)))) 

  
  (clj2mnz 
   (predicate my_less_than
     [x :float
      y :int]
     (< x y)))

  
  ;; (print
  ;;  (clj2mnz
  ;;   (predicate my_test
  ;;     [x [:var :int]
  ;;      y [:array :int [:var :int]]]
  ;;     (< x (nth y 0)))
  ;;   (let [x (variable (-- -1 1)) 
  ;;         y (array (-- 1 10) (-- 0 10))]
  ;;     (constraint (my_test x y))
  ;;     (solve :satisfy)
  ;;     (output-map {:x x :y y}))))
  

  (predicate alldifferent_except_x
             ;; Array declaration more similar to args of function array, but now all expressed with keywords
             [x [:var :int]
              y [:array :int [:var :int]]]
    (forall [i (index_set y)
                j (index_set y)
                :where (!= i j)]
      (-> (and (!= (nth y i) x)
                     (!= (nth y j) x))
             (!= (nth y i) (nth y j)))))



)



(defn validate-predicate
  "Calls defined mzn predicate in a minimalistic CSP with the given fixed arguments (Clojure values) to demonstrate whether the arguments comply with the predicate (returns `true`) or not (returns error message as string)."    
  [predicate & args]
  (try (do
         (minizinc
          (clj2mnz
           (constraint (apply predicate (map literal args)))
           (solve :satisfy)
           (output-map {:solved true})))
         true)
       (catch clojure.lang.ExceptionInfo e
         (let [e-type (core/-> e ex-data :type)]
           ;; only catch minizinc errors or warnings, but no other exceptions
           (if (or (= e-type :minizinc-warning)
                   (= e-type :minizinc-error))                   
             (.getMessage e)
             (throw e))))))


(comment

  (predicate alldifferent_except_x
    "Constrains the elements of array `y` to be all different except those
  elements that are assigned to `x`.
  source: modification of MiniZinc's `alldifferent_except_0.mzn`"
    [x [:var :int]
     y [:array :int [:var :int]]]    
    (forall [i (index_set y)
             j (index_set y)
             :where (!= i j)]
      (-> (and (!= (nth y i) x)
               (!= (nth y j) x))
          (!= (nth y i) (nth y j)))))

  (validate-predicate alldifferent_except_x 0 [5 4 3 2 1 6 9 0 0 0]) ;; OK
  (validate-predicate alldifferent_except_x 0 [5 4 3 2 1 6 1 0 0 0]) ;; error, because 1 occurs twice


  

  )
  

;; ;; !! Unfinished (and with namespace qualifier m/...) 
;; (defmacro def-predicate-test
;;   ;; TODO: `local` or `let` syntax?
;;   "Vector bindings sets arguments for bindings like `local` or `let`
;; Provides init value for all args of predicate and only args of predicate, in the right order"
;;   [predicate bindings]
;;   (let [args  ;; TODO: extract from bindings
;;         fn-name (symbol (str test "-" predicate))]
;;   `(defn ~fn-name
;;      ~(str "Tests predicate " predicate)
;;      [my-x my-y]
;;      (m/minizinc
;;       (m/clj2mnz
;;        ;; TODO: decide: `local` or `let`
;;        (let ~bindings
;;          (m/constraint ~(apply predicate args))
;;          (m/solve :satisfy)
;;          ;; TODO: generate out map
;;          (m/output-map {:my-array my-array})))))))
;;
;; (predicate-test alldifferent_except_x
;;   [x (m/int 'x 9)
;;    my-array (m/array (m/-- 1 10) [:var (m/-- 0 10)])])

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local Variables
;;;

;; Tutorial p. 49
;;
;; var s..e: x;
;; let {int: l = s div 2, int: u = e div 2, var l .. u: y} in x = 2*y


;; Must be public, because it is called by macro local
(defn ^:no-doc mk-decl-str
  "[Aux fn] def for macro `local`."
  [binding]
  (let [;; destructure binding#
        [name type-inst init-value] binding
        ;; declaration without initialisation 
        var-decl (str (mk-type-inst-string type-inst) ": " name)
        ;; optional initialisation
        init-str (if init-value (str " = " init-value) "")]
    (str var-decl init-str)))

(comment
  (mk-decl-str ['x [:var (-- -1 1)]])
  (mk-decl-str ['z [:int] 1])
  )


;; macro name chosen because `let` is a special form
(defmacro local
  "Defines local MiniZinc variables. Similar to `let` in Clojure plus required
  type-ints for variables.

  `bindings*` is a vector of variable declarations; each is a list consisting 
  of the variable name, a type-inst, and optionally an initialisation value.

  Example:

  (local [(x [:var (-- 0 10)])
          (y :int 1)]
    (constraint (> x y)))

  See [[predicate]] for type-inst examples."
  {:forms '[(local [bindings*] exprs*)]
   :style/indent [1 [[:defn]] :form]}
  [bindings & body]
  (let [;; vector of arguments without type-insts or initialisation
        args (apply vector (map first bindings))
        ;; bindings where each binding is wrapped in vector, not list
        bindings-vec (apply vector (map (fn [binding]
                                          ;; turn var decl into vector and "stringify"
                                          ;; var name to avoid evaluation
                                          (assoc (apply vector binding)
                                                 0 ;; var name at 1st pos.
                                                 (str (first binding))))
                                        bindings))]
    `(let [;; plain aVar for each variable of local, without any mzn string
           aVars# (map make-aVar '~args)]
       ;; let header
       (tell-store! :other
                    (str "let {"
                         (str/join ", " (map mk-decl-str ~bindings-vec))
                         "} in"))
       ;; let body -- store told internally (e.g., in `constraint` calls)
       (apply (fn ~args ~@body) (map name-or-val aVars#))
       )))


;; !? TODO: add type-inst spec, once that is working
(spec/def ::local (spec/cat :bindings (spec/coll-of (spec/cat :var symbol? :type-inst any? :initialsation (spec/? any?))
                                                   :kind vector?)
                           :body (spec/+ any?)))
(spec/fdef local :args ::local)
(spectest/instrument `local)
;; (spectest/unstrument `local)



(comment

  ;; (local [(x [:var (-- 0 10)])
  ;;         (y :int 1)]
  ;;   (constraint (> x y)))

  ;; (local [(x [:var (-- -1 1)])
  ;;         (y [:var #{1 3 5}])
  ;;         (z [:int] 1)]
  ;;   (constraint (+ x y z)))

  ;; multiple constraints
  (clj2mnz
   (local [(x [:var (-- -1 1)])
           (y [:var #{1 3 5}])
           (z [:int] 1)]
     (constraint (+ x y z))
     (constraint (< x y))))

  ;; with custom predicate
  (predicate test-pred [x [:var :int]]
             (local [(y [:var :int])]
               (+ x y)))
  (print
   (clj2mnz
    (constraint (test-pred 1))
    (constraint (test-pred 2))
    ))
  

  ;; ;; Spec error without surrounding () of each binding
  ;; (local [y :int 1]
  ;;   y)
  ;; (local ([y :int 1])
  ;;   y)

  (spec/conform ::local
                `[[(x [:var (-- -1 1)])
                   (y [:var #{1 3 5}])
                   (z [:int] 1)]
                  (constraint (+ x y z))
                  (constraint (< x y))])
      
  )

(comment

  ;; Alternative `local` syntax
  
  ;; constraint let { var int: s = x1 + x2 + x3 + x4 } in l <= s /\ s <= u;
  ;; 
  ;; BUG: variable def changed: skipped var name and added init value
  (local [s (variable :int (+ x1x2 x3 x4))]
         (<= l s u))  ; BUG: <= only binary 

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditional Expressions
;;;

;; Tutorial p. 26


;; fn name chosen because `if` is a special form
(defn ifte
  "MiniZinc if-then-else expression.  

  Example: 

  (ifte (< x y) x y)

  Instead of elseif simply nest ifte-expressions.

  (ifte (< x 0) -1 (ifte (> x 0) 1 0))

  The type-inst of the `test` expression must be par bool or var bool. 
  The `then` and `else` expressions must have the same type-inst, or 
  be coercible to the same type-inst, which is also the type-inst of 
  the whole expression.

  If the `test` expression is var bool then the type-inst of the `then` 
  and `else` expressions must be varifiable.

  If the `test` expression is par boool then evaluation of if-then-else
  expressions is lazy—the condition is evaluated, and then only one of 
  the `then` and `else` branches are evaluated, depending on whether the
  condition succeeded or failed. This is not the case if it is var bool.
  "
  {:style/indent [1 [[:defn]] :form]}
  [test then else]
  (str "if " (name-or-val test)
       " then " (name-or-val then)
       " else " (name-or-val else)
       " endif"))


(comment

  (print 
   (clj2mnz 
    (let [x (variable (-- -1 1)) 
          y (variable (-- -1 1))]
      (constraint (= 0
                     (ifte (< x y) x y))))))

  ;; variant quasi with elseif: nested ifs 
  (ifte (< x 0)
    -1
    (ifte (> x 0)
      1
      0))
  
  )


