;; TODO: break into multiple namespaces for clarification, e.g., for integer, set and float domains and core defs.

;; TODO: ? Add support for arrays

;; TODO: Add some examples (in extra files) -- just start with examples from MiniZinc tutorial 

;; TODO: revise with FlatZinc spec at http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf
;; - Document all basic constraints with that spec
;; - ! Add support for search annotations as documented in the spec (but allow for more than given there)
;; - DOuble-check all param and var declarations are there etc.


(ns clojure2minizinc.core
  ;; make explicit shadowing a range of core clojure functions etc
  (:refer-clojure :exclude [> >= <= < = == != -> + - * / mod assert concat min max 
                            int float set and or nth]) ; not
  (:require [clojure.core :as core]
            [clojure.java.shell :as shell]
            ;; http://clojuredocs.org/clojure_core/1.3.0/clojure.pprint
            [clojure.pprint :as pprint]
            ;; http://raynes.github.io/fs/  https://github.com/Raynes/fs
            [me.raynes.fs :as fs]
            ;; [clojure.walk :as walk] ;; not used yet
            [clojure.java.io :as io] ;; only required for testing here
            ;; https://github.com/clojure/math.combinatorics/
            ;; https://clojure.github.io/math.combinatorics/
            [clojure.math.combinatorics :as combi]
            ))

;; (require '[clojure2minizinc.core :as mzn])

;;;
;;; Customization
;;;

;; Path to shell apps
;; (def *mzn2fzn* "Path to the mzn2fzn executable" 
;;   "/Applications/minizinc-1.6/bin/mzn2fzn")
;; TODO: Find out -- use def or defvar? And does def support a doc string?
(def ^:dynamic *fd-solver* "Path to constraint solver for finite domain (integers)" 
  "mzn-g12fd")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defining the store for storing all information about a CSP
;;;

;; Sending all constraints to a single store instead of returning them from functions like constraint allows, e.g., to store minizinc vars in arbitrary clojure data structures and freely traversing such data structure for applying constraints to these, without worrying how to collect the constraint information.
;; Note: Must be public so that other packages can indirectly write to it (why? tell-store does not need to be public either.)
(def ^{:dynamic true} *mzn-store*  ; :private true
  "A thread-local store for collecting all information about a CSP."
  false)

(defn- tell-store 
  "Extends *mzn-store* by given constraint and returns constraint (only extends *mzn-store* at thread-local level, otherwise does nothing)."
  [constraint]
  (if *mzn-store*
    (do (set! *mzn-store* (conj *mzn-store* constraint))
        constraint)
    constraint))


(comment
  (binding [*mzn-store* ()]
    (tell-store 1)
    (tell-store 2)
    (tell-store 3)
    ;; (println *mzn-store*)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data structure definitions 
;;; (I could perhaps only use strings, but additional explicit information important for certain types)
;;;


(defrecord aVar [name mzn-string])
;; NOTE: I would prefer making this a private function (and also aVar? make-anArray etc.), but it is required to be public (because used in macros?) 
(defn make-aVar 
  "[Aux function] Returns an aVar record."
  [name mzn-string]
  (aVar. name mzn-string))
(defn aVar? 
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


(defrecord anArray [name mzn-string boundaries])
(defn ^:private index-set->boundaries 
  "Retrieves the max and min of index-set. Example:
(index-set->boundaries (-- 0 10)) ; > {:min 0 :max 10}"
  [index-set]
  (let [coll (map read-string (clojure.string/split index-set #"\.\."))]
    {:min (core/nth coll 0) :max (core/nth coll 1)}))
(defn make-anArray 
  "[Aux function] Returns an anArray record."
  [name mzn-string index-set]
  ;; NOTE: no type-checking of index-set
  (anArray. name mzn-string
            (cond 
             ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
             (aVar? index-set) (index-set->boundaries (:name index-set))
             (string? index-set) (index-set->boundaries index-set) 
             (list? index-set) (map #(index-set->boundaries
                                      (cond 
                                       ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
                                       (aVar? %) (:name %)
                                       (string? %) %)) 
                                    index-set))))
(defn anArray? 
  "Returns true if x is anArray."
  [x]
  (core/= (type x) clojure2minizinc.core.anArray))

(comment
  ;; access boundaries of given index-set
  (index-set->boundaries (-- 0 10))
  (make-anArray 'test "this is a test" (list (-- 0 10) (-- 0 10))) 
  )



(defn- expr
  "Returns an expression (e.g., a string with a MiniZinc expression). If x is aVar or similar record, it returns its name. Otherwise it returns the value that corresponds to x (e.g., a string remains that string etc.)."
  [x]
  ;; (pprint/pprint (list literal-tests (some #(% x) literal-tests))) 
  (cond (core/or (aVar? x) (anArray? x)) (:name x)
        (core/or (string? x)
                 (number? x)) x
                 (core/or (keyword? x)
                          (symbol? x)) (name x)
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
  (expr :test)
  
  ;; errors 
  (expr ['myVar])
  (expr 'x)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MiniZinc parameters (quasi constants)
;;;

;; BUG: I may need to declare a parameter with an explicitly given name, but not initialisation -- initialisation happens in init file. This case is more important than being able to initialise a variable without giving it a name - giving it a name unnecessarily does not hurt too much (but is inconvenient). By contrast, initialising it unnecessarily makes initialisation in datafile impossible. 
;; More flexible, but more verbose: using keyword args. Even better: making key-args also an option.
(defn- par   
  "Declares a parameter (quasi a constant) with the given type (a string, symbol or keyword; can be int, float, bool and 'set of int'), an optional init-value (default nil, meaning no initialisation), and optional var name (a string, symbol or keyword, default is a gensym-ed name)."
  ([param-type] (par param-type (gensym (name param-type))))
  ([param-type var-name] (par param-type var-name nil))
  ([param-type var-name init-value]
     {:pre [(#{"int" "float" "bool" "set of int"} (name param-type))]}
     ;; (println (pprint/cl-format nil "param-type: ~S, init-value: ~S, var-name ~S" param-type init-value var-name))
     (tell-store
      (make-aVar (name var-name) 
                 (if init-value
                   (format "%s: %s = %s;" (name param-type) (name var-name) init-value)
                   (format "%s: %s;" (name param-type) (name var-name)))))))

(comment
  (:mzn-string (par :int 'x 1))
  (par :int 'test 1)
  (par :int)

  (par :float 'x 1.0)
  (par :float 'x)
  (par :float)

  (par :bool 'x 'true)
  (par :bool 'x)
  (par :bool)

  (par "set of int" 'MySet (-- 1 'max))
  )

(defn int 
  "Declares an initeger parameter (quasi a constant) with an optional init-value (default nil, meaning no initialisation), and optional name (a string, symbol or keyword, default is a gensym-ed name)."
  ([] (par :int)) 
  ([var-name] (par :int var-name))
  ([var-name init-value] (par :int var-name init-value)))

(defn float 
  "Declares a float parameter (quasi a constant) with an optional init-value (default nil, meaning no initialisation), and optional name (a string, symbol or keyword, default is a gensym-ed name)."
  ([] (par :float)) 
  ([var-name] (par :float var-name))
  ([var-name init-value] (par :float var-name init-value)))

(defn bool 
  "Declares a bool parameter (quasi a constant) with an optional init-value (default nil, meaning no initialisation), and optional name (a string, symbol or keyword, default is a gensym-ed name)."
  ([] (par :bool)) 
  ([var-name] (par :bool var-name))
  ([var-name init-value] (par :bool var-name init-value)))



;; see http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf p. 4
;; TODO: add distinction between parameter and variable declaration
;; etc -- in short, cover all type-inst variants in the flatzinc-spec, see link above
(defn set
  "Declares a set of integers parameter (quasi a constant) with an optional init-value and optional name (a string, symbol or keyword, default is a gensym-ed name). The init value is a range, e.g., `(-- 1 10)` meaning the set contains all integers in the range. The default is nil, meaning no initialisation."
  ([] (set (gensym "Set"))) 
  ([var-name] (set var-name nil))
  ([var-name init-value] (par "set of int" var-name init-value)))

(comment
  (int)
  (int 'test)
  (int 'test 3)
  (bool)

  (set-of-int)
  (set-of-int "MySet")
  (set-of-int "MySet" (-- 1 10))
  (set-of-int "MySet" (literal-set-of-int 1 3 5))
  )

(defn literal-set
  "Specifies a set of explicitly given integers (can be MiniZinc expressions) as elements."
  [& exprs]
  (format "{%s}" (apply str (interpose ", " (map expr exprs)))))

(comment
  (literal-set-of-int 1 2 3)
  )



;; Musing: revision of array
;; 
;; Arguments in MiniZinc directly
;; - index-set
;; - type-inst: which can be a wide range of forms
;;   Required information
;;   - Whether or not variable
;;   - type-inst of contained vars
;;   TODO: need to revise type-inst defs that all necessary information is available
;; - optional name
;; - optional init value 
;;   TODO: need to add that arg (using literal-array)
;; 
;; "BUG:" Unfinished: ideas how to declare type-inst
;; which one to use?
(comment
  ;; Cannot use current fn variable to mark variables (fn variable implicitly tells store new variable)
  (array (-- 0 10) (variable (-- 1 3)))
  ;; Actually, cannot use`--` to mark variables either (MiniZinc/FlatZinc allows to specify int in range that way)
  (array (-- 0 10) (-- 1 3))  
  ;; set vars still missing altogether -- I need some approach that would support them as well
  )

(defn array   
  "Declares a one- or multi-dimensional array.

Arguments

index-set: The explicitly declared indices of the array. Either an integer range (declared with function --), a set variable initialised to an integer range, or for multi-dimensional arrays a list of integer ranges and set variables.
type-inst: Specifies the parameter type or variable domain 
The type of the variables contained in the array (a string, symbol or keyword; can be int, float, bool, string and \"set of int\").
var-name: an optional name for the array (a string, symbol or keyword) Default is a gensym-ed name.
"
  ([index-set type-inst] (array index-set type-inst (gensym (str (name type-inst) "_array"))))
  ([index-set type-inst var-name]
     ;; {:pre [(#{"int" "float" "bool" "string" "set of int"} (name type-inst))]}
     ;; (println (pprint/cl-format nil "type-inst: ~S, init-value: ~S, var-name ~S" type-inst init-value var-name))
     (tell-store
      (make-anArray 
       (name var-name) 
       (format "array[%s] of var %s: %s;" 
               ;; index-set
               (cond 
                ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
                (aVar? index-set) (:name index-set)
                (string? index-set) index-set
                (list? index-set) (apply str
                                         (interpose ", "
                                                    (map #(cond 
                                                           ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
                                                           (aVar? %) (:name %)
                                                           (string? %) %
                                                           :else (throw 
                                                                  (Exception. 
                                                                   (pprint/cl-format nil "Not allowed as array index-set: ~S of type ~S" 
                                                                                     % (type %)))))
                                                         index-set)))
                :else (throw (Exception. 
                              (pprint/cl-format nil "Not allowed as array index-set: ~S of type ~S" 
                                                index-set (type index-set)))))
               ;; type-inst etc
               ;; If variable (declared by domain) use keyword "var", otherwise (declared by domain) omit "var"
               ;; TODO: how is an array of set vars declared  
               ;; see http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf p. 4
               (if 
                   ;; TODO: test for domain spec... 
                   ;; ? Test string for pattern <num>..<num> ?
                   (throw (Exception. "unifinished definition: array"))
                   (format "var %s" type-inst)
                 ;; parameter domain
                 (name type-inst))
               (name var-name))
       index-set))))


(comment
  (array (-- 0 10) :bool)
  (array (-- 0 10) :int 'test) ;"array[0..10] of var int: test;"

  ;; which one to use?
  (array (-- 0 10) (variable (-- 1 3)))
  (array (-- 0 10) (-- 1 3))

  ;; Semi BUG: somewhat questionable: the [dimension] of the set (e.g., "1..10") is temporarily stored as aVar name to make it easily accessible for the array construction. Later the set-of-int is not used at all. Possibly better to completely avoid this potential cause of confusion, i.e., not to use a set for the array construction (or to clean up the internal use of sets here). 
  ;; Fixing in cond of functions array and make-anArray
  (array (set-of-int (-- 1 10)) :int)
  (array (list (-- 0 10) (-- 0 10) (set-of-int (-- 1 10))) :int)
  )


;; No explicit support for mapping needed, as I already have the mappable clojure data structure as input to literal-array
(defn literal-array 
  "Specifies a one- or two-dimensional array that contains the given MiniZinc expressions as elements. Two-dimensional arrays are defined by a list of expressions."
  [& exprs]
  (if (every? list? exprs)
    (str "[|" (apply str (flatten (interpose " | " (map (fn [sub-exprs]
                                                          (interpose ", " (map expr sub-exprs)))
                                                        exprs)))) "|]")    
    (format "[%s]" (apply str (interpose ", " (map expr exprs))))))

(comment
  (literal-array (int) (int) (int))
  (print (literal-array (list (int) (int)) (list (int) (int))))
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
  "Transforms a one or more dimensional MiniZinc array into a Clojure list (of MiniZinc code strings representing the array elements), so that MiniZinc functions can  be applied to individual MiniZinc elements (e.g., by mapping).

BUG: multi-dimensional array should return nested sequence to clearly highlight the dimensions. Currently, simply  flat sequence with all elements (the cartesian product is returned)."
  [my-array]
  (let [bounds (:boundaries my-array)]
    (cond 
     ;; multi-dimensional array
     (seq? bounds) (map (fn [args] (apply nth my-array args))
                        (apply combi/cartesian-product
                               (map (fn [bnds] (range (:min bnds) (core/+ 1 (:max bnds))))
                                    bounds)))
     ;; one-dimensional array
     (map? bounds) (map (fn [i] (nth my-array i))
                        (range (:min bounds) (core/+ 1 (:max bounds)))))))

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
;;
;; Aggregation functions for arithmetic arrays are
;;

;; TODO: later turn this into a local function with letfn, but that makes testing more difficult
;; Aux functions for macros need to be public, hm...
(defn- forall-format 
  "[Aux for forall]"
  [vars & body]
  (format "forall(%s)(%s)" 
          (apply str (interpose ", " (map :mzn-string vars))) 
          (apply str body)))

(comment
  (let [a (array (-- 0 10) :int)
        i (make-aVar (name 'i) (format "%s in %s" (name 'i) (-- 1 10)))
        j (make-aVar (name 'j) (format "%s in %s" (name 'j) (-- 1 10)))]
    (print (forall-format 
            (list i j)
            ;; (= (nth a i) 0)
            (= (nth a j) 0))))
  )


;; !! TODO: This definition is far away still from the flexibility of the true MiniZinc forall, which allows to declare multiple vars together, have "list generators" etc. 
;; One example: 
;; forall(i,j in index_set(x) where i < j) ( x[i] != x[j] );
;; 
;; TODO: allow for only a single expression, and not a body of multiple expressions? If multiple, they would need to be separated, e.g., by a semicolon?
(defmacro forall
  "MiniZinc looping. decls are pairs of range declarations <name> <domain>.

Example:
`(let [a (array (-- 0 10) :int)]
  (forall [i (-- 0 10)]
          (= (nth a i) 0)))
`

BUG: Only subset of MiniZinc's `forall` syntax supported yet. In particular, no list and set comprehensions supported yet."
  {:forms '[(forall [range-decls*] exprs*)]}
  [range-decls & body]
  (let [var-val-pairs (partition 2 range-decls)]
    `(let ~(vec (mapcat (fn [[var-name range]]
                          (list var-name `(make-aVar ~(name var-name) (format "%s in %s" ~(name var-name) ~range))))
                        var-val-pairs))
       (forall-format 
        ~(cons 'list (map first var-val-pairs))
        ~@body))))


(comment
  (def a (array (-- 1 10) :int 'a))
  (print (forall [i (-- 1 10)]
                 (= (nth a i) 0)))
  ;; => "forall(i in 1..10)((a[i] = 0))"    

  ;; a only 1 dimensional, but for test this is sufficient :)
  (print (forall [i (-- 0 10)
                  j (-- 0 10)]
                 (= (nth a i j) 0)))

  (print 
   (let [x 1
         a (array (-- 0 10) :int)]
     (forall [i (-- 0 10)
              j (-- -1 x)]
             (= (nth a i) 0)
             (= (nth a j) 0))))
  )


(comment
  
  ;; TODO: define missing aggregation functions for arithmetic arrays with their full syntax
  ;; See MiniZinc tutorial p. 23 and surrounding pages
  ;; arg vectors below are only sketch, and may not be what is needed in the end for full syntax
  ;; All these macros and forall are very similar -- likely I need to define general case only once and then customise...
  (defmacro exists
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  (defmacro xorall
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  (defmacro iffall
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  (defmacro sum
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  (defmacro product
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  (defmacro min
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  (defmacro max
    ""
    {:forms '[(forall [range-decls*] exprs*)]}
    [range-decls & body]
    ...)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditional Expressions
;;;

(comment
  ;; TODO: define `if`, but under different name
  ;; (if is a special form)

;; CompilerException java.lang.RuntimeException: No such var: core/if, 
;; compiling:(/private/var/folders/c_/14td248n5xd5wjbmrhldmwl00000gq/T/form-init6276176329836054569.clj:1:1) 
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local Variables
;;;

(comment
  ;; TODO: define `let`, but under different name
  ;; (let is a special form)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MiniZinc vars
;;;

;; TODO: find out whether there is a way in MiniZinc to restrict the domain of an integer to only a given list of integers (i.e., "cut holes" into the domain)
(defn --
  "Expects a minimum an a maximum value (ints or floats) and returns a domain specification for a decision variable (ints or floats)."
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
(letfn [(mk-dom-string [type-inst]
          {:pre [(if (vector? type-inst) 
                   ((first type-inst) #{:set :int :float})
                   true)]}
          (cond (seq? type-inst) (str "{" (apply str (interpose ", " type-inst)) "}")
                (vector? type-inst) (let [[dom-name dom-spec] type-inst]
                                      (case dom-name
                                        :set (str "set of " (mk-dom-string dom-spec))
                                        :int (mk-dom-string dom-spec)
                                        :float (mk-dom-string dom-spec)))
                :else (name type-inst)))]
  (defn variable
    "Declares a decision variable (bool, int, float, or set of int) with the given domain, and an optional variable name (string, symbol or keyword). 

Examples:
`(variable :bool)             ; a Boolean variable (no further domain specification supported)
(variable :int)              ; an integer variable with maximum supported domain size 
(variable (-- -1 10))        ; an integer variable with domain [-1, 10]
(variable '(1 3 6 8))        ; an integer variable with the domain {1, 3, 6, 8}
(variable (-- 1.0 10.0))     ; a float variable with domain [1.0, 10.0]
(variable '(1 3 6 8))        ; an integer variable with the domain {1, 3, 6, 8}
(variable [:set (-- 1 3)])   ; a set of integers with the given domain
(variable [:set '(1 3 6 8)]) ; a set of integers with the given domain
(variable [:int (-- 1 3)])   ; same as (variable (-- -1 10))
(variable (-- 1 10) 'x)      ; an integer variable named x (instead of an automatically assigned name)
`"
    ([type-inst] (variable type-inst (gensym "var")))
    ([type-inst var-name]
       (let [dom-string (mk-dom-string type-inst)
             name-string (name var-name)]
         (tell-store (make-aVar name-string (format "var %s: %s;" dom-string name-string)))))))

    

(comment
  ;; (mk-dom-string :bool)
  ;; (mk-dom-string (-- 1 3))
  ;; (mk-dom-string [:set '(1 3 6 8)])
  ;; (mk-dom-string [:int '(1 3 6 8)])
  
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
  (variable [:set '(1 3 6 8)])
  ;; for completeness/consistency, but contains superflous information
  (variable [:int '(1 3 6 8)])
  (variable [:float (-- 1.0 3.0)])
  ;; (variable [:bool]) ;; not possible!
  ;; alternative notation -- more clean solution -- can be defined when notation above is supported
  (set-variable (-- 1 3))
  (set-variable (-- 1 3) 'mySet)
  (set-variable '(1 3 6 8))
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



(defn include 
  "Include the given file."
  [file]
  (format "include \"%s\";" file))

(comment
  (include "test.mzn")
  (include (io/as-file "test2.mzn"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constraints -- aux defs
;;;

(defn constraint 
  "Expects a constraint expression (a string) and turns it into a constraint statement."
  [constraint-expr]
  (tell-store (format "constraint %s;" (expr constraint-expr))))

(defmacro ^:private def-unary-operator
  "Defines a function that outputs the code for a MiniZinc unary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     [arg#]
     (format ~(str operation " %s")  (expr arg#))))

(defmacro ^:private def-binary-operator
  "Defines a function that outputs the code for a MiniZinc binary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     [lh# rh#]
     (format ~(str "(%s " operation " %s)") (expr lh#) (expr rh#))))

(defmacro ^:private def-binary-and-n-ary-operator
  "Defines a function that outputs the code for a MiniZinc binary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     ([lh# rh#]
        (format ~(str "(%s " operation " %s)") (expr lh#) (expr rh#)))
     ([arg1# arg2# & args#]
        (reduce ~op-name arg1# (cons arg2# args#)))))

(defmacro ^:private def-unary-and-n-ary-operator
  "Defines a function that outputs the code for a MiniZinc unary and binary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     ([arg#]
        (format ~(str operation " %s") (expr arg#)))
     ([lh# rh#]
        (format ~(str "(%s " operation " %s)") (expr lh#) (expr rh#)))
     ([arg1# arg2# & args#]
        (reduce ~op-name arg1# (cons arg2# args#)))))

(defmacro ^:private def-unary-function
  "Defines a function that outputs the code for a MiniZinc function."
  [fn-name fn doc-string]
  `(defn ~fn-name
     ~doc-string
     [arg#]
     (format ~(str fn "(%s)")  (expr arg#))))

(defmacro ^:private def-binary-function
  "Defines a function that outputs the code for a MiniZinc function."
  [fn-name fn doc-string]
  `(defn ~fn-name
     ~doc-string
     [arg1# arg2#]
     (format ~(str fn "(%s, %s)")  (expr arg1#) (expr arg2#))))

(defmacro ^:private def-n-ary-function
  "Defines a function that outputs the code for a MiniZinc function."
  [fn-name fn doc-string]
  `(defn ~fn-name
     ~doc-string
     ([arg1# arg2#]
        (format ~(str fn "(%s, %s)")  (expr arg1#) (expr arg2#)))
     ([arg1# arg2# & args#]
        (reduce ~fn-name arg1# (cons arg2# args#)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constraint defs
;;;

(def-unary-and-n-ary-operator + + 
  "+ constraint")
(def-unary-and-n-ary-operator - -
  "- constraint")

(def-binary-operator <-> <->
  "Logical equivalence constraint")
(def-binary-operator -> ->
  "Logical implication constraint")
(def-binary-operator <- <-
  "")
;; core/not needed for Emacs interaction, and seemingly shadowing it breaks Emacs functionality (of AC?). Therefore alternative function name
(def-unary-operator nega not 
  "Logical negation constraint (not)")
(def-binary-and-n-ary-operator or "\\/"
  "Logical or constraint")
(def-binary-operator xor xor
  "Logical and constraint")
(def-binary-and-n-ary-operator and "/\\"
  "Logical xor constraint")
;; TODO: document briefly all below constraints
(def-binary-operator < <
  " constraint")
(def-binary-operator > >
  " constraint")
(def-binary-operator <= <=
  " constraint")
(def-binary-operator >= >=
  " constraint")
(def-binary-operator = =
  " constraint")
(def-binary-operator == ==
  " constraint")
(def-binary-operator != !=
  "Not equal constraint")
(def-binary-operator in in
  " constraint")
(def-binary-operator subset subset
  " constraint")
(def-binary-operator superset superset
  " constraint")
(def-binary-operator union union
  " constraint")
(def-binary-operator diff diff
  " constraint")
(def-binary-operator symdiff symdiff
  " constraint")
(def-binary-operator intersect intersect
  " constraint")
(def-binary-and-n-ary-operator ++ ++
  "Concatenates strings and arrays.")
(def-binary-and-n-ary-operator * *
  " constraint")
(def-binary-and-n-ary-operator / /
  " constraint")
(def-binary-and-n-ary-operator div div
  " constraint")
(def-binary-operator mod mod
  " constraint")

;; TODO: doc strings
(def-unary-function abort abort
  " function constraint")
(def-unary-function abs abs
  "absolute value constraint")
(def-unary-function acos acos
  "arccosine constraint")
(def-unary-function acosh acosh
  "hyperbolic arccosine constraint")
(def-unary-function array_intersect array_intersect
  " function constraint")
(def-unary-function array_union array_union
  " function constraint")
(def-unary-function array1d array1d
  " function constraint")
(def-unary-function array2d array2d
  " function constraint")
(def-unary-function array3d array3d
  " function constraint")
(def-unary-function array4d array4d
  " function constraint")
(def-unary-function array5d array5d
  " function constraint")
(def-unary-function array6d array6d
  " function constraint")
(def-unary-function asin asin
  "arcsine constraint")
(def-unary-function asinh asinh
  "hyperbolic arcsine constraint")
(def-unary-function atan atan
  "arctangent constraint")
(def-unary-function atanh atanh
  "hyperbolic arctangent constraint")
(def-unary-function bool2int bool2int
  " function constraint")
(def-unary-function card card
  " function constraint")
(def-unary-function ceil ceil
  " function constraint")
(def-unary-function concat concat
  " function constraint")
(def-unary-function cos cos
  "cosine constraint")
(def-unary-function cosh cosh
  "hyperbolic cosine constraint")
(def-unary-function dom dom
  " function constraint")
(def-unary-function dom_array dom_array
  " function constraint")
(def-unary-function dom_size dom_size
  " function constraint")
(def-unary-function fix fix
  " function constraint")
(def-unary-function exp exp
  "exponentiation of e constraint")
(def-unary-function floor floor
  " function constraint")
(def-unary-function index_set index_set
  " function constraint")
(def-unary-function index_set_1of2 index_set_1of2
  " function constraint")
(def-unary-function index_set_2of2 index_set_2of2
  " function constraint")
(def-unary-function index_set_1of3 index_set_1of3
  " function constraint")
(def-unary-function index_set_2of3 index_set_2of3
  " function constraint")
(def-unary-function index_set_3of3 index_set_3of3
  " function constraint")
(def-unary-function int2float int2float
  "Function to coerce integers to floating point numbers")
(def-unary-function is_fixed is_fixed
  " function constraint")
(def-unary-function join join
  " function constraint")
(def-unary-function lb lb
  " function constraint")
(def-unary-function lb_array lb_array
  " function constraint")
(def-unary-function length length
  "Returns the length of an array.")
(def-unary-function ln ln
  "natural logarithm constraint")
(def-unary-function log log
  " function constraint")
(def-unary-function log2 log2
  "logarithm base 2 constraint")
(def-unary-function log10 log10
  "logarithm base 10 constraint")
(def-n-ary-function min min
  " function constraint")
(def-n-ary-function max max
  " function constraint")
(def-unary-function product product
  " function constraint")
(def-unary-function round round
  " function constraint")
(def-unary-function set2array set2array
  " function constraint")
(def-unary-function show show
  " function constraint")
(def-unary-function show_int show_int
  " function constraint")
(def-unary-function show_float show_float
  " function constraint")
(def-unary-function sin sin
  "sine constraint")
(def-unary-function sinh sinh
  "hyperbolic sine constraint")
(def-unary-function sqrt sqrt
  "square root constraint")
(def-unary-function sum sum
  " function constraint")
(def-unary-function tan tan
  "tangent constraint")
(def-unary-function tanh tanh
  "hyperbolic tangent constraint")
(def-unary-function trace trace
  " function constraint")
(def-unary-function ub ub
  " function constraint")
(def-unary-function ub_array ub_array
  " function constraint")

(def-binary-function pow pow
  "power constraint")
(defn assert 
  "Constraint to guard against certain errors (e.g., to double-check input from data files).

mz-expr      (string) a MiniZinc expression returning a Boolean value
error-msg    (string) an error message printed in case mz-expr is false

BUG: mzn2fzn (version 1.6.0) detects inconsistency, but does not print the error message."
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






;; TMP: old defs
(comment

  (defn != 
    ""
    [lh rh]
    (format "%s != %s" (expr lh) (expr rh)))

  ;; (defn != 
  ;;   ""
  ;; [lh rh]
  ;; (pprint/cl-format nil "~S != ~S" (expr lh) (expr rh)))

  
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Solver
;;;

(defn solve 
  "Solve items specify what kind of solution is being looked for. Supported values for solver are satisfy, maximize, and minimize (a keyword)."
  ([solver]
     {:pre [(#{:satisfy} solver)]}
     (tell-store (format "solve %s;" (name solver))))
  ([solver expr]
     {:pre [(#{:maximize :minimize} solver)]}
     (tell-store (format "solve %s %s;" (name solver) expr))))


(comment
  (solve :satisfy)
  (solve :foo) ;; error -> assert failed
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
  "Expects a map containing MiniZinc variables and returns a string formatted for MiniZinc to output a Clojure map for Clojure to read."
  [my-map]
  (tell-store 
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
  "Expects a vector of MiniZinc variables and returns a string formatted for MiniZinc to output a Clojure vector for Clojure to read."
  [my-vec]
  (tell-store 
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
  "Outputs a single MiniZinc variable. For example, a one-dimensional MiniZinc array can be read into a Clojure vector directly."
  [my-var]
  (tell-store (format "output [ show(%s) ];" (expr my-var))))

;; TODO: output for multi-dimensional vector

;; TODO: make this somehow more smart by allowing for vars etc
;; e.g., at least allow for any number of args
(defn output 
  "Expects an output definition (a string) and turns it into an output statement (surround by brackets etc.)"
  [mzn-string]
  (tell-store (format "output [ %s ];" (expr mzn-string)))) ; \"\\n\"

(comment
  (print (output "x = show(a)"))
  )

(comment
  ;; TODO: finish definition
  ;; TODO: then revise definition such that it always results in a string expressing a clojure value such as a map.
  ;; Idea: input is also a map, where the values at keys are variables, or some other clojure data structure containing variables. This data structure is then expressed as a string.
  (defn output-clj
    "Output items are for nicely presenting the results of the model execution. Output expects any number of strings or variables."
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


  (walk/walk #(if (aVar? %) 
                (:name %)
                %)
             identity
             {:x x :y y})
  )


(defn map2minizinc 
  "Utility function for creating data files (*.dzn files) that map keys (MiniZinc variable names) to values."
  [mzn-map]
  (apply str (map (fn [[key val]] (str (format "%s = %s" (expr key) (expr val)) "; "))
                  mzn-map)))

(comment
  (map2minizinc {:x 1 :y 2 :z 3})
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Communicating with MiniZinc 
;;;

;; TODO: this will likely not work as a function, because given function calls are evaluated outside the necessary dynamic scope. Nevertheless, try before using a macro instead
;; TODO: Possibly I late embed this in minizinc below? Then it needs to become a macro itself.
;; TODO: should macros be defined elsewhere as caution?
(defmacro clj2mnz 
  "Translates a constraint problem defined in Clojure into the corresponding MiniZinc code. Expects any number of variable/parameter declarations, any number of constraints, one output, and one solver declaration, all in any order."
  [& constraints]
  `(binding [*mzn-store* []]
     ~@constraints
     ;; TODO: map is lazy -- make sure dynamic scope is not left
     (apply str (doall (map (fn [x#]  ; x# results in unique gensym
                              (str (cond (core/or (aVar? x#) (anArray? x#))
                                         (:mzn-string x#)
                                         (string? x#) x#
                                         :else (throw (Exception. (pprint/cl-format nil "~S not supported. MiniZinc statements must be strings or variables defined with function clojure2minizinc.core/variable." x#)))) 
                                   "\n"))
                            *mzn-store*)))))

(comment
  ;; minimum CSP
  ;; TODO: add output
  ;; TODO: try also macroexpand 
  (print
   (clj2mnz
    (let [a (variable (-- 1 3) 'a) ;; mzn var naming redundant, but ensures var name in *.mzn file
          b (variable (-- 1 3) 'b)]
      (constraint (!= a b))
      (solve :satisfy)
      (output-map {:a a :b b})
      (pprint/pprint *mzn-store*)
      )))
  )



;; ?? TODO: incorporate clj2mnz into minizinc (turning minizinc into a macro)? Perhaps having it separate is a good idea? Makes call more structured. 
;; TODO: Add solver arguments: parallel (unrecognized for mzn-g12fd), random-seed, solver-backend, flatzinc-flags (?), keep-files, ... 
(defn minizinc 
  "Calls a MiniZinc solver on a given MiniZinc program and returns a list of one or more solutions.

Options are

:mzn             (string) a MiniZinc program, which can be created with other functions of clojure2minizinc wrapped into clj2mnz
:print-mzn?      (boolean) whether or not to print resulting MiniZinc program (for debugging)
:print-solution? (boolean) whether or not to print the result output by MiniZinc directly instead of reading it into a Clojure value (e.g., for debugging). Prints the map resulting from clojure.java.shell/sh.
:solver          (string) solver to call
:mznfile         (string or file) MiniZinc file path to generate and use in the background
:data            (string) Content for a MiniZinc data file (*.dzn file). Can conveniently be created with map2minizinc 
:num-solutions   (int) An upper bound on the number of solutions to output
:all-solutions   (boolean) If true, return all solutions
"
  [mzn & {:keys [solver mznfile data
                 print-mzn?
                 print-solution? 
                 num-solutions all-solutions?] 
          :or {solver *fd-solver*
               mznfile (doto (java.io.File/createTempFile "clojure2minizinc" ".mzn") .deleteOnExit)
               data false
               print-mzn? false
               print-solution? false
               num-solutions 1
               all-solutions? false}}]
  ;; (println "mzn:" mzn "\nmznfile:" mznfile "\nsolver:" solver)
  (when print-mzn? (println mzn))
  ;; TMP: 
  ;; (when data (throw (Exception. (format "minizinc: arg data not yet supported. %s" data))))
  (spit mznfile mzn)
  ;; mznfile split into filename (base-name) and dirname (parent), so that shell/sh first moves into that dir, because otherwise I got errors from *fd-solver*
  (let [sh-args (core/concat [solver]
                             [(if all-solutions?
                                "--all-solutions"
                                ;; I could not get long parameter names working 
                                (format "-n%s" num-solutions))]
                             (if data
                               [(format "-D%s" data)]
                               [])
                             [(fs/base-name mznfile)]
                             [:dir (fs/parent mznfile)])
        ;; dummy (pprint/pprint sh-args)
        result (apply shell/sh sh-args)]
    (if print-solution?
      (pprint/pprint result)
      (if (core/= (:exit result) 0)
        (do 
          ;; TODO: this is not yet a clean solution
          ;; In case there is an error as part of a warning then show that. How can I show a warning and still return the final result?
          (if (core/not= (:err result) "")
            (throw (Exception. (format "MiniZinc: %s" (:err result)))))
          (map read-string
               (clojure.string/split (:out result) #"(\n----------\n|==========\n)")))
        (throw (Exception. (format "MiniZinc: %s" (:err result))))))))



(comment
  ;; !! NB: first mini version running :)
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

  

  ;; aust CSP in Clojure using a Clojure vector of variables
  ;; TODO: incomplete -- constraints and integer decl missing
  ;; TODO: not yet working?
  (minizinc 
   (clj2mnz
    ;; TODO: Create map instead of vector for named variables. Give vector of keys and by and by extend map {} by adding keys with vars (e.g., using fun reduce)
    (let [vars (map #(variable (domain 1 3) %) [:wa :nt :sa :q :nsw :v :t])]
      (solve :satisfy)
      ;; TODO: use map created above
      (output-map
       (apply hash-map 
              (flatten (map #(cons (keyword (:name %)) (list %))
                            vars))))
      ;; (pprint/pprint *mzn-store*)
      ))
   )  
  ;; (apply hash-map 
  ;;        (flatten (map #(cons (keyword (:name %)) (list %))
  ;;                      (map #(variable (domain 1 3) %) [:wa :nt :sa :q :nsw :v :t]))))


  ;; dummy example
  (minizinc 
   "var 0..2: x;
solve satisfy;"
   )

  (minizinc aust-csp)


  ;; NB: string literals in MiniZinc must not contain linebreaks -- so do not resolve \n 
  (def aust-csp
    "int: nc = 3;

var 1..nc: wa; var 1..nc: nt; var 1..nc: sa; var 1..nc: q;
var 1..nc: nsw; var 1..nc: v; var 1..nc: t;

constraint wa != nt; 
constraint wa != sa; 
constraint nt != sa; 
constraint nt != q; 
constraint sa != q; 
constraint sa != nsw; 
constraint sa != v; 
constraint q != nsw; 
constraint nsw != v; 
solve satisfy;


output [\"wa=\", show(wa), \"\t nt=\", show(nt),
\"\t sa=\", show(sa), \"\\n\", \"q=\", show(q),
\"\t nsw=\", show(nsw), \"\t v=\", show(v), \"\\n\",
\"t=\", show(t), \"\\n\"];"
    )
  
  ;; (minizinc candles-csp)

  ;; (def candles-csp "include \"globals.mzn\"; 
  ;; int: n = 7;

  ;; % decision variables
  ;; array[1..n] of var 1..20: vars;
  ;; % var 0..100: now          = sum(vars);  % number of candles this year
  ;; var 0..100: now;  % number of candles this year
  ;; var 0..100: twoyearsago  =  now - 2*n; % number of candles two years ago

  ;; solve satisfy;
  ;; % solve :: int_search(vars ++ [now, twoyearsago], first_fail, indomain_min, complete) satisfy;

  ;; constraint
  ;;    forall(i in 2..n) (
  ;;       vars[i-1] = vars[i]+1 
  ;;    )
  ;;    /\\
  ;;    twoyearsago*2 = now
  ;;    /\\ 
  ;;    now = sum(vars)
  ;; ;

  ;; output
  ;; [
  ;;   \"{\" ++ \":vars \" ++ show(vars) ++ \"\\n\" ++
  ;;   \":twoyearsago \" ++ show(twoyearsago) ++ \"\\n\" ++
  ;;   \":now \" ++ show(now) ++ \"}\"
  ;; ];")

  )
