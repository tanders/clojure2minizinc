;; TODO: revise with FlatZinc spec at http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf
;; - ! Add support for search annotations as documented in the spec (but allow for more than given there)
;; OK - Document all basic constraints with that spec
;; OK - Double-check all param and var declarations are there etc.


(ns clojure2minizinc.core
  "clojure2minizinc provides an interface between MiniZinc and Clojure. The clojure2minizinc user models in Clojure constraint satisfaction or optimisation problems over Boolean, integer, real number, and/or set variables. clojure2minizinc translates them into MiniZinc, they are solved in the background by a compatible solver, and the result is read back into Clojure. clojure2minizinc code can be very similar to the corresponding MiniZinc code, but in addition the full power of Clojure is at hand."
  ;; make explicit shadowing a range of core clojure functions etc
  (:refer-clojure :exclude [> >= <= < = == != -> + - * / mod assert concat min max 
                            int float set and or not nth
                            string?
                            count range sort]) 
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
(def ^:dynamic *fd-solver* "Path to default constraint solver for finite domain (integers)" 
  "minizinc")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defining the store for storing all information about a CSP
;;;

;; Sending all constraints to a single store instead of returning them from functions like constraint allows, e.g., to store minizinc vars in arbitrary clojure data structures and freely traversing such data structure for applying constraints to these, without worrying how to collect the constraint information.
;; Note: Must be public so that other packages can indirectly write to it (why? tell-store! does not need to be public either.)
(def ^{:dynamic true} ^:no-doc *mzn-store*  ; :private true
  "A thread-local store for collecting all information about a CSP."
  false)

(defn ^:no-doc tell-store! 
  "[Aux function] Extends *mzn-store* by given constraint and returns constraint (only extends *mzn-store* at thread-local level, otherwise does nothing)."
  [constraint]
  (if *mzn-store*
    (do (set! *mzn-store* (conj *mzn-store* constraint))
        constraint)
    constraint))

(comment
  (binding [*mzn-store* ()]
    (tell-store! 1)
    (tell-store! 2)
    (tell-store! 3)
    ;; (println *mzn-store*)
    )
  )

(def ^{:dynamic true} ^:no-doc *included-files*  ; :private true
  "A thread-local store for collecting which files have already been included. Used for automatic inclusion of global constraint defs."
  false)

;; (defn- add-included-file!
;;   "Extends *included-files* by given file and tells store to include that file, but only if that file was not included already. (Only extends *included-files* at thread-local level, otherwise does nothing)."
;;   [file]
;;   (if (core/and *included-files*
;;                 (core/not (contains? *included-files* file)))
;;     (do (println (format "add-included-file!: %s, %s, %s" file *included-files* *mzn-store*))
;;         (tell-store! (include file))
;;         (set! *included-files* (conj *included-files* file))
;;         file)
;;     file))

;; Extends *included-files* by given file and tells store to include that file, but only if that file was not included already. (Only extends *included-files* at thread-local level, otherwise does nothing).
(defn include 
  "Include the given file. Does automatic book keeping whether file was already included, and includes it only once."
  [file]
  (if (core/and *included-files*
                (core/not (contains? *included-files* file)))
    (do ; (println (format "add-included-file!: %s, %s, %s" file *included-files* *mzn-store*))
        (tell-store! (format "include \"%s\";" file))
        (set! *included-files* (conj *included-files* file))
        file)
    file))

(comment
  (binding [*included-files* #{}
            *mzn-store* ()]
    (include "test.mzn")
    (include (io/as-file "test2.mzn"))
    (include "test.mzn")
    (include (io/as-file "test2.mzn"))
    ;; (println *included-files*)
    (println *mzn-store*)
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data structure definitions 
;;; (I could perhaps only use strings, but additional explicit information important for certain types)
;;;

;; NOTE: ->aVar and map->aVar are created automatically, and it is included in the doc, because I cannot set ^:no-doc to it 
(defrecord ^:no-doc aVar [name mzn-string])
;; NOTE: I would prefer making this a private function (and also aVar? make-anArray etc.), but it is required to be public (because used in macros?) 
(defn ^:no-doc make-aVar 
  "[Aux function] Returns an aVar record."
  [name mzn-string]
  (aVar. name mzn-string))
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
  "Retrieves the max and min of index-set. Example:
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
  "Translates a Clojure value into a MiniZinc value (e.g., a string with a MiniZinc expression). If x is aVar or similar record, it returns its name. Otherwise it returns the value that corresponds to x (e.g., a string remains that string etc.).

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
  ([param-type par-name] (par param-type par-name nil))
  ([param-type par-name init-value]
     {:pre [(#{"int" "float" "bool" "set of int"} (name param-type))]}
     ;; (println (pprint/cl-format nil "param-type: ~S, init-value: ~S, par-name ~S" param-type init-value par-name))
     (tell-store!
      (make-aVar (name par-name) 
                 (if init-value
                   (format "%s: %s = %s;" (name param-type) (name par-name) init-value)
                   (format "%s: %s;" (name param-type) (name par-name)))))))

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
  ([par-name] (par :int par-name))
  ([par-name init-value] (par :int par-name init-value)))

(defn float 
  "Declares a float parameter (quasi a constant) with an optional init-value (default nil, meaning no initialisation), and optional name (a string, symbol or keyword, default is a gensym-ed name)."
  ([] (par :float)) 
  ([par-name] (par :float par-name))
  ([par-name init-value] (par :float par-name init-value)))

(defn bool 
  "Declares a bool parameter (quasi a constant) with an optional init-value (default nil, meaning no initialisation), and optional name (a string, symbol or keyword, default is a gensym-ed name)."
  ([] (par :bool)) 
  ([par-name] (par :bool par-name))
  ([par-name init-value] (par :bool par-name init-value)))



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
  (format "{%s}" (apply str (interpose ", " (map expr exprs)))))

(comment
  (literal-set 1 2 3)
  ;; Minor BUG: order of set elements scrambled. This is used in expr for Clojure sets -- can this be a problem? 
  (apply literal-set #{1 2 3})
  )


;; see http://www.minizinc.org/downloads/doc-1.6/flatzinc-spec.pdf p. 4
;; TODO: add distinction between parameter and variable declaration
;; etc -- in short, cover all type-inst variants in the flatzinc-spec, see link above
(defn set
  "Declares a set of integers parameter (quasi a constant) with an optional init-value and optional name (a string, symbol or keyword, default is a gensym-ed name). The init value is a range, e.g., `(-- 1 10)` meaning the set contains all integers in the range. The default is nil, meaning no initialisation."
  ([] (set (gensym "Set"))) 
  ([par-name] (set par-name nil))
  ([par-name init-value] (par "set of int" par-name init-value)))

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



;; No explicit support for mapping needed, as I already have the mappable clojure data structure as input to literal-array
(defn literal-array 
  "Specifies a one- or two-dimensional array that contains the given MiniZinc expressions as elements. Two-dimensional arrays are defined by a list of expressions."
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

(defn- mk-type-inst-string [type-inst]
  ;; {:pre [(if (vector? type-inst) 
  ;;          ((first type-inst) #{:var :set :int :float})
  ;;          true)]}
  (if (vector? type-inst)
    (apply str (interpose " " (map mk-type-inst-string type-inst)))
    (cond (core/= :var type-inst) "var"
          (core/= :set type-inst) "set of"        
          (set? type-inst) (str "{" (apply str (interpose ", " type-inst)) "}")
          :else (name type-inst))))

(comment
  (mk-type-inst-string :bool)
  (mk-type-inst-string (-- 1 3))
  (mk-type-inst-string #{1 3 5})
  (mk-type-inst-string [:set (-- 1 3)])
  (mk-type-inst-string [:var #{1 3 5}])
  (mk-type-inst-string [:var :set #{1 3 5}])

  (mk-type-inst-string [:int #{1 3 6 8}])
  )



;; Semi BUG: somewhat questionable: the [dimension] of the set (e.g., "1..10") is temporarily stored as aVar name to make it easily accessible for the array construction. Later the set-of-int is not used at all. Possibly better to completely avoid this potential cause of confusion, i.e., not to use a set for the array construction (or to clean up the internal use of sets here). 
(defn array   
  "Declares a one- or multi-dimensional array.

Arguments:

- index-set: The explicitly declared indices of the array. Either an integer range (declared with function --), a set variable initialised to an integer range, or for multi-dimensional arrays a list of integer ranges and/or MiniZinc sets.
- type-inst: Specifies the parameter type or variable domain 
The type of the variables contained in the array (a string, symbol or keyword; can be int, float, bool, string and \"set of int\").
- array-name (optional): a name for the array (a string, symbol or keyword). Default is a \"gensym-ed\" name.
- init-value (optional): a vector of MiniZinc-supported values. Defaults to nil.

Examples:

    (array (-- 1 10) :bool)               ; array of bools at indices 1-10 (not decision variables!)
    (array (-- 1 10) :int)                ; array of ints at indices 1-10
    (array (-- 1 10) [:set :int])         ; array of sets of integers
    (array (-- 1 10) (-- -1 2))           ; array of integers in given range  
    (array (-- 1 10) (-- 2.0 4.0))        ; array of floats in range
    (array (-- 1 10) [:set (-- -1 2)])    ; array of subsets of set range

    (array (-- 1 10) [:var :int])         ; array of int variables
    (array (-- 1 10) [:var (-- 1 3)])     ; array of int variables with given domain
    (array (-- 1 10) [:var #{1 3 5}])     ; array of int variables with given domain
    (array (-- 1 10) [:var (-- 1.0 3.0)]) ; array of float variables with given domain
    (array (-- 1 10) [:var :set (-- 1 3)]) ; array of set variables with domain
    (array (-- 1 10) [:var :set #{1 3 5}]) ; array of set variables with domain   

    (array [(-- 1 10) (-- 1 10)]  [:var :int (-- 1 3)]) ; two-dimensional array of int variables

    (array (-- 1 10) :int 'x)              ; array explicitly named x 

    (array (-- 1 3) :int 'x [5 6 7])       ; array of ins with init value

BUG: literal arrays not supported as init val.
"
  ([index-set type-inst] (array index-set type-inst (gensym "array")))
  ([index-set type-inst array-name] (array index-set type-inst array-name nil))
  ([index-set type-inst array-name init-value]
     ;; {:pre [(#{"int" "float" "bool" "string" "set of int"} (name type-inst))]}
     ;; (println (pprint/cl-format nil "type-inst: ~S, init-value: ~S, array-name ~S" type-inst init-value array-name))
     (tell-store!
      (make-anArray 
       (name array-name) 
       (format "array[%s] of %s: %s;" 
               ;; index-set
               (cond 
                ;; TODO: consider revising design -- currently a set wrapped in a var, and index-set stored in name
                (aVar? index-set) (:name index-set)
                (core/string? index-set) index-set
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
  "Transforms a one or more dimensional MiniZinc array into a Clojure list (of MiniZinc code strings representing the array elements), so that MiniZinc functions can  be applied to individual MiniZinc elements (e.g., by mapping).

BUG: multi-dimensional array should return nested sequence to clearly highlight the dimensions. Currently, simply a flat sequence with all elements (the cartesian product) is returned."
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
  "A MiniZinc operator call supporting arities of 2 or more. For higher arities multiple operators are used (e.g., a ternary plus is translated into x + y + z)"
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
  "[Aux for aggregation functions like forall] This function is only public, because it is needed in a public macro."
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


(defmacro aggregate
  "List (array) and set comprehension. Generates a MiniZinc array/set containing the possible combinations of locally declared MiniZinc parameters (generators) used in the expression (exp). 

Example:

    (aggregate [i (-- 1 3)] 
      (* i i))
    ; means [1*1, 2*2, 3*3]  (in MiniZinc syntax)

Note that every generator (e.g., `(-- 1 3)`) declares always only a single MiniZinc parameter (`i` in the example above). This follows standard Clojure conventions (e.g., similar to `let` and friends), while MiniZinc allows for declaring multiple parameters together.

A where-expression can be added after the generators, which acts as a filter. Only elements satisfying this Boolean expression are used to construct elements in the output array/set.

Example:

    (def a (array (-- 1 3) :int))
    (aggregate [i (-- 1 3)
                j (-- 1 3) 
                :where (< i j)]
      (!= (nth a i) (nth a j)))  
    ; means [a[1] != a[2], a[2] != a[3]] (in MiniZinc syntax)

The optional arguments `set-or-array` specifies whether result is array or set. It must be either `:array` or `:set`, default is `:array`.  

Example:

    (aggregate [i (-- 1 3)]
      (= (nth a i) 0)
      :set)
"
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
   "Universal quantification with list comprehension support: Logical conjunction of aggregated Boolean expressions. When applied to an empty list, forall returns true.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(forall [generators*] exp)]}
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
  "Existential quantification (logical disjunction of Boolean expressions). When applied to an empty list, exists returns false.

Unary: MiniZinc function exists.

Binary: exists with list comprehension support.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(exists [generators*] exp)]}
  ([x] (call-fn 'exists x))
  ([generators exp]
  `(format "exists(%s)" (aggregate ~generators ~exp))))

(comment
  (def a (array (-- 1 10) :int 'a))
  (exists [i (-- 1 3)] (= (nth a i) 0))
  (exists a)
  )

(defmacro xorall
  "N-ary exclusive disjunction with list comprehension support: odd number of aggregated Boolean expressions holds.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(xorall [generators*] exp)]}
  [generators exp]
  `(format "xorall(%s)" (aggregate ~generators ~exp)))

(defmacro iffall
  "N-ary bi-implication with list comprehension support: even number of aggregated Boolean expressions holds.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(iffall [generators*] exp)]}
  [generators exp]
  `(format "iffall(%s)" (aggregate ~generators ~exp)))

(defmacro sum
  "Summation

Unary: MiniZinc function sum.

Binary: sum with list comprehension support: adds aggregated expressions. If aggregated expressions are empty returns 0.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(sum [generators*] exp)]}
  ([x] (call-fn 'sum x))
  ([generators exp]
     `(format "sum(%s)" (aggregate ~generators ~exp))))

(defmacro product
  "Multiplication

Unary: MiniZinc function Multiplication.

Binary: product with list comprehension support: multiplies aggregated expressions. If aggregated expressions are empty returns 1.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(product [generators*] exp)]}
  ([x] (call-fn 'product x))
  ([generators exp]
     `(format "product(%s)" (aggregate ~generators ~exp))))

(defmacro min
  "Minimal value

Unary: MiniZinc function min.

Binary: min with list comprehension support: least element in aggregated expressions. If aggregated expressions are empty gives MiniZinc error.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(min [generators*] exp)]}
  ([x] (call-fn 'min x))
  ([generators exp]
     `(format "min(%s)" (aggregate ~generators ~exp))))

(defmacro max
  "Maximal value

Unary: MiniZinc function max.

Binary: max with list comprehension support: greatest element in aggregated expressions. If aggregated expressions are empty gives MiniZinc error.

See [[aggregate]] for list comprehension syntax and examples."
  {:forms '[(max [generators*] exp)]}
  ([x] (call-fn 'max x))
  ([generators exp]
     `(format "max(%s)" (aggregate ~generators ~exp))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditional Expressions
;;;

(comment
  ;; Tutorial p. 26
  ;; TODO: define `if`, but under name `TODO` if* ??
  ;; (if is a special form)

  ;; MiniZinc if should have same syntax as Clojure if
  
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local Variables
;;;

(comment
  ;; Tutorial p. 49

  ;; TODO: define `let`, but under name `local`
  ;; (let is a special form)

  ;; `local` should be usable like Clojure's if, but automatically assign the same names as the Clojure symbols/locals to MiniZinc params and variables

;; var s..e: x;
;; let {int: l = s div 2, int: u = e div 2, var l .. u: y} in x = 2*y


  ;; constraint let { var int: s = x1 + x2 + x3 + x4 } in l <= s /\ s <= u;
  ;; 
  ;; BUG: variable def changed: skipped var name and added init value
  (local [s (variable :int (+ x1x2 x3 x4))]
         (<= l s u))  ; BUG: <= only binary 

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
(defn variable
    "Declares a decision variable (bool, int, float, or set of int) with the given domain, and an optional variable name (string, symbol or keyword). 

Examples:

    (variable :bool)             ; a Boolean variable (no further domain specification supported)
    (variable :int)              ; an integer variable with maximum supported domain size 
    (variable (-- -1 10))        ; an integer variable with domain [-1, 10]
    (variable #{1 3 6 8})        ; an integer variable with the domain {1, 3, 6, 8}
    (variable (-- 1.0 10.0))     ; a float variable with domain [1.0, 10.0]
    (variable [:set (-- 1 3)])   ; a set of integers with the given domain (set is subset of domain)
    (variable [:set #{1 3 6 8}]) ; a set of integers with the given domain (set is subset of domain)
    (variable [:int (-- 1 3)])   ; same as (variable (-- -1 10))

    (variable (-- 1 10) 'x)      ; an integer variable named x (instead of an automatically assigned name)
"
    ([type-inst] (variable type-inst (gensym "var")))
    ([type-inst var-name]
       (let [dom-string (mk-type-inst-string type-inst)
             name-string (name var-name)]
         (tell-store! (make-aVar name-string (format "var %s: %s;" dom-string name-string))))))
    

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
  "Expects a constraint expression (a string) and turns it into a constraint statement."
  [constraint-expr]
  (tell-store! (format "constraint %s;" (expr constraint-expr))))

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
  "Concatenate an array of strings. Equivalent to folding '++' over the array, but may be implemented more efficiently."
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
  "Domain reflection: a safe approximation to the possible values of x (int)."
  [x] 
  (call-fn 'dom x))
(defn dom_array
  "Domain reflection: a safe approximation to the union of all possible values of the expressions appearing in the array x (ints)."
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
  "Check if the argument’s value is fixed at this point in evaluation. If not, abort; if so, return its value. This is most useful in output items when decision variables should be fixed -- it allows them to be used in places where a fixed value is needed, such as if-then-else conditions."
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
  "Reflection function: the index set (set of indices) of a one dimensional array."
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
  "Concatenates an array of strings a, putting a seperator string s beween adjacent strings. Returns the empty string if the array is empty."
  [s a] 
  (call-fn 'join s a))

;; (def-unary-function lb lb
;;   " function constraint")
;; (def-unary-function lb_array lb_array
;;   " function constraint")

(defn lb
  "Domain reflection: a safe approximation to the lower bound value of x (int, float, or set of int)."
  [x] 
  (call-fn 'lb x))
(defn lb_array
  "Domain reflection: a safe approximation to the lower bound of all expressions appearing in the array x (of int, float, or set of int)."
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
  "To-string conversion. Converts any value to a string for output purposes. The exact form of the resulting string is implementation-dependent."
  [x] 
  (call-fn 'show x))
(defn show_int
  "Formatted to-string conversion for integers. Converts the integer `x` into a string right justified by the number of characters `justification` (int), or left justified if that argument is negative. If `x` is not fixed, the form of the string is implementation-dependent."
  [justification x] 
  (call-fn 'show_int justification x))
(defn show_float
  "Formatted to-string conversion for floats. Converts the float `x` into a string right justified by the number of characters given by `justification` (int), or left justified if that argument is negative. The number of digits to appear after the decimal point is given by `digits` (int). It is a run-time error for `digits` to be negative. If `x` is not fixed, the form of the string is implemenation-dependent."
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
  "Return x (any type). As a side-effect, an implementation may print the string s."
  [s x] 
  (call-fn 'trace s x))

;; (def-unary-function ub ub
;;   " function constraint")
;; (def-unary-function ub_array ub_array
;;   " function constraint")

(defn ub
  "Domain reflection: a safe approximation to the upper bound value of x (int, bool, float or set of int)."
  [x] 
  (call-fn 'ub x))
(defn ub_array
  "Domain reflection: a safe approximation to the upper bound of all expres- sions appearing in the array x x (of int, float, or set of int)."
  [x] 
  (call-fn 'ub_array x))

;; (def-binary-function pow pow
;;   "power constraint")

(defn pow
  "power: x^expt"
  [x expt] 
  (call-fn 'pow x expt))

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

Constrains the elements of the array x to be all different except those elements that are assigned the value 0."
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

Requires that each item i be put into bin bin[i] such that the sum of the weights of each item, w[i], in each bin does not exceed the capacity c.
Aborts if an item has a negative weight or if the capacity is negative.
Aborts if the index sets of bin and w are not identical."
  [c bin w]
  (call-global-constraint 'bin_packing c bin w))

(defn bin_packing_capa 
  "bin_packing_capa(array[int] of int: c, array[int] of var int: bin, array[int] of int:w)

Requires that each item i be put into bin bin[i] such that the sum of the weights of each item, w[i], in each bin b does not exceed the capacity c[b].
Aborts if an item has negative weight.
Aborts if the index sets of bin and w are not identical."
  [c bin w]
  (call-global-constraint 'bin_packing_capa c bin w))

(defn bin_packing_load 
  "bin_backing_load(array[int] of var int: l, array[int] of var int: bin, array[int] of int: w)

Requires that each item i be put into bin bin[i] such that the sum of the weights of each item, w[i], in each bin b is equal to the load l[b].
Aborts if an item has negative weight.
Aborts if the index sets of bin and w are not identical."
  [l bin w]
  (call-global-constraint 'bin_packing_load l bin w))

(defn circuit
  "circuit[array[int] of var int: x)

Constraints the elements of x to define a circuit where x[i] = j mean that j is the successor of i."
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

Constrains c to greater than or equal to the number of occurrences of y in x."
  [x y c]
  (call-global-constraint 'count_geq x y c))

(defn count_gt
  "count_gt(array[int] of var int: x, var int: y, var int: c)

Constrains c to strictly greater than the number of occurrences of y in x."
  [x y c]
  (call-global-constraint 'count_gt x y c))

(defn count_leq
  "count_leq(array[int] of var int: x, var int: y, var int: c)

Constrains c to less than or equal to the number of occurrences of y in x."
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

Requires that a set of tasks given by start times s, durations d, and resource requirements r, never require more than a global resource bound b at any one time.
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

The same as x[i] = y or (= (nth x i) y). That is, y is the ith element of the array x. The difference to nth is that i can be a variable."
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

Requires that for all i, the value cover[i] appears at least lb[i] and at most ub[i] times in the array x."
  [x cover lb ub]
  (call-global-constraint 'global_cardinality_low_up x cover lb ub))

(defn global_cardinality_low_up_closed
  "global_cardinality_low_up_closed(array[int] of var int: x, array[int] of int: cover, array[int] of int: lb, array[int] of int: ub)

Requires that for all i, the value cover[i] appears at least lb[i] and at most ub[i] times in the array x.
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

Constrains two arrays to represent inverse functions of each other. All the values in each array must be within the index set of the other array."
  [f invf]
  (call-global-constraint 'inverse f invf))

(defn inverse_set
  "inverse_set(array[int] of var set of int: f, array[int] of var set of int: invf)

Constrains the two arrays f and invf so that a j is an element of f[i] if and only if i is an element of invf[j]. All the values in each array's sets must be within the index set of the other array."
  [f invf]
  (call-global-constraint 'inverse_set f invf))

(defn lex_greater
  "lex_greater(array[int] of var bool:       x, array[int] of var bool:       y)
lex_greater(array[int] of var float:      x, array[int] of var float:      y)
lex_greater(array[int] of var int:        x, array[int] of var int:        y)
lex_greater(array[int] of var set of int: x, array[int] of var set of int: y)

Requires that the array x is strictly lexicographically greater than array y.
Compares them from first to last element, regardless of indices."
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

Requires that the array x is strictly lexicographically less than array y.
Compares them from first to last element, regardless of indices."
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

Require adjacent rows and adjacent columns in the the array x to be lexicographically ordered. Adjacent rows and adjacent columns may be equal."
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

Constrains m to be the maximum of the values in x. (The array x must have at least one element.)"
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

Constrains m to be the minimum of the values in x. (The array x must have at least one element.)"
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

The sequence of values in array x (which must all be in the range 1..S) is accepted by the DFA of Q states with input 1..S and transition function d (which maps ⟨1..Q, 1..S⟩ to 0..Q) and initial state q0 (which must be in 1..Q) and accepting states F (which all must be in 1..Q). State 0 is reserved to be an always failing state.
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

Requires that in each subsequence vs[i], ..., vs[i + seq - 1] the sum of the values belongs to the interval [low, up]."
  [low up seq vs]
  (call-global-constraint 'sliding_sum low up seq vs))

(defn sort
  "sort(array[int] of var int: x, array[int] of var int: y)

Requires that the multiset of values in x is the same as the multiset of values in y but y is in sorted order.
Aborts if the cardinality of the index sets of x and y is not equal."
  [x y]
  (call-global-constraint 'sort x y))

(defn strict_lex2
  "strict_lex2(array[int, int] of var int: x)

Require adjacent rows and adjacent columns in the the array x to be lexicographically ordered. Adjacent rows and adjacent columns cannot be equal."
  [x]
  (call-global-constraint 'strict_lex2 x))

(defn subcircuit
  "subcircuit(array[int] of var int: x)

Constrains the elements of x to define a subcircuit where x[i] = j means that j is the successor of i and x[i] = i means that i is not in the circuit."
  [x]
  (call-global-constraint 'subcircuit x))

(defn sum_pred
  "sum_pred(var int: i, array[int] of set of int: sets, array[int] of int: c, var int: s)

Requires that the sum of c[i1]...c[iN] equals s, where i1..iN are the elements of the ith set in sets.
This constraint is usually named sum, but using that would conflict with the MiniZinc built-in function of the same name."
  [i sets c s]
  (call-global-constraint 'sum_pred i sets c s))

(defn table
  "table(array[int] of var bool: x, array[int, int] of bool: t)
table(array[int] of var int:  x, array[int, int] of int:  t)

Represents the constraint x is element of t where we consider each row in t to be a tuple and t as a set of tuples.
Aborts if the second dimension of t does not equal the number of variables in x.
The default decomposition of this constraint cannot be flattened if it occurs in a reified context."
  [x t]
  (call-global-constraint 'table x t))

(defn value_precede
  "value_precede(int: s, int: t, array[int] of var int: x)
value_precede(int: s, int: t, array[int] of var set of int: x)

Requires that s precede t in the array x.
For integer variables this constraint requires that if an element of x is equal to t, then another element of x with a lower index is equal to s.
For set variables this constraint requires that if an element of x contains t but not s, then another element of x with lower index contains s but not t."
  [s t x]
  (call-global-constraint 'value_precede s t x))

(defn value_precede_chain
  "value_precede_chain(array[int] of int: c, array[int] of var int: x)
value_precede_chain(array[int] of int: c, array[int] of var set of int: x)

Requires that the [[value_precede]] constraint is true for every pair of adjacent integers in c in the array x."
  [c x]
  (call-global-constraint 'value_precede_chain c x))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Solver
;;;

(defn solve 
  "Solve items specify what kind of solution is being looked for. Supported values for solver are :satisfy, :maximize, and :minimize (a keyword)."
  ([solver]
     {:pre [(#{:satisfy} solver)]}
     (tell-store! (format "solve %s;" (name solver))))
  ([solver exp]
     {:pre [(#{:maximize :minimize} solver)]}
     (tell-store! (format "solve %s %s;" (name solver) (expr exp)))))


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
  (tell-store! 
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
  (tell-store! 
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
  (tell-store! (format "output [ show(%s) ];" (expr my-var))))

;; TODO: output for multi-dimensional vector

;; TODO: make this somehow more smart by allowing for vars etc
;; e.g., at least allow for any number of args
(defn output 
  "Expects an output definition (a string) and turns it into an output statement (surround by brackets etc.)

BUG: this fn is currently far too inflexible."
  [mzn-string]
  (tell-store! (format "output [ %s ];" (expr mzn-string)))) ; \"\\n\"

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
  "Utility function for creating data files (*.dzn files) that map keys (MiniZinc variable names) to values.

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
(defmacro clj2mnz 
  "Translates a constraint problem defined in Clojure into the corresponding MiniZinc code. Expects any number of variable/parameter declarations, any number of constraints, one output, and one solver declaration, all in any order."
  [& constraints]
  `(binding [*mzn-store* []
             *included-files* #{}]
     ~@constraints
     ;; TODO: map is lazy -- make sure dynamic scope is not left
     (apply str (doall (map (fn [x#]  ; x# results in unique gensym
                              (str (cond (core/or (aVar? x#) (anArray? x#))
                                         (:mzn-string x#)
                                         (core/string? x#) x#
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

- :mzn             (string) a MiniZinc program, which can be created with other functions of clojure2minizinc wrapped into clj2mnz
- :print-cmd?      (boolean) whether or not to print the UNIX command call of the solver (for debugging)
- :print-mzn?      (boolean) whether or not to print resulting MiniZinc data and model (for debugging)
- :print-solution? (boolean) whether or not to print the result output by MiniZinc directly instead of reading it into a Clojure value (e.g., for debugging). Prints the map resulting from clojure.java.shell/sh.
- :solver          (string) solver to call
- :mznfile         (string or file) MiniZinc file path to generate and use in the background
- :data            (string) Content for a MiniZinc data file (*.dzn file). Can conveniently be created with map2minizinc 
- :num-solutions   (int) An upper bound on the number of solutions to output
- :all-solutions   (boolean) If true, return all solutions
- :options         (collection of strings) Arbitrary options given to the solver in UNIX shell syntax, e.g., [\"-a\"] for all solutions.

BUG: resulting temporary MiniZinc file is not deleted after Clojure quits."
  [mzn & {:keys [solver mznfile data
                 print-cmd?
                 print-mzn?
                 print-solution? 
                 num-solutions all-solutions?
                 options] 
          :or {solver *fd-solver*
               ;; BUG: tmp file not deleted later
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
            (throw (Exception. (format "MiniZinc: %s" (:err result)))))
          (map read-string
               (clojure.string/split (:out result) #"(\n----------\n|==========\n)")))
        (throw (Exception. (format "MiniZinc: %s" (:err result))))))))



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Abstractions like predicates 
;;;

;; TODO: 
;; - Make the doc more easy to comprehend.
;; - Consider renaming 
(defmacro def-submodel
  "Abstracts a part of a MiniZinc model as a Clojure function. Such submodels can be used as a substitute for MiniZinc predicates and functions, but calls to any submodel are resolved in the actual MiniZinc code. A submodel call returns a (MiniZinc code) string.

BUG: Unfinished def -- does not yet support the full set of arguments of clojure.core/defn. (e.g., no doc string.)"
  [name args & body]
  ;; *mzn-store* is false outside of a call to mz/clj2mnz
  `(def ~name
     (fn ~args
       (clj2mnz 
        (tell-store! 
         ~@body)))))
