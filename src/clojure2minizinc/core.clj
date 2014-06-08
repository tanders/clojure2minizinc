; TODO: break into multiple namespaces for clarification, e.g., for integer, set and float domains and core defs.

;; TODO: ? Add support for arrays

;; TODO: Add some examples (in extra files) -- just start with examples from MiniZinc tutorial 



(ns clojure2minizinc.core
  ;; make explicit shadowing a range of core clojure functions etc
  (:refer-clojure :exclude [> >= <= < = == != -> + - * / mod assert concat min max 
                            int float and]) ; not or
  (:require [clojure.core :as core]
            [clojure.java.shell :as shell]
            ;; http://clojuredocs.org/clojure_core/1.3.0/clojure.pprint
            [clojure.pprint :as pprint]
            ;; http://raynes.github.io/fs/  https://github.com/Raynes/fs
            [me.raynes.fs :as fs]
            [clojure.walk :as walk] ;; not used yet
            [clojure.java.io :as io]) ;; only required for testing here
  )

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

;;;
;;; Data structure definitions 
;;; (I could perhaps only use strings, but more additional explicit information could be helpful later)
;;;

(defrecord aVar [name mzn-string])
(defn aVar? 
  "Returns true if x is aVar."
  [x]
  (core/= (type x) clojure2minizinc.core.aVar))

(comment
  (def myVar (aVar. 'x (format "var %s: %s;" (-- 1 3) (name 'x))))
  (:name myVar)
  (:mzn-string myVar)
  (aVar? myVar)
)

(defn- expr
  "Returns an expression (e.g., a string with a MiniZinc expression). If x is aVar, it returns its name. Otherwise it returns the value that corresponds to x (e.g., a string remains that string etc.)."
  [x]
  ;; (pprint/pprint (list literal-tests (some #(% x) literal-tests))) 
  (cond (aVar? x) (:name x)
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

  (def myVar (aVar. 'x (format "var %s: %s;" (-- 1 3) (name 'x))))

  (expr "myVar")
  (expr myVar)
  (expr 1)
  (expr :test)
  
  ;; errors 
  (expr ['myVar])
  (expr 'x)
  )

;;;
;;; Creating MiniZinc parameters (quasi constants)
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
      (aVar. (name var-name) 
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

(defn set-of-int
  "Declares a set of integers parameter (quasi a constant) with an optional init-value and optional name (a string, symbol or keyword, default is a gensym-ed name). The init value is a range, e.g., `(-- 1 10)` meaning the set contains all integers in the range. The default is nil, meaning no initialisation."
  ([] (set-of-int (gensym "Set"))) 
  ([var-name] (set-of-int var-name nil))
  ([var-name init-value] (par "set of int" var-name init-value)))

(comment
  (int)
  (int 'test)
  (int 'test 3)
  (bool)

  (set-of-int)
  (set-of-int "MySet")
  (set-of-int "MySet" (-- 1 10))
  )


;; TODO: 
;; array literal
(defn array   
  "Declares a one- or multi-dimensional array.

Arguments are

param-type: The type of the variables contained in the array (a string, symbol or keyword; can be int, float, bool, string and \"set of int\").
index-set: The explicitly declared indices of the array. Either an integer range (declared with function --), a set variable initialised to an integer range, or for multi-dimensional arrays a list of integer ranges and set variables.
var-name: an optional name for the array (a string, symbol or keyword) Default is a gensym-ed name."
  ([param-type index-set] (array param-type index-set (gensym (str (name param-type) "_array"))))
  ([param-type index-set var-name]
     {:pre [(#{"int" "float" "bool" "string" "set of int"} (name param-type))]}
     ;; (println (pprint/cl-format nil "param-type: ~S, init-value: ~S, var-name ~S" param-type init-value var-name))
     (tell-store
      (aVar. (name var-name) 
             (format "array[%s] of var %s: %s;" 
                     (cond (aVar? index-set) (:name index-set)
                           (string? index-set) index-set
                           (list? index-set) (apply str
                                                    (interpose ", "
                                                               (map #(cond (aVar? %) (:name %)
                                                                           (string? %) %
                                                                           :else (throw 
                                                                                  (Exception. 
                                                                                   (pprint/cl-format nil "Not allowed as array index-set: ~S of type ~S" 
                                                                                                     % (type %)))))
                                                                    index-set)))
                           :else (throw (Exception. 
                                         (pprint/cl-format nil "Not allowed as array index-set: ~S of type ~S" 
                                                           index-set (type index-set)))))
                     (name param-type) 
                     (name var-name))))))



(comment
  (array :bool (-- 0 10))
  (array :int (-- 0 10) "test") ;"array[0..10] of var int: test;"

  (array :int (set-of-int (-- 1 10)))
  (array :int (list (-- 0 10) (-- 0 10) (set-of-int (-- 1 10))))
  )


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

;;;
;;; Creating MiniZinc vars
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
  )

;; You cannot shadow special forms, and therefore a function cannot be called var
(defn variable
  "Declares a decision variable (int or float) with the given domain and an optional variable name (string, symbol or keyword)."
  ([dom] (variable dom (gensym "var")))
  ([dom var-name]
     (tell-store (aVar. (name var-name) (format "var %s: %s;" dom (name var-name))))))

(comment
  (-- 1 3)
  (-- 1.0 3.0)
  (binding [*mzn-store* ()]
    (variable (-- 1 3) 'x))
  (binding [*mzn-store* ()]
    (variable (-- 1 3) :x))
  (variable (-- 1 3))
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

;;;
;;; Constraints
;;;

;; TODO: add doc string 
(defn constraint 
  ""
  [c]
  (tell-store (format "constraint %s;" (expr c))))

(defmacro ^:private def-unary-operator
  "Defines a function that outputs the code for a MiniZinc unary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     [arg#]
     (format ~(str operation " %s")  (expr arg#))))

;; TODO: add parenthesis around operator to clarify [Vorrang], just in case 
(defmacro ^:private def-binary-operator
  "Defines a function that outputs the code for a MiniZinc binary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     [lh# rh#]
     (format ~(str "%s " operation " %s") (expr lh#) (expr rh#))))

(defmacro ^:private def-unary-and-binary-operator
  "Defines a function that outputs the code for a MiniZinc unary and binary operator."
  [op-name operation doc-string]
  `(defn ~op-name
     ~doc-string
     ([arg#]
        (format ~(str operation " %s") (expr arg#)))
     ([lh# rh#]
        (format ~(str "%s " operation " %s") (expr lh#) (expr rh#)))))

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

(def-unary-operator not not 
  "Logical not constraint")

(def-unary-and-binary-operator + + 
  "+ constraint")
(def-unary-and-binary-operator - -
  "- constraint")

(def-binary-operator <-> <->
  "Logical equivalence constraint")
(def-binary-operator -> ->
  "Logical implication constraint")
(def-binary-operator <- <-
  "")
(def-binary-operator or "\\/"
  "Logical or constraint")
(def-binary-operator xor xor
  "Logical and constraint")
(def-binary-operator and "/\\"
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
(def-binary-operator ++ ++
  "Concatenates strings and arrays.")
(def-binary-operator * *
  " constraint")
(def-binary-operator / /
  " constraint")
(def-binary-operator div div
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
(def-unary-function assert assert
  " function constraint")
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
(def-unary-function min min
  " function constraint")
(def-unary-function max max
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


(comment
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




;;;
;;; Output
;;;


;; NB: discussion of mzn_show.pl at http://www.hakank.org/minizinc/ :
;; Since version 1.0 MiniZinc don't support the output [] anymore in the external solvers (e.g. all except the minizinc solver)

;; TMP: def until a more general function output that translates arbitrary data structures containing variables is defined.
;; ??? TODO: replace by more general variant that supports arbitrary Clojure data strutures.
;; NOTE: “fancy” output can be (very) slow (http://web.it.kth.se/~cschulte/events/SweConsNet-2012/hakan.pdf)
(defn output-map
  "[TMP function] Expects a map containing MiniZinc variables and returns a string formatted for MiniZinc to output a Clojure map for Clojure to read."
  [my-map]
  (tell-store 
   (str "output [\"{\", " 
        ;; BUG: of REPL? Strings containing parentheses can cause blocking.
        ;; waiting for a response at https://groups.google.com/forum/#!forum/clojure-tools
        (apply str (doall (map (fn [[key val]] (str "\" " key " \"" ", show(" (:name val) "), ")) my-map))) 
        "\"}\\n\"];")))

(comment
  (def x (variable (domain 1 3) 'x))
  (def y (variable (domain 4 6) 'y))
  (print (output-map {:x x :y y}))

  (str "output [\"{\", " 
               (apply str (doall (map (fn [[key val]] (str "\" " key " \"" ", show(" (:name val) "), ")) {:x x :y y}))) 
               "\"}\\n\"];")

  (apply str '("\" :x \", show(x), " "\" :y \", show(y), "))
  (apply str '("show(x)" " bar"))

  ;; string with parenthesis blocks str? 
  (apply str '(")" " bar"))

  ;; BUG: in Clojure? Strings containing parentheses can cause problems?
  ;; This works in a plain lein repl, so possibly the bug is in CIDER? 
  (str "(")
  (str ")")

  (. "test" (toString))
  (. "(" (toString)) ;; blocks
  (. ")" (toString))

  ; even evaluating only string containing parentheses causes problems -- seemingly blocks the repl
  (def parString "()")

  ")"

  )

(comment
;; TODO: finish definition
;; TODO: then revise definition such that it always results in a string expressing a clojure value such as a map.
;; Idea: input is also a map, where the values at keys are variables, or some other clojure data structure containing variables. This data structure is then expressed as a string.
(defn output
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
  "Utility function for creating simple data files (*.dzn files)"
  [mzn-map]
  (apply str (map (fn [[key val]] (str (= key val) "; "))
                  mzn-map)))

(comment
  (map2minizinc {:x 1 :y 2 :z 3})
  )


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
                              (str (cond (aVar? x#)
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

:mzn            (string) a MiniZinc program, which can be created with other functions of clojure2minizinc wrapped into clj2mnz
:print-mzn?     (boolean) whether or not to print resulting MiniZinc program (for debugging)
:solver         (string) solver to call
:mznfile        (string or file) MiniZinc file path to generate and use in the background
:data           (string) Content for a MiniZinc data file (*.dzn file). Can conveniently be created with map2minizinc 

:num-solutions  (int) An upper bound on the number of solutions to output
:all-solutions  (boolean) If true, return all solutions
"
  [mzn & {:keys [solver mznfile data
                 print-mzn?
                 num-solutions all-solutions?] 
          :or {solver *fd-solver*
               mznfile (doto (java.io.File/createTempFile "clojure2minizinc" ".mzn") .deleteOnExit)
               data false
               print-mzn? false
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
        dummy (pprint/pprint sh-args)
        result (apply shell/sh sh-args)]
    (if (core/= (:exit result) 0)
      (map read-string
           (clojure.string/split (:out result) #"(\n----------\n|==========\n)"))
      (throw (Exception. (format "MiniZinc error: %s" (:err result)))))))



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
