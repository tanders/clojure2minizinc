(ns clojure2minizinc.core
  (:require [clojure.java.shell :as shell])
  ;; http://clojuredocs.org/clojure_core/1.3.0/clojure.pprint
  (:require [clojure.pprint :as pprint])
  ;; http://raynes.github.io/fs/  https://github.com/Raynes/fs
  (:require [me.raynes.fs :as fs])
  (:require [clojure.walk :as walk]) ;; not used yet
  )

;; (require '[clojure2minizinc.core :as mzn])

;;;
;;; Customization
;;;


;; Path to shell apps
;; (def *mzn2fzn* "Path to the mzn2fzn executable" 
;;   "/Applications/minizinc-1.6/bin/mzn2fzn")
;; TODO: Find out -- use def or defvar? And does def support a doc string?
(def *fd-solver* "Path to constraint solver for finite domain (integers)" 
  "mzn-g12fd")


;;;
;;; Defining the store for storing all information about a CSP
;;;

;; Sending all constraints to a single store instead of returning them from functions like constraint allows, e.g., to store minizinc variables in arbitrary clojure data structures and freely traversing such data structure for applying constraints to these, without worrying how to collect the constraint information.
(def ^:dynamic *mzn-store*
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

(defrecord aVariable [name domain mzn-string])

(defn- extract-mzn-string 
  "Returns the name of aVariable instances (called within constraint expressions), or simply argument if arg is a string."
  [x]
  (cond (= (type x) clojure2minizinc.core.aVariable) (:name x)
        (string? x) x
        :else (throw (Exception. (pprint/cl-format nil "extract-mzn-string cannot handle ~S of type ~S" x (type x))))))


(comment
  (def test (aVariable. 'x (domain 1 3) (format "var %s: %s;" (:mzn-string (domain 1 3)) (name 'x))))
  (:domain test)
  (:name test)
  (:mzn-string test)
  (= (type test) clojure2minizinc.core.aVariable)

  (extract-mzn-string "test")
  (extract-mzn-string test)
  (extract-mzn-string ['test])
  )

;;;
;;; Creating MiniZinc variables
;;;

;; TODO: find out whether there is a way to restrict the domain of an integer to only a given list of integers (i.e., "cut holes" into the domain)
(defn domain 
  "Expects a minimum an a maximum value (ints or floats) and returns a domain specification for a decision variable (ints or floats)."
  [min max]
  (pprint/cl-format nil "~S..~S" min max))

;; TODO: Var name should be optional  
;; OK TODO: Create a central store, where variables and constraints on these variables are added. Then, variables can be stored in arbitrary Clojure data structures and constraints to these can be applied by traversing these data structures. However, the problem is to create such store locally, so that multiple csp can be created and solved concurrently. 
;; For different variable bindings in different threads I may need some form of dynamic scope. See book "The Joy of Clojure", Sec. 11.9 Vars and dynamic binding
(defn variable
  "Declares a decision variable (int or float) with the given domain and variable name (string, symbol or keyword)."
  [dom var-name]
  (tell-store (aVariable. (name var-name) dom (format "var %s: %s;" dom (name var-name)))))

(comment
  (domain 1 3)
  (domain 1.0 3.0)
  (binding [*mzn-store* ()]
    (variable (domain 1 3) 'x))
  (binding [*mzn-store* ()]
    (variable (domain 1 3) :x))
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



;;;
;;; Constraints
;;;

(defn constraint 
  ""
  [c]
  (tell-store (format "constraint %s;" (extract-mzn-string c))))

;; constraint expressions just strings for now. Also, should not be told store! 
;; TODO: consider defining defconstraint (macro) or make-constraint (function) to simplify the definition of new constraint expressions (hiding the tell-store etc.) 
;; TODO: Consistently rename constraint so that it does not shadow basic built-ins (e.g., use c!= for constraint != instead?)
(defn != 
    ""
  [lh rh]
  (format "%s != %s" (extract-mzn-string lh) (extract-mzn-string rh)))


(comment
  (defn != 
    ""
  [lh rh]
  (pprint/cl-format nil "~S != ~S" (extract-mzn-string lh) (extract-mzn-string rh)))

  
  )



;;;
;;; Solver
;;;

(defn solve 
  "Solve items specify what kind of solution is being looked for. Supported values for solver are satisfy, maximize, and minimize (a keyword)."
  [solver]
  {:pre [(#{:satisfy :maximize :minimize} solver)]}
  (tell-store (format "solve %s;" (name solver))))


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
(defn output-map
  "[TMP function] Expects a map containing MiniZinc variables and returns a string formatted for MiniZinc to output a Clojure map for Clojure to read.
TODO: replace by more general variant that supports arbitrary Clojure data strutures."
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
  (= (type x) clojure2minizinc.core.aVariable) 
  (:name x)

  (name :x)

  (walk/walk extract-mzn-string identity
             {:x x :y y})


  (walk/walk #(if (= (type %) clojure2minizinc.core.aVariable) 
                  (:name %)
                  %)
               identity
     {:x x :y y})
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
                              (str (cond (= (type x#) clojure2minizinc.core.aVariable) (:mzn-string x#)
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
    (let [a (variable (domain 1 3) 'a) ;; mzn var naming redundant, but ensures var name in *.mzn file
          b (variable (domain 1 3) 'b)]
      (constraint (!= a b))
      (solve :satisfy)
      (output-map {:a a :b b})
      (pprint/pprint *mzn-store*)
      )))
  )



;; TODO: incorporate clj2mnz into minizinc (turning minizinc into a macro)?
;; TODO: allow for multiple solutions: split string with multiple solution along the "--------" marking, and read each solution individually
;; TODO: Support all solver arguments as keywords (e.g., how many solutions) 
(defn minizinc 
  "Calls a MiniZinc solver on a given MiniZinc program and returns a list of one or more solutions.

Options are

:mzn            (string) a MiniZinc program, which can be created with other functions of clojure2minizinc
:solver         (string) solver to call
:mznfile        (string or file) MiniZinc file generated
:print-mzn?     (boolean) whether or not to print resulting MiniZinc program (for debugging)

Solver options
:num-solutions  (int) An upper bound on the number of solutions to output
"
  [mzn & {:keys [solver mznfile print-mzn?
                 num-solutions] 
          :or {solver *fd-solver*
               mznfile (doto (java.io.File/createTempFile "clojure2minizinc" ".mzn") .deleteOnExit)
               print-mzn? false
               num-solutions 1}}]
  ;; (println "mzn:" mzn "\nmznfile:" mznfile "\nsolver:" solver)
  (when print-mzn? (println mzn))
  (spit mznfile mzn)
  ;; mznfile split into filename (base-name) and dirname (parent), so that shell/sh first moves into that dir, because otherwise I got errors from *fd-solver*
  (let [result (shell/sh solver 
                         (format "-n%s" num-solutions) 
                         (fs/base-name mznfile)
                         :dir (fs/parent mznfile)
                         )]
    (if (= (:exit result) 0)
      (map read-string
           (clojure.string/split (:out result) #"\n----------\n"))
      (throw (Exception. (format "MiniZinc error: %s" (:err result)))))))


(comment
  ;; !! NB: first mini version running :)
  (minizinc 
   (clj2mnz
    (let [a (variable (domain -1 1) 'a) ;; mzn var naming redundant, but ensures var name in *.mzn file
          b (variable (domain -1 1) 'b)]
      (constraint (!= a b))
      (solve :satisfy)
      (output-map {:a a :b b})
      ;; (pprint/pprint *mzn-store*)
      ))
   :print-mzn? true
   :num-solutions 3
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
