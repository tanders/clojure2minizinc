(ns clojure2minizinc.core
  (:require [clojure.java.shell :as shell])
  ;; http://clojuredocs.org/clojure_core/1.3.0/clojure.pprint
  (:require [clojure.pprint :as pprint])
  ;; http://raynes.github.io/fs/  https://github.com/Raynes/fs
  (:require [me.raynes.fs :as fs])
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
  "Extends *mzn-store* by given constraint."
  [constraint]
  (set! *mzn-store* (cons constraint *mzn-store*)))

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

(defrecord aDomain [min max mzn-string])
(defrecord aVariable [name domain mzn-string])
(defrecord aConstraint [args mzn-string]) ;; args for seq of expressions given to constraint
(defrecord aSolve [solver mzn-string])
(defrecord anOutput [arg mzn-string])

(comment
  (def test (aVariable. 'x (domain 1 3) (format "var %s: %s;\n" (domain 1 3) 'x)))
  (:domain test)
  (:mzn-string test)
  (type test)
  )

;;;
;;; Creating MiniZinc variables
;;;

;; TODO: find out whether there is a way to restrict the domain of an integer to only a given list of integers (i.e., "cut holes" into the domain)
(defn domain 
  "Expects a minimum an a maximum value (ints or floats) and returns a domain specification for a decision variable (ints or floats)."
  [min max]
  (aDomain. min max (pprint/cl-format nil "~S..~S" min max)))

;; TODO: Var name should be optional  
;; OK TODO: Create a central store, where variables and constraints on these variables are added. Then, variables can be stored in arbitrary Clojure data structures and constraints to these can be applied by traversing these data structures. However, the problem is to create such store locally, so that multiple csp can be created and solved concurrently. 
;; For different variable bindings in different threads I may need some form of dynamic scope. See book "The Joy of Clojure", Sec. 11.9 Vars and dynamic binding
(defn variable
  "Declares a decision variable (int or float) with the given domain and variable name (string, symbol or keyword)."
  [dom var-name]
  (tell-store (aVariable. (name var-name) dom (format "var %s: %s;\n" (:mzn-string dom) (name var-name)))))

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
  (format "constraint c;\n" c))

(defn != 
  ""
  [lh rh]
  (pprint/cl-format nil "~S != ~S" lh rh))



(comment
  

  )



;;;
;;; Solver
;;;

(defn solve 
  "Solve items specify what kind of solution is being looked for. Supported values for solver are satisfy, maximize, and minimize (a keyword)."
  [solver]
  {:pre [(#{:satisfy :maximize :minimize} solver)]}
  (format "solve %s;\n" (name solver)))


(comment
  (solve :satisfy)
  (solve :foo) ;; error -> assert failed
  )




;;;
;;; Output
;;;


;; NB: discussion of mzn_show.pl at http://www.hakank.org/minizinc/ :
;; Since version 1.0 MiniZinc don't support the output [] anymore in the external solvers (e.g. all except the minizinc solver)

;; TODO: finish definition
;; TODO: then revise definition such that it always results in a string expressing a clojure value such as a map.
;; Idea: input is also a map, where the values at keys are variables, or some other clojure data structure containing variables. This data structure is then expressed as a string.
(defn output
  "Output items are for nicely presenting the results of the model execution. Output expects any number of strings or variables."
  [arg]
  arg)

(comment
  (output "this" "is" "a" "test")
  )



;;;
;;; Communicating with MiniZinc 
;;;




;; TODO: add convenience function to create program from multiple strings
(defn minizinc 
  "Calls a MiniZinc solver on a given MiniZinc program and returns the result.

Options are

:csp      (string) a MiniZinc program, which can be created with other functions of clojure2minizinc.
:solver   (string) solver to call. 
:mznfile  (string or file) MiniZinc file generated."
  [csp & {:keys [solver mznfile] 
          :or {solver *fd-solver*
               mznfile (doto (java.io.File/createTempFile "clojure2minizinc" ".mzn") .deleteOnExit)}}]
  (println "csp:" csp "\nmznfile:" mznfile "\nsolver:" solver)
  (spit mznfile csp)
  ;; TODO: read result with read-string, once I assured that output always results in clojure-parsable output
  ;; mznfile split into filename (base-name) and dirname (parent), so that shell/sh first moves into that dir, because otherwise I got errors from *fd-solver*
  (shell/sh solver (fs/base-name mznfile) :dir (fs/parent mznfile))
  )



(comment
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
