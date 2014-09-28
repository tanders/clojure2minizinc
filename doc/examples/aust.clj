
;; Our First Example (MiniZinc tutorial, p. 3ff)
;;    :PROPERTIES:
;;    :header-args: :tangle "examples/aust.clj"
;;    :END:

;; The first example defines a map-colouring of Australia. The [[http://www.minizinc.org/downloads/doc-latest/minizinc-tute.pdf][MiniZinc tutorial]], p. 3ff, motivates and explains this model. This tutorial assumes that you read the MiniZinc tutorial alongside. To help you with that, even the same headlines are used.

;; Notice that most functions in this example are in the =mz= namespace, but the decision variables (and an int) of the model are stored with the standard Clojure =let= form -- clojure2minizinc allows to store MiniZinc variables and parameters (constants) in arbitrary Clojure data structures.

(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz]  ; loading clojure2minizinc.core 
            [clojure.pprint :as pprint]))   ; pprint is needed later...
 
(def aust
  (mz/clj2mnz
   (let [nc (mz/int 'nc 3)              ; number of colours
         wa (mz/variable (mz/-- 1 nc))  ; 7 variables for 7 states
         nt (mz/variable (mz/-- 1 nc))
         sa (mz/variable (mz/-- 1 nc))
         q (mz/variable (mz/-- 1 nc))
         nsw (mz/variable (mz/-- 1 nc))
         v (mz/variable (mz/-- 1 nc))
         t (mz/variable (mz/-- 1 nc))]
     (mz/constraint (mz/!= wa nt))
     (mz/constraint (mz/!= wa sa))
     (mz/constraint (mz/!= nt sa))
     (mz/constraint (mz/!= nt q))
     (mz/constraint (mz/!= sa q))
     (mz/constraint (mz/!= sa nsw))
     (mz/constraint (mz/!= sa v))
     (mz/constraint (mz/!= nsw v))
     (mz/solve :satisfy)
     (mz/output-map {:wa wa :nt nt :sa sa :q q :nsw nsw :v v :t t}))))

;; It returns the following result. Different numbers encode different colours on the map for the Australian states.

;;   =; => ({:wa 1, :nt 2, :sa 3, :q 1, :nsw 2, :v 1, :t 1})=

;; In this example, a solution is wrapped in a Clojure map. You can ask for multiple solutions, if you like.

(mz/minizinc aust :num-solutions 3)

;; =; => ({:wa 1, :nt 2, :sa 3, :q 1, :nsw 2, :v 1, :t 1} {:wa 2, :nt 1, :sa 3, :q 2, :nsw 2, :v 1, :t 1} {:wa 1, :nt 3, :sa 2, :q 1, :nsw 3, :v 1, :t 1})=


;; In fact, you can specify arbitrary options supported by the =minizinc= shell command. The next code line specifies to use (the FlatZinc interpreter of) [[http://www.gecode.org/flatzinc.html][Gecode]] (which must of could be installed). This solver won many [[http://www.minizinc.org/challenge.html][MiniZinc Challenges]] and is much more efficient than the solver that =minizinc= uses by default. For this simple model this makes no real difference (most time is spend on the [[http://ww2.cs.mu.oz.au/~sbrand/project/minizinc07.pdf][translation of MiniZinc to FlatZinc]] here), but for more complex problems the difference can be huge.

(mz/minizinc aust :options ["-f fzn-gecode"])

;; The solution happens to be different, because different solvers may use different search strategies, which can lead to different first solutions. (MiniZinc also allows to specify the search strategy, [[http://www.minizinc.org/workshop2011/mzn2011_submission_1.pdf][within certain bounds]]). 

;;   =; => ({:wa 3, :nt 2, :sa 1, :q 3, :nsw 3, :v 2, :t 1})=  

;; In the next call we ask Gecode to use 8 threads and/or cores for a parallel search. Again, this makes only a real difference for more complex problems.

(mz/minizinc aust :options ["-f fzn-gecode" "-p8"])
