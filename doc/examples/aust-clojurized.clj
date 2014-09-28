
;; Storing Variables in Other Data Structures 
;;    :PROPERTIES:
;;    :header-args: :comments org :tangle "examples/aust-clojurized.clj" 
;;    :END:

;; As mentioned above, clojure2minizinc can store MiniZinc variables and parameters in arbitrary Clojure data structures. The following example stores the variables for the colours of Australian states in a map. The same inequality constraints are applied by mapping over pairs of keywords representing these inequalities somewhat more concisely.

(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz]  ; loading clojure2minizinc.core 
            [clojure.pprint :as pprint]))   ; pprint is needed later...
 
(mz/minizinc 
 (mz/clj2mnz
  (let [nc (mz/int 'nc 3)
        states (zipmap [:wa :nt :sa :q :nsw :v :t]
                       (take 7 (repeatedly #(mz/variable (mz/-- 1 nc)))))]
    (doall (map (fn [[s1 s2]] 
                  (mz/constraint (mz/!= (s1 states) (s2 states))))
                [[:wa :nt] [:wa :sa] [:nt :sa] [:nt :q] [:sa :q] [:sa :nsw] [:sa :v] [:nsw :v]]))
    (mz/solve :satisfy)
    (mz/output-map states))))
