
;; Real Number Solving (MiniZinc tutorial, p. 11ff)
;;    :PROPERTIES:
;;    :header-args: :tangle "examples/loan.clj" 
;;    :END:

;; The next example demonstrates constraint programming on "real numbers" (floating point variables). The example models the repayment of a loan with interest over four quarters.

;; The model is also parameterised -- values for variables =r=, =p= and so forth can be specified to the solver. Remember that we must specify explicit names for these variables (they should not be named automatically).

;; # TODO: revise m/output-map -- no parentheses. What about parentheses around expressions at values?

(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz]  ; loading clojure2minizinc.core 
            [clojure.pprint :as pprint]))   ; pprint is needed later...
 
(def loan
  (mz/clj2mnz
   (let [r (mz/variable :float 'r)           ; quarterly repayment
         p (mz/variable :float 'p)           ; principal initially borrowed
         i (mz/variable (mz/-- 0.0 10.0) 'i) ; interest rate
         ;; intermediate variables 
         b1 (mz/variable :float 'b1)         ; balance after one quarter
         b2 (mz/variable :float 'b2)         ; balance after two quarters
         b3 (mz/variable :float 'b3)         ; balance after three quarters
         b4 (mz/variable :float 'b4)]        ; balance owing at end
     (mz/constraint (mz/= b1 (mz/- (mz/* p (mz/+ 1.0 i)) r)))
     (mz/constraint (mz/= b2 (mz/- (mz/* b1 (mz/+ 1.0 i)) r)))
     (mz/constraint (mz/= b3 (mz/- (mz/* b2 (mz/+ 1.0 i)) r)))
     (mz/constraint (mz/= b4 (mz/- (mz/* b3 (mz/+ 1.0 i)) r)))
     (mz/solve :satisfy)
     (mz/output-map {:borrowing p :interest-rate (mz/* i 100.0)
                     :repayment-per-quarter r
                     :owing-at-end b4}))))

;; The default MiniZinc solver (=mzn-g12fd=) does not support floating point variables, so we can use Gecode again. A solution is shown below the solver call.

(mz/minizinc loan :options ["-f fzn-gecode"] 
  :data (mz/map2minizinc {:i 0.04 :p 1000.0 :r 260.0}))

;; =; => ({:borrowing 1000.0, :interest-rate 4.00000000000001, :repayment-per-quarter 260.0, :owing-at-end 65.7779200000003})=

;; In constraint programming any variable can be quasi input or output of the algorithm. Instead of setting the values for =r=, =p= and =i= in the solver call, we can set the values for other variables. By setting =b4= to 0 we specify that the loan is fully payed back after four quarters.

(mz/minizinc loan :options ["-f fzn-gecode"] 
  :data (mz/map2minizinc {:i 0.04 :p 1000.0 :b4 0.0}))

;; =; => ({:borrowing 1000.0, :interest-rate 4.00000000000001, :repayment-per-quarter 275.490045364803, :owing-at-end 0.0})=

;; Here are again other variables set before the search.

(mz/minizinc loan :options ["-f fzn-gecode"] 
  :data (mz/map2minizinc {:i 0.04 :r 250.0 :b4 0.0}))

;; =; => ({:borrowing 907.473806064214, :interest-rate 4.00000000000001, :repayment-per-quarter 250.0, :owing-at-end 0.0})=


;; If you do not have Gecode installed, you can also use the solver =mzn-g12mip=, which comes with MiniZinc. The result happens to be slightly different.

(mz/minizinc loan :solver "mzn-g12mip"
  :data (mz/map2minizinc {:i 0.04 :r 250.0 :b4 0.0}))
