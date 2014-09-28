
;; An Arithmetic Optimisation Example (MiniZinc tutorial, p. 6ff)
;;    :PROPERTIES:
;;    :header-args: :tangle "examples/cakes.clj" 
;;    :END:

;; The MiniZinc tutorial continues with an optimisation example, that computes the number of banana (=b=) and chocolate (=c=) cakes to bake for maximum profit given the recipes for these cakes, the amount of ingredients, and the price at which each cake can be sold. 

;; The corresponding clojure2minizinc code is shown below. Please see the MiniZinc tutorial for an explanation of this model.

(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz]  ; loading clojure2minizinc.core 
            [clojure.pprint :as pprint]))   ; pprint is needed later...
 
(mz/minizinc 
 (mz/clj2mnz
  (let [b (mz/variable (mz/-- 1 100))
        c (mz/variable (mz/-- 1 100))]
    ;; flour
    (mz/constraint (mz/<= (mz/+ (mz/* 250 b)
                                (mz/* 200 c))
                          4000))
    ;; bananas
    (mz/constraint (mz/<= (mz/* 2 b) 6))
    ;; sugar
    (mz/constraint (mz/<= (mz/+ (mz/* 75 b)
                                (mz/* 150 c))
                          2000))
    ;; butter 
    (mz/constraint (mz/<= (mz/+ (mz/* 100 b)
                                (mz/* 150 c))
                          500))
    ;; cocoa
    (mz/constraint (mz/<= (mz/* 75 c) 500))
    ;; maximise profit
    (mz/solve :maximize (mz/+ (mz/* 400 b) (mz/* 450 c)))
    (mz/output-map {:banana-cakes b :chocolate-cakes c}))))
