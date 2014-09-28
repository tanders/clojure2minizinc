
;; Datafiles and Assertions (MiniZinc tutorial, p. 8ff)
;;    :PROPERTIES:
;;    :header-args: :tangle "examples/cakes2.clj" 
;;    :END:

;; In the previous example, the amount of each ingredient was fixed in the model. MiniZinc supports parameterising models, where MiniZinc parameters or variables are declared but not further initialised. Values for this parameters/variables are specified outside of the model to the solver, usually with MiniZinc data files. 

;; The clojure2minizinc version of the parameterised model is shown below. Again, please see the MiniZinc tutorial (p. 8ff) for an explanation of this model. 

;; Note that we must specify explicit names for the parameters of a parameterised model (here =flour=, =banana=, =sugar=, and so forth), so that these names are the same as in the parameter file (i.e., automatically generated names would not work).

(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz]  ; loading clojure2minizinc.core 
            [clojure.pprint :as pprint]))   ; pprint is needed later...
 
(def cakes2
  (mz/clj2mnz
   (let [flour (mz/int 'flour)
         banana (mz/int 'banana)
         sugar (mz/int 'sugar)
         butter (mz/int 'butter)
         cocoa (mz/int 'cocoa)]
     (mz/constraint (mz/assert (mz/>= flour 0) "Amount of flour must not be negative"))
     (mz/constraint (mz/assert (mz/>= banana 0) "Amount of banana must not be negative"))
     (mz/constraint (mz/assert (mz/>= sugar 0) "Amount of sugar must not be negative"))
     (mz/constraint (mz/assert (mz/>= butter 0) "Amount of butter must not be negative"))
     (mz/constraint (mz/assert (mz/>= cocoa 0) "Amount of cocoa must not be negative"))
     (let [b (mz/variable (mz/-- 1 100))
           c (mz/variable (mz/-- 1 100))]
       ;; flour
       (mz/constraint (mz/<= (mz/+ (mz/* 250 b)
                                   (mz/* 200 c))
                             flour))
       ;; bananas
       (mz/constraint (mz/<= (mz/* 2 b) banana))
       ;; sugar
       (mz/constraint (mz/<= (mz/+ (mz/* 75 b)
                                   (mz/* 150 c))
                             sugar))
       ;; butter 
       (mz/constraint (mz/<= (mz/+ (mz/* 100 b)
                                   (mz/* 150 c))
                             butter))
       ;; cocoa
       (mz/constraint (mz/<= (mz/* 75 c) cocoa))
       ;; maximise profit
       (mz/solve :maximize (mz/+ (mz/* 400 b) (mz/* 450 c)))
       (mz/output-map {:banana-cakes b :chocolate-cakes c})))))

;; In clojure2minizinc, the parameters for a model are given directly to the solver. The code below specifies the same amounts of ingredients for the cakes as the original example, and therefore the result is the same.

(mz/minizinc cakes2
  :data (mz/map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500}))

;; =; => ({:banana-cakes 2, :chocolate-cakes 2})=

;; Different amounts have a different optimal result.

(mz/minizinc cakes2
  :data (mz/map2minizinc {:flour 8000 :banana 11 :sugar 3000 :butter 1500 :cocoa 800}))
