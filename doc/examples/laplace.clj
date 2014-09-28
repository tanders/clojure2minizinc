
;; Arrays and Sets (MiniZinc tutorial, p. 15ff)
;;    :PROPERTIES:
;;    :header-args: :tangle "examples/laplace.clj" 
;;    :END:

;; This example demonstrates the use of a two-dimensional array of float variables. It models temperatures on a rectangular sheet of metal. The MiniZinc tutorial explains the details. 

;; In order to make the result better comprehensible, we will print it in table form instead of just returning the result. We need an auxiliary function that prints a table. Lets use =print-table= that is shown as an example for =get-pretty-writer= at [[http://clojure.github.io/clojure/clojure.pprint-api.html][http://clojure.github.io]] (=print-table= is only slightly edited here). This function is the reason why we =require='d =clojure.pprint= in the name space definition above.

(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz]  ; loading clojure2minizinc.core 
            [clojure.pprint :as pprint]))   ; pprint is needed later...
 
(defn print-table [column-width aseq]
  (binding [*out* (pprint/get-pretty-writer *out*)]
    (doseq [row aseq]
      (doseq [col row]
        (pprint/cl-format true "~6,2F~7,vT" col column-width))
      (prn))))

;; Now we can present the clojure2minizinc version of the Laplace model from the MiniZinc tutorial.

(let [width 5
      height 5]
  (print-table 2
   (partition (+ 1 height)  ;; add one, because array boundaries are [0, height] etc.
    (first 
     (mz/minizinc 
      (mz/clj2mnz
       (let [w (mz/int 'w width)
             h (mz/int 'h height)
             ;; array decl
             t (mz/array (list (mz/-- 0 w) (mz/-- 0 h)) [:var :float] 't)
             left (mz/variable :float 'left)
             right (mz/variable :float 'right)
             top (mz/variable :float 'top)
             bottom (mz/variable :float 'bottom)]
         ;; Laplace equation
         ;; Each internal temp. is average of its neighbours
         (mz/constraint 
          (mz/forall [i (mz/-- 1 (mz/- w 1))
                      j (mz/-- 1 (mz/- h 1))]
                     (mz/= (mz/* 4.0 (mz/nth t i j))
                           ;; Constraints like + support an arbitray number of arguments 
                           (mz/+ (mz/nth t (mz/- i 1) j)
                                 (mz/nth t i (mz/- j 1))
                                 (mz/nth t (mz/+ i 1) j)
                                 (mz/nth t i (mz/+ j 1))))))
         ;; edge constraints
         (mz/constraint (mz/forall [i (mz/-- 1 (mz/- w 1))]
                                   (mz/= (mz/nth t i 0) left)))
         (mz/constraint (mz/forall [i (mz/-- 1 (mz/- w 1))]
                                   (mz/= (mz/nth t i h) right)))
         (mz/constraint (mz/forall [j (mz/-- 1 (mz/- h 1))]
                                   (mz/= (mz/nth t 0 j) top)))
         (mz/constraint (mz/forall [j (mz/-- 1 (mz/- h 1))]
                                   (mz/= (mz/nth t w j) bottom)))
         ;; corner constraints
         (mz/constraint (mz/= (mz/nth t 0 0) 0.0))
         (mz/constraint (mz/= (mz/nth t 0 h) 0.0))
         (mz/constraint (mz/= (mz/nth t w 0) 0.0))
         (mz/constraint (mz/= (mz/nth t w h) 0.0))
         (mz/constraint (mz/= left 0.0))
         (mz/constraint (mz/= right 0.0))
         (mz/constraint (mz/= top 100.0))
         (mz/constraint (mz/= bottom 0.0))
         (mz/solve :satisfy)
         ;; 2d-array output as flat 1d array -- formatting of result done by Clojure
         (mz/output-var t) 
         ))
      :options ["-f fzn-gecode"]
      ; :solver "mzn-g12mip"
      )))))
