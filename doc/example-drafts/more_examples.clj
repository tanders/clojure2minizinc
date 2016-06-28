(ns clojure2minizinc.more_examples
  (:require [clojure2minizinc.core :as mz]  
            ;; [clojure.java.shell :as shell]
            ;; [clojure.pprint :as pprint]
            ))

;;
;; Higher-order programming in MiniZinc
;;

;; mapping a MiniZinc record, applying some constraint to each of its elements :)
(map (fn [element] (mz/constraint (mz/< (mz/+ element 1) 10)))
     (mz/array->clj-seq (mz/array (mz/-- 1 3) :bool)))


;;
;; all interval series
;;

(comment
  
  ;; !! TODO: unfinished example.  
  ;; For role model see local: ~/oz/music/Strasheela/strasheela/strasheela/examples/01-AllIntervalSeries.oz
  (let [l 12]
    (mz/minizinc 
     (mz/clj2mnz
      (let [;; TODO: format for array of vars not yet clear
            xs (mz/array (mz/-- 0 (- l 1)) (mz/-- 0 (- l 1)) 'xs) 
            dxs (mz/array (mz/-- 1 (- l 1)) (mz/-- 1 (- l 1)) 'dxs)
            ;; xs_s (mz/array->clj-seq xs)
            ;; dxs_s (mz/array->clj-seq dxs)
            ]
        (mz/solve :satisfy)
        (mz/output-map {:xs xs :dxs dxs})))
     :num-solutions 1
     ;; :all-solutions? true
     :print-mzn? true
     ))

  )




