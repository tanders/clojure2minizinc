(ns clojure2minizinc.more_examples
  (:require [clojure2minizinc.core :as mz]  
            ;; [clojure.java.shell :as shell]
            ;; [clojure.pprint :as pprint]
            ))

;; Higher-order programming in MiniZinc

;; mapping a MiniZinc record, applying some constraint to each of its elements :)
(map (fn [element] (mz/constraint (mz/< (mz/+ element 1) 10)))
     (mz/array->clj-seq (mz/array (mz/-- 1 3) :bool)))

