(ns clojure2minizinc.mapping_examples
  (:require [clojure2minizinc.core :as mz]  
            ;; [clojure.java.shell :as shell]
            ;; [clojure.pprint :as pprint]
            ))

;; Higher-order programming in MiniZinc

;; mapping a MiniZinc record, applying some constraint to each of its elements :)
(map (fn [element] (constraint (< (+ element 1) 10)))
     (array->clj-list (array (-- 1 3) :bool)))


