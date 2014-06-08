(ns clojure2minizinc.examples
  (:require [clojure2minizinc.core :as m] ; 
            ;; [clojure.java.shell :as shell]
            [clojure.pprint :as pprint])
  )

 

;; Examples of MiniZinc tutorial, translated to Clojure

;; Colouring Australia using nc colours (tutorial of version 1.6, p. 4)
;; Version as in tutorial, only "clojurized"
(m/minizinc 
 (m/clj2mnz
  (let [nc (m/int 'nc 3)     
        wa (m/variable (m/-- 1 nc))
        nt (m/variable (m/-- 1 nc))
        sa (m/variable (m/-- 1 nc))
        q (m/variable (m/-- 1 nc))
        nsw (m/variable (m/-- 1 nc))
        v (m/variable (m/-- 1 nc))
        t (m/variable (m/-- 1 nc))]
    (m/constraint (m/!= wa nt))
    (m/constraint (m/!= wa sa))
    (m/constraint (m/!= nt sa))
    (m/constraint (m/!= nt q))
    (m/constraint (m/!= sa q))
    (m/constraint (m/!= sa nsw))
    (m/constraint (m/!= sa v))
    (m/constraint (m/!= nsw v))
    (m/solve :satisfy)
    (m/output-map {:wa wa :nt nt :sa sa :q q :nsw nsw :v v :t t})
    ))
 ;; :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
; => ({:wa 1, :nt 2, :sa 3, :q 1, :nsw 2, :v 1, :t 1})


;; Colouring Australia using nc colours
;; version using Clojure abstraction means
(m/minizinc 
 (m/clj2mnz
  (let [nc (m/int 'nc 3)
        states (zipmap [:wa :nt :sa :q :nsw :v :t]
                       (take 7 (repeatedly #(m/variable (m/-- 1 nc)))))]
    ;; enforce that lazy map is actually computed with doall
    (doall (map (fn [[s1 s2]] 
                  (m/constraint (m/!= (s1 states) (s2 states))))
                [[:wa :nt]
                 [:wa :sa]
                 [:nt :sa]
                 [:nt :q]
                 [:sa :q]
                 [:sa :nsw]
                 [:sa :v]
                 [:nsw :v]]))
    (m/solve :satisfy)
    (m/output-map states)
    ;; (pprint/pprint *mzn-store*)
    ))
 :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
; Same result as above (order in map reversed)
; => ({:t 1, :v 1, :nsw 2, :q 1, :sa 3, :nt 2, :wa 1})



;; Baking cakes for the school fete (tutorial p. 7)
(m/minizinc 
 (m/clj2mnz
  (let [b (m/variable (m/-- 1 100))
        c (m/variable (m/-- 1 100))]
    ;; flour
    (m/constraint (m/<= (m/+ (m/* 250 b)
                             (m/* 200 c))
                   4000))
    ;; bananas
    (m/constraint (m/<= (m/* 2 b) 6))
    ;; sugar
    (m/constraint (m/<= (m/+ (m/* 75 b)
                             (m/* 150 c))
                   2000))
    ;; butter 
    (m/constraint (m/<= (m/+ (m/* 100 b)
                             (m/* 150 c))
                   500))
    ;; cocoa
    (m/constraint (m/<= (m/* 75 c) 500))
    ;; maximise profit
    (m/solve :maximize (m/+ (m/* 400 b) (m/* 450 c)))
    (m/output-map {:banana-cakes b :chocolate-cakes c})
    ;; (pprint/pprint *mzn-store*)
    ))
 :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
; => ({:banana-cakes 2, :chocolate-cakes 2})













(comment
;; Translated from Strasheela Pattern.oz
;; Possible problem: not tail recursive
(defn map-pairwise 
  "Collects the result of applying the binary function f on all pairwise combinations of xs, i.e. [(f xs1 xs2) .. (f xs1 xsN) (f xs2 xs3) .. (f xsN-1 xsN)]"
  [xs f]
  (if (empty? xs) 
    nil
    (let [xs-rest (rest xs)]
      (core/concat (map #(f (first xs) %) xs-rest)
                   (map-pairwise xs-rest f)))))

  (map-pairwise [1 2] list)
  (map-pairwise [1 2 3 4] list)
  )
