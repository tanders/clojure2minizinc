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
 :print-mzn? true
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




;; Baking cakes for the school fete -- version with datafile (tutorial p. 9)
(m/minizinc 
 (m/clj2mnz
  (let [flour (m/int :flour)
        banana (m/int :banana)
        sugar (m/int :sugar)
        butter (m/int :butter)
        cocoa (m/int :cocoa)]
    (m/constraint (m/assert (m/>= flour 0) "Amount of flour must not be negative"))
    (m/constraint (m/assert (m/>= banana 0) "Amount of banana must not be negative"))
    (m/constraint (m/assert (m/>= sugar 0) "Amount of sugar must not be negative"))
    (m/constraint (m/assert (m/>= butter 0) "Amount of butter must not be negative"))
    (m/constraint (m/assert (m/>= cocoa 0) "Amount of cocoa must not be negative"))
    (let [b (m/variable (m/-- 1 100))
          c (m/variable (m/-- 1 100))]
      ;; flour
      (m/constraint (m/<= (m/+ (m/* 250 b)
                               (m/* 200 c))
                          flour))
      ;; bananas
      (m/constraint (m/<= (m/* 2 b) banana))
      ;; sugar
      (m/constraint (m/<= (m/+ (m/* 75 b)
                               (m/* 150 c))
                          sugar))
      ;; butter 
      (m/constraint (m/<= (m/+ (m/* 100 b)
                               (m/* 150 c))
                          butter))
      ;; cocoa
      (m/constraint (m/<= (m/* 75 c) cocoa))
      ;; maximise profit
      (m/solve :maximize (m/+ (m/* 400 b) (m/* 450 c)))
      (m/output-map {:banana-cakes b :chocolate-cakes c})
      ;; (pprint/pprint *mzn-store*)
      )))
 ;; :data (map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500})
 :data (m/map2minizinc {:flour -8000 :banana 11 :sugar 3000 :butter 1500 :cocoa 800})
 ;; :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
;; For :data (map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500})
; => ({:banana-cakes 2, :chocolate-cakes 2})
;; For :data (map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500})
; => ({:banana-cakes 3, :chocolate-cakes 8})


;; Float domain constraints
;; Model for determining relationships between a 1 year loan repaying every quarter, p. 11
(m/minizinc 
 (m/clj2mnz
  (let [r (m/variable :float 'r) ; quarterly repayment
        p (m/variable :float 'p) ; principal initially borrowed
        i (m/variable (m/-- 0.0 10.0) 'i) ; interest rate
        ;; intermediate variables 
        b1 (m/variable :float 'b1) ; balance after one quarter
        b2 (m/variable :float 'b2) ; balance after two quarters
        b3 (m/variable :float 'b3) ; balance after three quarters
        b4 (m/variable :float 'b4)] ; balance owing at end
    (m/constraint (m/= b1 (m/- (m/* p (m/+ 1.0 i)) r)))
    (m/constraint (m/= b2 (m/- (m/* b1 (m/+ 1.0 i)) r)))
    (m/constraint (m/= b3 (m/- (m/* b2 (m/+ 1.0 i)) r)))
    (m/constraint (m/= b4 (m/- (m/* b3 (m/+ 1.0 i)) r)))
    (m/solve :satisfy)
    ;; TODO: revise m/output-map -- no parentheses. What about parentheses around expressions at values?
    (m/output-map {:borrowing p :interest-rate (m/* i 100.0)
                   :repayment-per-quarter r
                   :owing-at-end b4})))
 :print-mzn? true
 ;; :data (m/map2minizinc {:i 0.04 :p 1000.0 :r 260.0})
 ;; :data (m/map2minizinc {:i 0.04 :p 1000.0 :b4 0.0})
 :data (m/map2minizinc {:i 0.04 :r 250.0 :b4 0.0})
 :solver "mzn-g12mip")



;; TODO: finish def
;; TODO: def some m/output-array
;; laplace, p. 15f 
(m/minizinc 
 (m/clj2mnz
  (let [w (m/int 'w 4)
        h (m/int 'h 4)
        ;; array decl
        t (m/array (list (m/-- 0 w) (m/-- 0 h)) :float 't)
        left (m/variable :float 'left)
        right (m/variable :float 'right)
        top (m/variable :float 'top)
        bottom (m/variable :float 'bottom)]
    ;; Laplace equation
    ;; Each internal temp. is average of its neighbours
    (m/constraint 
     (m/forall [i (m/-- 1 (m/- w 1))
                j (m/-- 1 (m/- h 1))]
               (m/= (m/* 4.0 (m/nth t i j))
                    ;; TODO: generalise binary operators like +, -, *, / 
                    ;; to support an arbitrary number of args 
                    (m/+ (m/nth t (m/- i 1) j)
                         (m/+ (m/nth t i (m/- j 1))
                              (m/+ (m/nth t (m/+ i 1) j)
                                   (m/nth t i (m/+ j 1))))))))
    ;; edge constraints
    (m/constraint (m/forall [i (m/-- 1 (m/- w 1))]
                            (m/= (m/nth t i 0) left)))
    (m/constraint (m/forall [i (m/-- 1 (m/- w 1))]
                            (m/= (m/nth t i h) right)))
    (m/constraint (m/forall [j (m/-- 1 (m/- h 1))]
                            (m/= (m/nth t 0 j) top)))
    (m/constraint (m/forall [j (m/-- 1 (m/- h 1))]
                            (m/= (m/nth t w j) bottom)))
    ;; corner constraints
    (m/constraint (m/= (m/nth t 0 0) 0.0))
    (m/constraint (m/= (m/nth t 0 h) 0.0))
    (m/constraint (m/= (m/nth t w 0) 0.0))
    (m/constraint (m/= (m/nth t w h) 0.0))
    (m/constraint (m/= left 0.0))
    (m/constraint (m/= right 0.0))
    (m/constraint (m/= top 100.0))
    (m/constraint (m/= bottom 0.0))
    (m/solve :satisfy)
    ;; TODO: revise with a better version of m/output
    (m/output "show_float(6, 2, t[i,j]) ++
if j == h then \"\\n\" else \" \" endif |
i in 0..w, j in 0..h")
;; (m/output "show(t[i,j]) ++
;; if j == h then \" | \" else \" \" endif |
;; i in 0..w, j in 0..h")
;; ;; next version not working (tried to output nested Clojure vectors)
;; (m/output "\"[\" show(t[i,j]) ++
;; if j == h then \"][\" else \" \" endif |
;; i in 0..w, j in 0..h \"]\"")
;;     (m/output "show_float(6, 2, t[i,j]) |
;; i in 0..w, j in 0..h")
    ;; (m/output "show(t[0,j]) ++ \" \" | j in 0..h")
    ;; (m/output "show(t)")
    ;; (m/output-var t)
    ))
 :solver "mzn-g12mip"
 :print-mzn? true
 :print-solution? true
 ;; :num-solutions 3
)

