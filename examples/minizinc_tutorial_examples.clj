(ns clojure2minizinc.tutorial
  (:require [clojure2minizinc.core :as mz] ; 
            ;; [clojure.java.shell :as shell]
            [clojure.pprint :as pprint])
  )



;; Examples of MiniZinc tutorial, translated to Clojure

;; Colouring Australia using nc colours (tutorial of version 1.6, p. 4)
;; Version as in tutorial, only "clojurized"
(mz/minizinc 
 (mz/clj2mnz
  (let [nc (mz/int 'nc 3)     
        wa (mz/variable (mz/-- 1 nc))
        nt (mz/variable (mz/-- 1 nc))
        sa (mz/variable (mz/-- 1 nc))
        q (mz/variable (mz/-- 1 nc))
        nsw (mz/variable (mz/-- 1 nc))
        v (mz/variable (mz/-- 1 nc))
        t (mz/variable (mz/-- 1 nc))]
    (mz/constraint (mz/!= wa nt))
    (mz/constraint (mz/!= wa sa))
    (mz/constraint (mz/!= nt sa))
    (mz/constraint (mz/!= nt q))
    (mz/constraint (mz/!= sa q))
    (mz/constraint (mz/!= sa nsw))
    (mz/constraint (mz/!= sa v))
    (mz/constraint (mz/!= nsw v))
    (mz/solve :satisfy)
    (mz/output-map {:wa wa :nt nt :sa sa :q q :nsw nsw :v v :t t})
    ))
 ;; :print-mzn? true
 :num-solutions 3
 ;; :all-solutions? true
 )
                                        ; => ({:wa 1, :nt 2, :sa 3, :q 1, :nsw 2, :v 1, :t 1})


; performance testing -- about 3 msecs
(time 
 (mz/clj2mnz
  (let [nc (mz/int 'nc 3)     
        wa (mz/variable (mz/-- 1 nc))
        nt (mz/variable (mz/-- 1 nc))
        sa (mz/variable (mz/-- 1 nc))
        q (mz/variable (mz/-- 1 nc))
        nsw (mz/variable (mz/-- 1 nc))
        v (mz/variable (mz/-- 1 nc))
        t (mz/variable (mz/-- 1 nc))]
    (mz/constraint (mz/!= wa nt))
    (mz/constraint (mz/!= wa sa))
    (mz/constraint (mz/!= nt sa))
    (mz/constraint (mz/!= nt q))
    (mz/constraint (mz/!= sa q))
    (mz/constraint (mz/!= sa nsw))
    (mz/constraint (mz/!= sa v))
    (mz/constraint (mz/!= nsw v))
    (mz/solve :satisfy)
    (mz/output-map {:wa wa :nt nt :sa sa :q q :nsw nsw :v v :t t})
    )))

(time  ;; about 37 msecs
 (mz/minizinc 
 (mz/clj2mnz
  (let [nc (mz/int 'nc 3)     
        wa (mz/variable (mz/-- 1 nc))
        nt (mz/variable (mz/-- 1 nc))
        sa (mz/variable (mz/-- 1 nc))
        q (mz/variable (mz/-- 1 nc))
        nsw (mz/variable (mz/-- 1 nc))
        v (mz/variable (mz/-- 1 nc))
        t (mz/variable (mz/-- 1 nc))]
    (mz/constraint (mz/!= wa nt))
    (mz/constraint (mz/!= wa sa))
    (mz/constraint (mz/!= nt sa))
    (mz/constraint (mz/!= nt q))
    (mz/constraint (mz/!= sa q))
    (mz/constraint (mz/!= sa nsw))
    (mz/constraint (mz/!= sa v))
    (mz/constraint (mz/!= nsw v))
    (mz/solve :satisfy)
    (mz/output-map {:wa wa :nt nt :sa sa :q q :nsw nsw :v v :t t})
    )))

 (time
  (mz/minizinc 
   (mz/clj2mnz
    (let [nc (mz/int 'nc 3)     
          wa (mz/variable (mz/-- 1 nc))
          nt (mz/variable (mz/-- 1 nc))
          sa (mz/variable (mz/-- 1 nc))
          q (mz/variable (mz/-- 1 nc))
          nsw (mz/variable (mz/-- 1 nc))
          v (mz/variable (mz/-- 1 nc))
          t (mz/variable (mz/-- 1 nc))]
      (mz/constraint (mz/!= wa nt))
      (mz/constraint (mz/!= wa sa))
      (mz/constraint (mz/!= nt sa))
      (mz/constraint (mz/!= nt q))
      (mz/constraint (mz/!= sa q))
      (mz/constraint (mz/!= sa nsw))
      (mz/constraint (mz/!= sa v))
      (mz/constraint (mz/!= nsw v))
      (mz/solve :satisfy)
      (mz/output-map {:wa wa :nt nt :sa sa :q q :nsw nsw :v v :t t})
      ))
   ;; :print-solution? true
   :options ["-f fzn-gecode"] ; "-s"
   ))
;; Gecode search statistics - Gecode's time (< 1 ms) takes much less than time required overall (about 3 msecs for clojure2minizinc -> MiniZinc, > 10/15 msecs for MiniZinc -> FlatZinc according to MiniZincIDE total time reported for this problem, and possibly another overhead for reading results back into Clojure.
(comment
  %%  runtime:       0.000 (0.320 ms)
%%  solvetime:     0.000 (0.073 ms)
%%  solutions:     1
%%  variables:     7
%%  propagators:   8
%%  propagations:  8
%%  nodes:         5
%%  failures:      0
%%  restarts:      0
%%  peak depth:    4
  )


;; Colouring Australia using nc colours
;; version using Clojure abstraction means
(mz/minizinc 
 (mz/clj2mnz
  (let [nc (mz/int 'nc 3)
        states (zipmap [:wa :nt :sa :q :nsw :v :t]
                       (take 7 (repeatedly #(mz/variable (mz/-- 1 nc)))))]
    ;; enforce that lazy map is actually computed with doall
    (doall (map (fn [[s1 s2]] 
                  (mz/constraint (mz/!= (s1 states) (s2 states))))
                [[:wa :nt]
                 [:wa :sa]
                 [:nt :sa]
                 [:nt :q]
                 [:sa :q]
                 [:sa :nsw]
                 [:sa :v]
                 [:nsw :v]]))
    (mz/solve :satisfy)
    (mz/output-map states)
    ;; (pprint/pprint *mzn-store*)
    ))
 :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
                                        ; Same result as above (order in map reversed)
                                        ; => ({:t 1, :v 1, :nsw 2, :q 1, :sa 3, :nt 2, :wa 1})



;; Baking cakes for the school fete (tutorial p. 7)
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
    (mz/output-map {:banana-cakes b :chocolate-cakes c})
    ;; (pprint/pprint *mzn-store*)
    ))
 :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
                                        ; => ({:banana-cakes 2, :chocolate-cakes 2})




;; Baking cakes for the school fete -- version with datafile (tutorial p. 9)
(mz/minizinc 
 (mz/clj2mnz
  (let [flour (mz/int :flour)
        banana (mz/int :banana)
        sugar (mz/int :sugar)
        butter (mz/int :butter)
        cocoa (mz/int :cocoa)]
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
      (mz/output-map {:banana-cakes b :chocolate-cakes c})
      ;; (pprint/pprint *mzn-store*)
      )))
 :data (mz/map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500})
 ;; :data (mz/map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500})
 ;; :print-mzn? true
 ;; :num-solutions 3
 ;; :all-solutions? true
 )
;; For :data (map2minizinc {:flour 4000 :banana 6 :sugar 2000 :butter 500 :cocoa 500})
                                        ; => ({:banana-cakes 2, :chocolate-cakes 2})


;; Float domain constraints
;; Model for determining relationships between a 1 year loan repaying every quarter, p. 11
(mz/minizinc 
 (mz/clj2mnz
  (let [r (mz/variable :float 'r) ; quarterly repayment
        p (mz/variable :float 'p) ; principal initially borrowed
        i (mz/variable (mz/-- 0.0 10.0) 'i) ; interest rate
        ;; intermediate variables 
        b1 (mz/variable :float 'b1) ; balance after one quarter
        b2 (mz/variable :float 'b2) ; balance after two quarters
        b3 (mz/variable :float 'b3) ; balance after three quarters
        b4 (mz/variable :float 'b4)] ; balance owing at end
    (mz/constraint (mz/= b1 (mz/- (mz/* p (mz/+ 1.0 i)) r)))
    (mz/constraint (mz/= b2 (mz/- (mz/* b1 (mz/+ 1.0 i)) r)))
    (mz/constraint (mz/= b3 (mz/- (mz/* b2 (mz/+ 1.0 i)) r)))
    (mz/constraint (mz/= b4 (mz/- (mz/* b3 (mz/+ 1.0 i)) r)))
    (mz/solve :satisfy)
    ;; TODO: revise m/output-map -- no parentheses. What about parentheses around expressions at values?
    (mz/output-map {:borrowing p :interest-rate (mz/* i 100.0)
                    :repayment-per-quarter r
                    :owing-at-end b4})))
 :print-mzn? true
 ;; :data (mz/map2minizinc {:i 0.04 :p 1000.0 :r 260.0})
 ;; :data (mz/map2minizinc {:i 0.04 :p 1000.0 :b4 0.0})
 :data (mz/map2minizinc {:i 0.04 :r 250.0 :b4 0.0})
 :solver "mzn-g12mip")


;; TODO: def some mz/output-array
;; Example laplace, Tutorial p. 15f 
;; print-table copied from http://clojure.github.io/clojure/clojure.pprint-api.html
(defn print-table [column-width aseq]
  (binding [*out* (pprint/get-pretty-writer *out*)]
    (doseq [row aseq]
      (doseq [col row]
        (pprint/cl-format true "~6,2F~7,vT" col column-width))
        ;; (pprint/cl-format true "~4D~7,vT" col column-width))
      (prn))))
;; (print-table 1 (map #(vector % (* % %) (* % % %) %) (range 1 11)))
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
         ;; Orig output statement -- use with option :print-solution?
         ;; (mz/output "show_float(6, 2, t[i,j]) ++
         ;; if j == h then \"\\n\" else \" \" endif |
         ;; i in 0..w, j in 0..h")
         (mz/output-var t) ;; 2d-array output as flat 1d array -- formatting of result done by Clojure
         ))
      :options ["-f fzn-gecode"]
      ;; :solver "mzn-g12mip"
      ;; :print-mzn? true
      ;; :print-solution? true
      ;; :num-solutions 3
      )))))
;; Outputs nil -- solution printed (arranged as table)


;; simple production planning problem (tutorial p. 18)
;; TODO: abstraction/avoid repetition: save (mz/-- 1 nresources) and friends in strings with names like `resources`?
(mz/minizinc 
 (mz/clj2mnz
  (let [;; Number of different products
        nproducts (mz/int 'nproducts)
        products (mz/set 'products (mz/-- 1 nproducts))  ;; !! needed?
        ;; Profit per unit for each product
        ;; Note: array index-set must be specified explicitly (otherwise no support for translation into list for higher-order programming) 
        profit (mz/array (mz/-- 1 nproducts) :int 'profit)
        pname (mz/array (mz/-- 1 nproducts) :string 'pname)
        ;; Number of resources
        nresources (mz/int 'nresources)
        resources (mz/set 'resources (mz/-- 1 nresources))  ;; !! needed?
        ;; Amount of each resource available
        capacity (mz/array (mz/-- 1 nresources) :int 'capacity)
        rname (mz/array (mz/-- 1 nresources) :string 'rname)
        ;; Units of each resource required to produce 1 unit of product
        consumption (mz/array (list (mz/-- 1 nproducts) (mz/-- 1 nresources)) :int 'consumption)
        ;; Bound on number of Products
        mproducts (mz/int 'mproducts (mz/max [p (mz/-- 1 nproducts)]
                                        (mz/min [r (mz/-- 1 nresources)
                                                 :where (mz/> (mz/nth consumption p r) 0)]
                                           (mz/div (mz/nth capacity r) (mz/nth consumption p r)))))
        ;; Variables: how much should we make of each product
        produce (mz/array (mz/-- 1 nproducts) [:var (mz/-- 0 mproducts)])
        used (mz/array (mz/-- 1 nresources) [:var (mz/-- 0 (mz/max capacity))])]
    (mz/constraint (mz/assert (mz/forall [r (mz/-- 1 nresources)
                                          p (mz/-- 1 nproducts)]
                                (mz/>= (mz/nth consumption p r) 0)) 
                              "Error: negative consumption"))
    ;; Production cannot use more than the available resources
    (mz/constraint (mz/forall [r (mz/-- 1 nresources)]
                      (mz/and (mz/= (mz/nth used r)
                                    (mz/sum [p (mz/-- 1 nproducts)]
                                       (mz/* (mz/nth consumption p r)
                                             (mz/nth produce p))))
                              (mz/<= (mz/nth used r)
                                     (mz/nth capacity r)))))
    ;; Maximize profit
    (mz/solve :maximize (mz/sum [p (mz/-- 1 nproducts)]
                           (mz/* (mz/nth profit p)
                                 (mz/nth produce p))))
    ;; TODO: array comprehension within output -- make fn output more flexible
    ;; TODO: make printed result more clear (possibly with Clojure instead of MiniZinc coding)
    (mz/output-map {:produce produce :used used})))
 :data (mz/map2minizinc {:nproducts 2 ; banana cakes and chocolate cakes
                         :profit [400, 450] ; in cents
                         ;; MiniZinc strings must be explicitly declared (they are indistuigishable from other MiniZinc code otherwise, because such code is stored internally in Clojure strings)
                         :pname (map mz/string ["banana-cake", "chocolate-cake"]) 
                         :nresources 5 ; flour, banana, sugar, butter, cocoa
                         :capacity [4000, 6, 2000, 500, 500]
                         :rname (map mz/string ["flour","banana","sugar","butter","cocoa"])
                         :consumption (mz/literal-array [250, 2, 75, 100, 0][200, 0, 150, 150, 75])
                         }))


;; send more money, tutorial, p. 25
(mz/minizinc 
 (mz/clj2mnz
  ;; every global constraint of the MiniZinc library is automatically included when that global constraint is used
  ;; (mz/include "alldifferent.mzn") 
  (let [s (mz/variable (mz/-- 1 9))
        e (mz/variable (mz/-- 0 9))
        n (mz/variable (mz/-- 0 9))
        d (mz/variable (mz/-- 0 9))
        m (mz/variable (mz/-- 1 9))
        o (mz/variable (mz/-- 0 9))
        r (mz/variable (mz/-- 0 9))
        y (mz/variable (mz/-- 0 9))]
    (mz/constraint (mz/= (mz/+ (mz/* 1000 s) (mz/* 100 e) (mz/* 10 n) d
                               (mz/* 1000 m) (mz/* 100 o) (mz/* 10 r) e)
                         (mz/+ (mz/* 10000 m) (mz/* 1000 o) (mz/* 100 n) (mz/* 10 e) y)))
    (mz/constraint (mz/alldifferent [s e n d m o r y]))
    (mz/solve :satisfy)
    ;; TODO: revise output, see MiniZinc tutorial (or use Clojure capabilities)
    (mz/output-map {:s s :e e :n n :d d :m m :o o :r r :y y})
    ))
 :print-mzn? true)


