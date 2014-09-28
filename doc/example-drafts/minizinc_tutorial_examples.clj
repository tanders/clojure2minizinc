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
      :print-mzn? true
      ;; :print-solution? true
      ;; :num-solutions 3
      )))))
;; Outputs nil -- solution printed (arranged as table)


;; simple production planning problem (tutorial p. 18)
(mz/minizinc 
 (mz/clj2mnz
  (let [;; Number of different products
        nproducts (mz/int 'nproducts)
        ;; products (mz/set 'products (mz/-- 1 nproducts))  ;; !! needed?
        products (mz/-- 1 nproducts)
        ;; Profit per unit for each product
        ;; Note: array index-set must be specified explicitly (otherwise no support for translation into list for higher-order programming) 
        profit (mz/array products :int 'profit)
        pname (mz/array products :string 'pname)
        ;; Number of resources
        nresources (mz/int 'nresources)
        ;; resources (mz/set 'resources (mz/-- 1 nresources))  ;; !! needed?
        resources (mz/-- 1 nresources)
        ;; Amount of each resource available
        capacity (mz/array resources :int 'capacity)
        rname (mz/array resources :string 'rname)
        ;; Units of each resource required to produce 1 unit of product
        consumption (mz/array [products resources] :int 'consumption)
        ;; Bound on number of Products
        mproducts (mz/int 'mproducts (mz/max [p products]
                                        (mz/min [r resources
                                                 :where (mz/> (mz/nth consumption p r) 0)]
                                           (mz/div (mz/nth capacity r) (mz/nth consumption p r)))))
        ;; Variables: how much should we make of each product
        produce (mz/array products [:var (mz/-- 0 mproducts)])
        used (mz/array resources [:var (mz/-- 0 (mz/max capacity))])]
    (mz/constraint (mz/assert (mz/forall [r resources
                                          p products]
                                (mz/>= (mz/nth consumption p r) 0)) 
                              "Error: negative consumption"))
    ;; Production cannot use more than the available resources
    (mz/constraint (mz/forall [r resources]
                      (mz/and (mz/= (mz/nth used r)
                                    (mz/sum [p products]
                                       (mz/* (mz/nth consumption p r)
                                             (mz/nth produce p))))
                              (mz/<= (mz/nth used r)
                                     (mz/nth capacity r)))))
    ;; Maximize profit
    (mz/solve :maximize (mz/sum [p products]
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
                         :consumption [[250, 2, 75, 100, 0][200, 0, 150, 150, 75]]
                         ;; :consumption (mz/literal-array [250, 2, 75, 100, 0][200, 0, 150, 150, 75])
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


;; TODO: sudoku, tutorial, p. 27
;; Needs def of MiniZinc if



;; jobshop, tutorial, p. 29
(mz/minizinc 
 (mz/clj2mnz
  ;; every global constraint of the MiniZinc library is automatically included when that global constraint is used
  ;; (mz/include "alldifferent.mzn") 
  (let [jobs (mz/int 'jobs)   ; no of jobs
        tasks (mz/int 'tasks) ; no of tasks per job
        d (mz/array [(mz/-- 1 jobs) (mz/-- 1 tasks)] :int 'd)                ; task duration
        total (mz/int 'total (mz/sum [i (mz/-- 1 jobs)
                                      j (mz/-- 1 tasks)]  
                                (mz/nth d i j)))                             ; total duration
        ;; digs (mz/int 'digs (mz/ceil (mz/log 10.0, (mz/int2float total)))) ; digits for output
        s (mz/array [(mz/-- 1 jobs) (mz/-- 1 tasks)] [:var (mz/-- 0 total)]) ; start times
        end (mz/variable (mz/-- 0 total))]                                   ; total end time
    ;; ensure the tasks occur in sequence
    (mz/constraint (mz/forall [i (mz/-- 1 jobs)]
                      (mz/and (mz/forall [j (mz/-- 1 (mz/- tasks 1))]
                                 (mz/<= (mz/+ (mz/nth s i j) (mz/nth d i j))
                                        (mz/nth s i (mz/+ j 1))))
                              (mz/<= (mz/+ (mz/nth s i tasks) (mz/nth d i tasks))
                                     end))))
    ;; ensure no overlap of tasks
    (mz/constraint (mz/forall [j (mz/-- 1 tasks)]
                      (mz/forall [i (mz/-- 1 jobs)
                                  k (mz/-- 1 jobs)
                                  :where (mz/< i k)]
                         (mz/or (mz/<= (mz/+ (mz/nth s i j) (mz/nth d i j))
                                       (mz/nth s k j))
                                (mz/<= (mz/+ (mz/nth s k j) (mz/nth d k j))
                                       (mz/nth s i j))))))
    (mz/solve :minimize end)
    ;; TODO: revise output -- print array in several rows
    (mz/output-map {:end end :s s})
    ))
 ;; :print-mzn? true
 :data (mz/map2minizinc {:jobs 5
                         :tasks 5
                         :d [[1, 4, 5, 3, 6]
                             [3, 2, 7, 1, 2]
                             [4, 4, 4, 4, 4]
                             [1, 1, 1, 6, 8]
                             [7, 3, 2, 2, 1]]})
 ;; :options ["-f fzn-gecode"] 
 )


;; stable-marriage, p. 31
(mz/minizinc 
 (mz/clj2mnz
  (let [n (mz/int 'n)
        ;; Men (mz/set 'Men (mz/-- 1 n))
        Men (mz/-- 1 n)        
        ;; Women (mz/set 'Women (mz/-- 1 n))
        Women (mz/-- 1 n)
        rankWomen (mz/array [Women Men] :int 'rankWomen)
        rankMen (mz/array [Men Women] :int 'rankMen)
        wife (mz/array Men [:var Women])
        husband (mz/array Women [:var Men])]
    ;; assignment
    (mz/constraint (mz/forall [m Men] (mz/= (mz/nth husband (mz/nth wife m)) m)))
    (mz/constraint (mz/forall [w Women] (mz/= (mz/nth wife (mz/nth husband w)) w)))
    ;; ranking
    (mz/constraint (mz/forall [m Men
                               o Women]
                      (mz/-> (mz/< (mz/nth rankMen m o)
                                   (mz/nth rankMen m (mz/nth wife m)))
                             (mz/< (mz/nth rankWomen o (mz/nth husband o))
                                   (mz/nth rankWomen o m)))))
    (mz/constraint (mz/forall [w Women
                               o Men]
                      (mz/-> (mz/< (mz/nth rankWomen w o)
                                   (mz/nth rankWomen w (mz/nth husband w)))
                             (mz/< (mz/nth rankMen o (mz/nth wife o))
                                   (mz/nth rankMen o w)))))
    (mz/solve :satisfy)
    (mz/output-map {:wifes wife :husbands husband})
    ))
 ;; :print-mzn? true
 :data (mz/map2minizinc {:n 5
                         :rankWomen [[1, 2, 4, 3, 5]
                                     [3, 5, 1, 2, 4]
                                     [5, 4, 2, 1, 3]
                                     [1, 3, 5, 4, 2]
                                     [4, 2, 3, 5, 1]]
                         :rankMen [[5, 1, 2, 4, 3]
                                   [4, 1, 3, 2, 5]
                                   [5, 3, 2, 4, 1] 
                                   [1, 5, 4, 3, 2]
                                   [4, 3, 2, 1, 5]]})
 )
      

;; magic-series, p. 33
(mz/minizinc 
 (mz/clj2mnz
  (let [n (mz/int 'n)
        s (mz/array (mz/-- 0 (mz/- n 1)) [:var (mz/-- 0 n)])]
    (mz/constraint (mz/forall [i (mz/-- 0 (mz/- n 1))]
                      (mz/= (mz/nth s i)
                            (mz/sum [j (mz/-- 0 (mz/- n 1))]
                               (mz/bool2int (mz/= (mz/nth s j) i))))))
    (mz/solve :satisfy)
    (mz/output-map {:s s})))
 :data (mz/map2minizinc {:n 4})
 :options ["--all-solutions"] 
 ;; :print-mzn? true
 ;; :print-cmd? true
 )


;; knapsack, p. 33f
(mz/minizinc 
 (mz/clj2mnz
  (let [n (mz/int 'n)
        ;; Items (mz/set 'Items (mz/-- 1 n))
        Items (mz/-- 1 n)
        capacity (mz/int 'capacity)
        profits (mz/array Items :int 'profits)
        weights (mz/array Items :int 'weights)
        knapsack (mz/variable [:set Items])]
    ;; BUG: translation of sum args into list comprehension syntax causes error, while the corresponding alternative syntax sum(foo)(bar) works fine (tested directly in MiniZinc)
    ;; TODO:
    ;; - Check with MiniZinc 2.0 whether problem persists
    ;; - Create minimal MiniZinc example demonstrating problem with two different calls to sum (both syntax options)
    ;; - Discuss with MiniZinc forum whether this is bug 
    ;; - Likely, in clojure2minizinc I have for now to use the other syntax...
    (mz/constraint (mz/sum [i Items]
                     (mz/<= (mz/* (mz/bool2int (mz/in i knapsack))
                                  (mz/nth weights i))
                            capacity)))
    (mz/solve :maximize (mz/sum [i Items] 
                          (mz/* (mz/bool2int (mz/in i knapsack))
                                (mz/nth profits i))))
    (mz/output-map {:knapsack knapsack})))
  :print-mzn? true
  :data (mz/map2minizinc {:n 20
                          :capacity 50
                          :profits [4,7,3,9,3,8,3,8,2,7,9,2,6,2,8,6,4,8,4,3]
                          :weights [6,3,7,3,7,9,3,5,1,6,2,6,1,4,2,7,3,2,5,1]}))



;; TODO: social-golfers, p. 35f









