(defproject minizinc/clojure2minizinc "0.2.1-SNAPSHOT"
  :description "This library provides an interface to the MiniZinc constraint modelling language (http://www.minizinc.org/), and constraint solvers supporting a FlatZinc interface (e.g., Gecode, http://www.gecode.org/flatzinc.html)."
  :url "http://tanders.github.io/clojure2minizinc/"
  :license {:name "GNU General Public License"
            :url "https://gnu.org/licenses/gpl.html"}
  ;; ?? TODO: add dependencies -- how can Leiningen add dependencies outside Clojure?
  ;; Possible solution: https://github.com/pliant/lein-package
  ;; Difficulties: 
  ;; - The MiniZinc distribution and solvers are platform specific, so I would need an extra project for each platform.
  ;; - The MiniZinc distribution installation requires running some install script locally
  :dependencies [;; [org.clojure/clojure "1.9.0-alpha7"]
                 [org.clojure/clojure "1.8.0"]
                 ;; [org.clojure/clojure "1.6.0"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 ;; https://github.com/clojure-emacs/cider-tracing
                 ;; https://github.com/clojure/tools.trace
                 ;; [org.clojure/tools.trace "0.7.5"]
                 ]
  ;; see https://github.com/weavejester/codox
  :codox {:metadata {:doc "TODO: write docs"
                     :doc/format :markdown}
          :output-path "doc/reference"
          ;; TODO: update URI for soures 
          :source-uri "https://github.com/tanders/clojure2minizinc/tree/master/"
          :src-linenum-anchor-prefix "L"})
