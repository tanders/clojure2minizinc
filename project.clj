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
  :dependencies [;; Version with clojure.spec
                 ;; https://clojure.org/guides/spec
                 ;; http://clojure.github.io/clojure/branch-master/clojure.spec-api.html
                 [org.clojure/clojure "1.9.0-alpha10"]
                 ;; A backport of clojure.spec for Clojure 1.8.
                 ;; https://github.com/tonsky/clojure-future-spec
                 ;; [clojure-future-spec "1.9.0-alpha10"]
                 ;; [org.clojure/clojure "1.8.0"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 ;; https://github.com/clojure-emacs/cider-tracing
                 ;; https://github.com/clojure/tools.trace
                 ;; [org.clojure/tools.trace "0.7.5"]
                 ]
  ;; For spec generation -- https://github.com/clojure/test.check
  ;; dev profile dependencies are included during testing but not published as a dependency or included in uber jars
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  ;; see https://github.com/weavejester/codox
  :codox {:metadata {:doc "TODO: write docs"
                     :doc/format :markdown}
          :output-path "doc/reference"
          ;; TODO: update URI for soures 
          :source-uri "https://github.com/tanders/clojure2minizinc/tree/master/"
          :src-linenum-anchor-prefix "L"}
  ;; specify in which ns to start by default 
  :repl-options {:init-ns clojure2minizinc.core}
  )
