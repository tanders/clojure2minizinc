(defproject clojure2minizinc "0.1.0-SNAPSHOT"
  :description "This library provides an interface to the MiniZinc constraint modelling language (http://www.minizinc.org/), and constraint solvers supporting a FlatZinc interface (e.g., Gecode, http://www.gecode.org/flatzinc.html)."
  ;; :url "http://example.com/FIXME"
  :license {:name "GNU General Public License"
            :url "https://gnu.org/licenses/gpl.html"}
  ;; TODO: add dependencies -- how can Leiningen add dependencies outside Clojure?
  ;; mzn2fzn as part of http://www.minizinc.org/g12distrib.html -- note that this distribution additionally needs to run and install script locally
  ;; Gecode/FlatZinc, see http://www.gecode.org/flatzinc.html
  ;; or other FlatZinc implementations
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [me.raynes/fs "1.4.5"]
                 ])