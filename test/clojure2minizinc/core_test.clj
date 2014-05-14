(ns clojure2minizinc.core-test
  (:require [clojure.test :refer :all]
            [clojure2minizinc.core :refer :all]
            [clojure.java.shell :as shell]))


;; Doc for clojure.test: http://clojure.github.io/clojure/clojure.test-api.html

(comment
  
  ;; test outdated
  (deftest variable-declarations
    (testing "Integer decision variables."
      (is (= (variable (domain 1 3) 'x) "var 1..3: x;"))))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Playing around
;;;

;;; Testing to call external programs
(comment
  (println (shell/sh "pwd"))
  (println (shell/sh "ls" "-l"))  
  (println (shell/sh "ls" "-l" "/no-such-thing"))  
  )


;; Execute a (preexisting) MiniZinc program in a shell and read the result as a clojure datatype 
(comment
  ;; does not work if not in dir of minizinc program
  (shell/sh *fd-solver*
            ;; TMP: local dir 
"/Users/torsten/Clojure/Clojure2MiniZinc/clojure2minizinc/resources/minizinc-examples/candles-editForParsableOutput.mzn")

  ;; OK
  (read-string
   (:out (shell/sh *fd-solver* "candles-editForParsableOutput.mzn" 
                   :dir "/Users/torsten/Clojure/Clojure2MiniZinc/clojure2minizinc/resources/minizinc-examples")))
  ;; => {:vars [7 6 5 4 3 2 1], :twoyearsago 14, :now 28}
  )

;; simple reading and writing of files
(comment
  (spit "/tmp/test.txt" "Line to be written
This is another line
")

  (let [file (doto (java.io.File/createTempFile "clojure2minizinc" ".mzn") .deleteOnExit)]
    (println file)
    (spit file "Line to be written"))

  (slurp "/tmp/test.txt")

  
  )



(comment 
  (minizinc ))
