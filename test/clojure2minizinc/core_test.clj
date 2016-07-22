(ns clojure2minizinc.core-test
  ;; Doc for clojure.test: http://clojure.github.io/clojure/clojure.test-api.html
  (:require [clojure.core :as core]
            [clojure.test :refer :all]
            [clojure2minizinc.core :refer :all]
            [clojure.java.shell :as shell]
            [clojure.spec :as spec]))





(comment 
;; BUG: function names outdated, I changed _int into int in its namespace, e.g., mz/int

(deftest parameter-declarations
  (testing "Integer"
    (is (= (:mzn-string (_int 1 'x)) "int: x = 1;"))
    (comment
      ;; gensym-ed param names not tested -- could perhaps do that with pattern matching later
      (is (= (:mzn-string (_int 1)) "int: <> = 1;"))
      (is (= (:mzn-string (_int)) "int: <>;"))
      )
    )
  (testing "Float"
    (is (= (:mzn-string (_float 1.0 'x)) "float: x = 1.0;"))
    (comment
      ;; gensym-ed param names not tested
      (is (= (:mzn-string (_float 1.0)) "float: <> = 1;"))
      (is (= (:mzn-string (_float)) "float: <>;"))
      )  
    )
  (testing "Bool"
    (is (= (:mzn-string (_bool 'true 'x)) "bool: x = true;"))
    (comment
      ;; gensym-ed param names not tested
      (is (= (:mzn-string (_bool 'true)) "bool: <> = true;"))
      (is (= (:mzn-string (_bool)) "bool: <>;"))
      )  
    )
  (testing "Set of int"
    (is (= (:mzn-string (_set-of-int (_dom 1 'max) 'MySet)) "set of int: MySet = 1..max;"))
    )
  (comment
    ;; causes exception
    (parameter :foo 1.0 'x)
    )
  )


(deftest variable-declarations
  (testing "Integer decision variables."
    (is (= (:mzn-string (_var (_dom 1 3) 'x)) "var 1..3: x;"))))


)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TMP: Playing around
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TMP: Testing clojure.spec
;;;

(spec/conform even? 1001)

(spec/valid? even? 10)

(spec/valid? #(core/> % 5) 10)

(spec/valid? #(core/> % 5) 0)



(spec/valid? #{:club :diamond :heart :spade} :club)
(spec/valid? #{:club :diamond :heart :spade} 42)

(spec/def ::suit #{:club :diamond :heart :spade})

(spec/valid? ::suit :club)


(spec/explain-data ::suit 42)






(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(spec/def ::email-type (spec/and core/string? #(re-matches email-regex %)))

(spec/def ::acctid core/int?)
(spec/def ::first-name core/string?)
(spec/def ::last-name core/string?)
(spec/def ::email ::email-type)

(spec/def ::person (spec/keys :req [::first-name ::last-name ::email]
                              :opt [::phone]))

(spec/valid? ::email-type "test@test.com")

(spec/valid? ::person {::first-name "Elon"
                       ::last-name "Musk"
                       ::email "elon@example.com"})

