(defsystem "get-accepted"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("rove")
  :components ((:module "src"
                :depends-on ("utils")
                :components
                ((:file "main" :depends-on ("others"))
                 (:module "others"
                  :pathname ""
                  :components
                  ((:file "runner")
                   (:file "testcase")
                   (:file "reporter" :depends-on ("runner"))))))
               (:module "utils"
                :pathname "src"
                :components ((:file "utils"))))
  :description "Unit testing CLI tool for competitive programming"
  :in-order-to ((test-op (test-op "get-accepted/tests"))))
