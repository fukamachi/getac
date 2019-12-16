(defsystem "getac"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
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
  :description "Unit testing CLI tool for competitive programming")
