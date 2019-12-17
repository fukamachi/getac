(defsystem "getac"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("trivial-gray-streams")
  :components ((:module "src"
                :depends-on ("utils")
                :components
                ((:file "main" :depends-on ("others"))
                 (:module "others"
                  :pathname ""
                  :components
                  ((:file "runner" :depends-on ("timer"))
                   (:file "testcase")
                   (:file "reporter" :depends-on ("runner" "diff" "indent-stream"))
                   (:file "timer")
                   (:file "cli")
                   (:file "diff")
                   (:file "indent-stream")))))
               (:module "utils"
                :pathname "src"
                :components ((:file "utils"))))
  :description "Unit testing CLI tool for competitive programming")
