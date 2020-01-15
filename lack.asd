(defsystem "lack"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack-component"
               "lack-util")
  :components ((:module "src"
                :components
                ((:file "lack" :depends-on ("builder"))
                 (:file "builder"))))
  :description "A minimal Clack"
  :in-order-to ((test-op (test-op "t-lack"))))
