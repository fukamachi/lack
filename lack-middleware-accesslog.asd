(defsystem "lack-middleware-accesslog"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack-util"
               "local-time")
  :components ((:module "src"
                :components
                ((:file "middleware/accesslog"))))
  :in-order-to ((test-op (test-op "t-lack-middleware-accesslog"))))
