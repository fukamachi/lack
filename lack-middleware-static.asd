(defsystem "lack-middleware-static"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("trivial-mimes"
               "local-time"
               "uiop"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "middleware/static" :depends-on ("app/file"))
                 (:file "app/file"))))
  :in-order-to ((test-op (test-op "t-lack-middleware-static"))))
