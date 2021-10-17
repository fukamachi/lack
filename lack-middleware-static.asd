(defsystem "lack-middleware-static"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack-app-file"
               "lack-component"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "middleware/static"))))
  :in-order-to ((test-op (test-op "t-lack-middleware-static"))))

(register-system-packages "lack-middleware-static" '(:lack.middleware.static))
