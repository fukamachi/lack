(defsystem "lack-middleware-accesslog"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-util"
               "local-time")
  :components ((:module "src"
                :components
                ((:file "middleware/accesslog")))))

(register-system-packages "lack-middleware-accesslog" '(:lack.middleware.accesslog))
