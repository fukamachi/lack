(defsystem "lack-middleware-static"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-app-file"
               "lack-component"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "middleware/static")))))

(register-system-packages "lack-middleware-static" '(:lack.middleware.static))
