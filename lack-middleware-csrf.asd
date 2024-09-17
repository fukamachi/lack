(defsystem "lack-middleware-csrf"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-request"
               "lack-util")
  :components ((:module "src"
                :components
                ((:file "middleware/csrf")))))

(register-system-packages "lack-middleware-csrf" '(:lack.middleware.csrf))
