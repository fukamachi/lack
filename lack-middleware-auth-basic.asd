(defsystem "lack-middleware-auth-basic"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("cl-base64"
               "split-sequence")
  :components ((:module "src"
                :components
                ((:file "middleware/auth/basic")))))

(register-system-packages "lack-middleware-auth-basic" '(:lack.middleware.auth.basic))
