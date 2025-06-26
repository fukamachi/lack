(defsystem "lack-middleware-when"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-util")
  :components ((:file "src/middleware/when")))

(register-system-packages "lack-middleware-when" '(:lack.middleware.when))
