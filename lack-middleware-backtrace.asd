(defsystem "lack-middleware-backtrace"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("uiop")
  :components ((:file "src/middleware/backtrace")))

(register-system-packages "lack-middleware-backtrace" '(:lack.middleware.backtrace))
