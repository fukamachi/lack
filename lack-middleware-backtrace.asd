(defsystem "lack-middleware-backtrace"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("uiop")
  :components ((:file "src/middleware/backtrace"))
  :in-order-to ((test-op (test-op "t-lack-middleware-backtrace"))))
