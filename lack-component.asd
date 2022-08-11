(defsystem "lack-component"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :components ((:file "src/component"))
  :in-order-to ((test-op (test-op "t-lack-component"))))

(register-system-packages "lack-component" '(:lack.component))
