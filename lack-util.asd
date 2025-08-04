(defsystem "lack-util"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("ironclad/core")
  :components ((:file "src/util")))

(register-system-packages "lack-util" '(:lack.util))
