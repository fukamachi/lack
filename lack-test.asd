(defsystem "lack-test"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack"
               "quri"
               "cl-cookie"
               "flexi-streams")
  :components ((:file "src/test")))

(register-system-packages "lack-test" '(:lack.test))
