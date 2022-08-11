(defsystem "lack-response"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("quri"
               "local-time")
  :components ((:file "src/response")))

(register-system-packages "lack-response" '(:lack.response))
