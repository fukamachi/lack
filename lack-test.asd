(defsystem "lack-test"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack"
               "quri"
               "cl-cookie"
               "flexi-streams")
  :components ((:file "src/test")))
