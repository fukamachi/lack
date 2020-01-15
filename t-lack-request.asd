(defsystem "t-lack-request"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack-request"
               "clack-test"
               "hunchentoot"
               "dexador"
               "prove"
               "flexi-streams"
               "alexandria")
  :components
  ((:test-file "t/request"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
