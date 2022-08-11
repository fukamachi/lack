(defsystem "t-lack-session-store-dbi"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack"
               "lack-test"
               "lack-session-store-dbi"
               "prove"
               "dbi"
               "sqlite")
  :components
  ((:test-file "t/session/store/dbi"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
