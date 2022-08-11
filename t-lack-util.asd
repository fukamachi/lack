(defsystem "t-lack-util"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-test"
               "lack-util"
               "prove")
  :components
  ((:test-file "t/util"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
