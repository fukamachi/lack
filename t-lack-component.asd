(defsystem "t-lack-component"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-component"
               "lack-test"
               "prove")
  :components
  ((:test-file "t/component"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
