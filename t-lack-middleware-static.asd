(defsystem "t-lack-middleware-static"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack"
               "lack-test"
               "prove"
               "alexandria")
  :components ((:test-file "t/middleware/static"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)))
