(defsystem "t-lack-middleware-backtrace"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack"
               "lack-test"
               "prove"
               "alexandria")
  :components
  ((:test-file "t/middleware/backtrace"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
