(defsystem "t-lack-middleware-mount"
  :author "Rudolph Miller"
  :license "MIT"
  :depends-on ("lack"
               "lack-test"
               "lack-component"
               "lack-middleware-mount"
               "prove")
  :components
  ((:test-file "t/middleware/mount"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
