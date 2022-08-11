(defsystem "t-lack-request"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-request"
               "clack-test"
               "hunchentoot"
               "dexador"
               "prove"
               "flexi-streams"
               "alexandria")
  :components
  ((:test-file "t/request")
   (:test-file "t/media-type"))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
