(defsystem "t-lack"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack"
               "clack"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "builder"))))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
