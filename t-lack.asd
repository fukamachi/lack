(defsystem "t-lack"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack"
               "clack"
               "clack-v1-compat"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "builder"))))

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)))
