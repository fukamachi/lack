(in-package :cl-user)
(defpackage t-lack-middleware-session-asd
  (:use :cl :asdf))
(in-package :t-lack-middleware-session-asd)

(defsystem t-lack-middleware-session
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-middleware-session
               :lack-test
               :cl-cookie
               :prove)
  :components ((:test-file "t/middleware/session"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)))
