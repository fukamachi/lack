(in-package :cl-user)
(defpackage t-lack-middleware-auth-basic-asd
  (:use :cl :asdf))
(in-package :t-lack-middleware-auth-basic-asd)

(defsystem t-lack-middleware-auth-basic
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-test
               :lack-middleware-auth-basic
               :prove
               :cl-base64
               :alexandria)
  :components
  ((:test-file "t/middleware/auth/basic"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
