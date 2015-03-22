(in-package :cl-user)
(defpackage t-lack-middleware-csrf-asd
  (:use :cl :asdf))
(in-package :t-lack-middleware-csrf-asd)

(defsystem t-lack-middleware-csrf
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-test
               :lack-request
               :lack-middleware-csrf
               :prove
               :cl-ppcre)
  :components
  ((:test-file "t/middleware/csrf"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
