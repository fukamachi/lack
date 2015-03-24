(in-package :cl-user)
(defpackage t-lack-middleware-backtrace-asd
  (:use :cl :asdf))
(in-package :t-lack-middleware-backtrace-asd)

(defsystem t-lack-middleware-backtrace
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-test
               :prove
               :alexandria)
  :components
  ((:test-file "t/middleware/backtrace"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
