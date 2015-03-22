(in-package :cl-user)
(defpackage t-lack-middleware-accesslog-asd
  (:use :cl :asdf))
(in-package :t-lack-middleware-accesslog-asd)

(defsystem t-lack-middleware-accesslog
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-test
               :prove
               :split-sequence)
  :components
  ((:test-file "t/middleware/accesslog"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
