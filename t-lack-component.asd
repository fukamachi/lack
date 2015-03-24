(in-package :cl-user)
(defpackage t-lack-component-asd
  (:use :cl :asdf))
(in-package :t-lack-component-asd)

(defsystem t-lack-component
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack-component
               :lack-test
               :prove)
  :components
  ((:test-file "t/component"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
