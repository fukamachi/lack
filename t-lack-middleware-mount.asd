(in-package :cl-user)
(defpackage t-lack-middleware-mount-asd
  (:use :cl :asdf))
(in-package :t-lack-middleware-mount-asd)

(defsystem t-lack-middleware-mount
  :author "Rudolph Miller"
  :license "LLGPL"
  :depends-on (:lack
               :lack-test
               :lack-component
               :lack-middleware-mount
               :prove)
  :components
  ((:test-file "t/middleware/mount"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
