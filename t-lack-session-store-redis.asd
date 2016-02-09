(in-package :cl-user)
(defpackage t-lack-session-store-redis-asd
  (:use :cl :asdf))
(in-package :t-lack-session-store-redis-asd)

(defsystem t-lack-session-store-redis
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-test
               :lack-session-store-redis
               :prove)
  :components
  ((:test-file "t/session/store/redis"))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
