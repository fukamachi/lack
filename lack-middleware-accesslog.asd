(in-package :cl-user)
(defpackage :lack-middleware-accesslog-asd
  (:use :cl :asdf))
(in-package :lack-middleware-accesslog-asd)

(defsystem lack-middleware-accesslog
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack-util
               :local-time)
  :components ((:module "src"
                :components
                ((:file "middleware/accesslog"))))
  :in-order-to ((test-op (test-op t-lack-middleware-accesslog))))
