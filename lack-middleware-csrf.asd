(in-package :cl-user)
(defpackage :lack-middleware-csrf-asd
  (:use :cl :asdf))
(in-package :lack-middleware-csrf-asd)

(defsystem lack-middleware-csrf
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack-request
               :lack-util)
  :components ((:module "src"
                :components
                ((:file "middleware/csrf"))))
  :in-order-to ((test-op (test-op t-lack-middleware-csrf))))
