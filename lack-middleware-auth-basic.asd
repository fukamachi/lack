(in-package :cl-user)
(defpackage :lack-middleware-auth-basic-asd
  (:use :cl :asdf))
(in-package :lack-middleware-auth-basic-asd)

(defsystem lack-middleware-auth-basic
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-base64
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "middleware/auth/basic"))))
  :in-order-to ((test-op (test-op t-lack-middleware-auth-basic))))
