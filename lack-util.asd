(in-package :cl-user)
(defpackage :lack-util-asd
  (:use :cl :asdf))
(in-package :lack-util-asd)

(defsystem lack-util
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:ironclad)
  :components ((:file "src/util"))
  :in-order-to ((test-op (test-op t-lack-util))))
