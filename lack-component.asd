(in-package :cl-user)
(defpackage :lack-component-asd
  (:use :cl :asdf))
(in-package :lack-component-asd)

(defsystem lack-component
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :components ((:file "src/component"))
  :in-order-to ((test-op (test-op t-lack-component))))
