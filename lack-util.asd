(in-package :cl-user)
(defpackage :lack-util-asd
  (:use :cl :asdf))
(in-package :lack-util-asd)

(defsystem lack-util
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ((:feature (:or :mswindows :win32 :cormanlisp) "ironclad")
               (:feature (:not (:or :mswindows :win32 :cormanlisp)) "cl-isaac"))
  :components ((:file "src/util"))
  :in-order-to ((test-op (test-op t-lack-util))))

(register-system-packages "lack-util" '(:lack.util))
