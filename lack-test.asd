(in-package :cl-user)
(defpackage lack-test-asd
  (:use :cl :asdf))
(in-package :lack-test-asd)

(defsystem lack-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :quri
               :cl-cookie
               :flexi-streams)
  :components ((:file "src/test")))
