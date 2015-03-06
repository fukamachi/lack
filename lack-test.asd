(in-package :cl-user)
(defpackage lack-test-asd
  (:use :cl :asdf))
(in-package :lack-test-asd)

(defsystem lack-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :prove
               :bordeaux-threads
               :usocket)
  :components ((:file "src/test")))
