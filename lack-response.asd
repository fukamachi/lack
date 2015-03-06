(in-package :cl-user)
(defpackage :lack-response-asd
  (:use :cl :asdf))
(in-package :lack-response-asd)

(defsystem lack-response
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:quri
               :local-time)
  :components ((:file "src/response")))
