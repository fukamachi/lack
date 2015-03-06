(in-package :cl-user)
(defpackage :lack-request-asd
  (:use :cl :asdf))
(in-package :lack-request-asd)

(defsystem lack-request
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:quri
               :http-body
               :cl-ppcre)
  :components ((:file "src/request")))
