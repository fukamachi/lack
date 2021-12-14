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
               :circular-streams
               :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "request" :depends-on ("media-type"))
                 (:file "media-type"))))
  :in-order-to ((test-op (test-op t-lack-request))))

(register-system-packages "lack-request" '(:lack.request))
