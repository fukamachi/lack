(in-package :cl-user)
(defpackage :lack-middleware-session-asd
  (:use :cl :asdf))
(in-package :lack-middleware-session-asd)

(defsystem lack-middleware-session
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack-request
               :lack-response
               :lack-util
               :cl-ppcre)
  :components ((:module "src/middleware"
                :components
                ((:file "session" :depends-on ("store" "state"))
                 (:module "store"
                  :pathname "session"
                  :components
                  ((:file "store")
                   (:file "store/memory")))
                 (:module "state"
                  :pathname "session"
                  :components
                  ((:file "state")
                   (:file "state/cookie"))))))
  :in-order-to ((test-op (test-op t-lack-middleware-session))))
