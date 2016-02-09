(in-package :cl-user)
(defpackage :lack-session-store-redis-asd
  (:use :cl :asdf))
(in-package :lack-session-store-redis-asd)

(defsystem lack-session-store-redis
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack-middleware-session
               :cl-redis
               :marshal
               :cl-base64
               :trivial-utf-8)
  :components ((:file "src/middleware/session/store/redis"))
  :in-order-to ((test-op (test-op t-lack-session-store-redis))))
