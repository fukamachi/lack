(in-package :cl-user)
(defpackage :lack-middleware-mount-asd
  (:use :cl :asdf))
(in-package :lack-middleware-mount-asd)

(defsystem lack-middleware-mount
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack-component)
  :components ((:file "src/middleware/mount")))
