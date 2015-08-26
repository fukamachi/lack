(in-package :cl-user)
(defpackage :lack-middleware-static-asd
  (:use :cl :asdf))
(in-package :lack-middleware-static-asd)

(defsystem lack-middleware-static
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:trivial-mimes
               :local-time
               :uiop
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "middleware/static" :depends-on ("app/file"))
                 (:file "app/file"))))
  :in-order-to ((test-op (test-op t-lack-middleware-static))))
