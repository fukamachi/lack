(in-package :cl-user)
(defpackage :lack-handler-hunchentoot-asd
  (:use :cl :asdf))
(in-package :lack-handler-hunchentoot-asd)

(defsystem lack-handler-hunchentoot
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:hunchentoot
               :flexi-streams
               :alexandria
               :split-sequence)
  :components ((:file "src/handler/hunchentoot"))
  :description "Lack handler for Hunchentoot.")
