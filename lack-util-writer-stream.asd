(in-package :cl-user)
(defpackage :lack-util-writer-stream-asd
  (:use :cl :asdf))
(in-package :lack-util-writer-stream-asd)

(defsystem lack-util-writer-stream
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:trivial-gray-streams
               :babel)
  :components ((:file "src/util/writer-stream")))
