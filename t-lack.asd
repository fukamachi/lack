#|
  This file is a part of lack project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage t-lack-asd
  (:use :cl :asdf))
(in-package :t-lack-asd)

(defsystem t-lack
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :clack
               :clack-v1-compat
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "builder"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
