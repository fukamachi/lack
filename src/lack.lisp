(in-package :cl-user)
(defpackage lack
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:export :builder))
(in-package :lack)
