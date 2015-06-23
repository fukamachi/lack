(in-package :cl-user)
(defpackage lack.middleware.session.state
  (:nicknames :lack.session.state)
  (:use :cl)
  (:export :state
           :make-state
           :extract-sid
           :expire-state
           :finalize-state))
(in-package :lack.middleware.session.state)

(defstruct state)

(defgeneric extract-sid (state env))
(defgeneric expire-state (state sid res options))
(defgeneric finalize-state (state sid res options))
