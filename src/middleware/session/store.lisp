(in-package :cl-user)
(defpackage lack.middleware.session.store
  (:nicknames :lack.session.store)
  (:use :cl)
  (:export :store
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack.middleware.session.store)

(defstruct store)

(defgeneric fetch-session (store sid))
(defgeneric store-session (store sid session))
(defgeneric remove-session (store sid))
