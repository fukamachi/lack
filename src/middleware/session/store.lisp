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

(defgeneric fetch-session (store sid)
  (:documentation "Grab a session from STORE using SID."))

(defgeneric store-session (store sid session)
  (:documentation "Store a SESSION in STORE using SID."))

(defgeneric remove-session (store sid)
  (:documentation "Remove a session from STORE using SID."))
