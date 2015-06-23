(in-package :cl-user)
(defpackage lack.middleware.session.store
  (:nicknames :lack.session.store)
  (:use :cl)
  (:export :session-id
           :session-data
           :make-session
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack.middleware.session.store)

(defstruct session
  id
  (data (make-hash-table :test 'equal)))

(defgeneric fetch-session (store sid))
(defgeneric store-session (store session))
(defgeneric remove-session (store session))

(defmethod fetch-session :around ((store t) sid)
  (or (call-next-method)
      (make-session)))

(defmethod fetch-session (store (sid (eql nil)))
  (make-session))

(defmethod remove-session :after ((store t) session)
  (setf (session-id session) nil))
