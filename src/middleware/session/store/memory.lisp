(in-package :cl-user)
(defpackage lack.middleware.session.store.memory
  (:nicknames :lack.session.store.memory)
  (:use :cl
        :lack.middleware.session.store)
  (:export :memory-store
           :make-memory-store
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack.middleware.session.store.memory)

(defstruct (memory-store (:include store))
  (stash (make-hash-table :test 'equal)))

(defmethod fetch-session ((store memory-store) sid)
  (gethash sid (memory-store-stash store)))

(defmethod store-session ((store memory-store) sid session)
  (setf (gethash sid (memory-store-stash store))
        session))

(defmethod remove-session ((store memory-store) sid)
  (remhash sid (memory-store-stash store)))
