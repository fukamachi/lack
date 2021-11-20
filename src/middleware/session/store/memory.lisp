#||
Implement the protocol for storing, fetching, and removing a session from Clacks session 
middleware
||#

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

;;;for SBCL use the synchronized hash table rather than manually grabbing and releasing
;;;a lock on each call.

#+sbcl
(defstruct (memory-store (:include store))
  (stash (make-hash-table :test 'equal :synchronized t)))

#-sbcl
(defstruct (memory-store (:include store))
  (stash (make-hash-table :test 'equal))
  (lock (bordeaux-threads:make-lock "session store lock")))

#+sbcl
(defmethod fetch-session ((store memory-store) sid)
  (gethash sid (memory-store-stash store)))

#-sbcl
(defmethod fetch-session ((store memory-store) sid)
  (bordeaux-threads:with-lock-held ((memory-store-lock store))
    (gethash sid (memory-store-stash store))))

#+sbcl
(defmethod store-session ((store memory-store) sid session)
  (setf (gethash sid (memory-store-stash store)) session))

#-sbcl 
(defmethod store-session ((store memory-store) sid session)
  (bordeaux-threads:with-lock-held ((memory-store-lock store))
    (setf (gethash sid (memory-store-stash store)) session)))

#+sbcl
(defmethod remove-session ((store memory-store) sid)
  (remhash sid (memory-store-stash store)))

#-sbcl
(defmethod remove-session ((store memory-store) sid)
  (bordeaux-threads:with-lock-held ((memory-store-lock store))
    (remhash sid (memory-store-stash store))))
