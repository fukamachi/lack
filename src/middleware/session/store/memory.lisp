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

(defstruct (memory-store (:include store))
  #+sbcl (stash (make-hash-table :test 'equal :synchronized t))
  #-sbcl (stash (make-hash-table :test 'equal))
  #-sbcl (lock (bt:make-lock "Lock for session store")))

(defmethod fetch-session ((store memory-store) sid)
  #+sbcl (gethash sid (memory-store-stash store))
  #-sbcl (bt:with-lock-held ((memory-store-lock store))
           (gethash sid (memory-store-stash store))))

(defmethod store-session ((store memory-store) sid session)
  #+sbcl (setf (gethash sid (memory-store-stash store)) session)
  #-sbcl (bt:with-lock-held ((memory-store-lock store))
           (setf (gethash sid (memory-store-stash store)) session)))

(defmethod remove-session ((store memory-store) sid)
  #+sbcl (remhash sid (memory-store-stash store))
  #-sbcl (bt:with-lock-held ((memory-store-lock store))
           (remhash sid (memory-store-stash store))))
