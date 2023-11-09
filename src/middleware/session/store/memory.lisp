(defpackage lack/middleware/session/store/memory
  (:nicknames :lack.middleware.session.store.memory
              :lack.session.store.memory
              :lack/session/store/memory)
  (:use :cl
        :lack/middleware/session/store)
  (:export :memory-store
           :make-memory-store
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack/middleware/session/store/memory)

(defstruct (memory-store (:include store))
  (stash (make-hash-table :test 'equal))
  (lock (bordeaux-threads-2:make-lock :name "session store lock")))

(defmethod fetch-session ((store memory-store) sid)
  (bordeaux-threads-2:with-lock-held ((memory-store-lock store))
      (gethash sid (memory-store-stash store))))

(defmethod store-session ((store memory-store) sid session)
  (bordeaux-threads-2:with-lock-held ((memory-store-lock store))
    (setf (gethash sid (memory-store-stash store))
          session)))

(defmethod remove-session ((store memory-store) sid)
  (bordeaux-threads-2:with-lock-held ((memory-store-lock store))
    (remhash sid (memory-store-stash store))))
