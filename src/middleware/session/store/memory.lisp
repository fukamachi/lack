(in-package :cl-user)
(defpackage lack.middleware.session.store.memory
  (:nicknames :lack.session.store.memory)
  (:use :cl
        :lack.middleware.session.store)
  (:import-from :lack.util
                :generate-random-id
                :valid-id-p)
  (:export :memory-store
           :make-memory-store
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack.middleware.session.store.memory)

(defstruct (memory-store (:include store))
  (stash (make-hash-table :test 'equal)))

(defmethod fetch-session ((store memory-store) sid)
  (when (valid-id-p sid)
    (let ((data (gethash sid (memory-store-stash store))))
      (when data
        (make-session :id sid :data data)))))

(defmethod store-session ((store memory-store) session)
  (unless (session-id session)
    (setf (session-id session)
          (generate-random-id)))
  (setf (gethash (session-id session) (memory-store-stash store))
        (session-data session)))

(defmethod remove-session ((store memory-store) session)
  (when (session-id session)
    (remhash (session-id session) (memory-store-stash store))))
