(in-package :cl-user)
(defpackage lack.middleware.session
  (:use :cl)
  (:import-from :lack.session.store
                :session-id
                :session-data
                :fetch-session
                :store-session
                :remove-session)
  (:import-from :lack.session.state
                :expire-state
                :extract-sid
                :finalize-state)
  (:import-from :lack.session.store.memory
                :make-memory-store)
  (:import-from :lack.session.state.cookie
                :make-cookie-state)
  (:export :*lack-middleware-session*))
(in-package :lack.middleware.session)

(defparameter *lack-middleware-session*
  (lambda (app &key
            (store (make-memory-store))
            (state (make-cookie-state)))
    (lambda (env)
      (let* ((sid (extract-sid state env))
             (session (fetch-session store sid)))
        (setf (getf env :lack.session)
              (session-data session))
        (setf (getf env :lack.session.options)
              (list :id sid))
        (finalize store state env session sid (funcall app env)))))
  "Middleware for session management")

(defun finalize (store state env session request-sid res)
  (let ((options (getf env :lack.session.options)))
    (when session
      (apply #'commit store session options))
    (if (getf options :expire)
        (expire-state state request-sid res options)
        (finalize-state state (session-id session) res options))))

(defun commit (store session &key expire change-id &allow-other-keys)
  (cond
    (expire
     (remove-session store session))
    (change-id
     (remove-session store session)
     (store-session store session))
    (t
     (store-session store session))))
