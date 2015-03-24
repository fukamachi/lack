(in-package :cl-user)
(defpackage lack.middleware.session
  (:use :cl)
  (:import-from :lack.session.store
                :fetch-session
                :store-session
                :remove-session)
  (:import-from :lack.session.state
                :expire-state
                :extract-sid
                :finalize-state
                :generate-sid)
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
             (session (and sid
                           (fetch-session store sid)))
             (sid (or sid
                      (generate-sid state env))))
        (setf (getf env :lack.session)
              (or session (make-hash-table :test 'equal)))
        (setf (getf env :lack.session.options)
              (list :id sid))
        (finalize store state env (funcall app env)))))
  "Middleware for session management")

(defun finalize (store state env res)
  (let ((session (getf env :lack.session))
        (options (getf env :lack.session.options)))
    (when session
      (apply #'commit store state session env options))
    (if (getf options :expire)
        (expire-state state (getf options :id) res options)
        (finalize-state state (getf options :id) res options))))

(defun commit (store state session env &key id expire change-id &allow-other-keys)
  (cond
    (expire
     (remove-session store id))
    (change-id
     (remove-session store id)
     (let ((new-sid (generate-sid state env)))
       (store-session store new-sid session)))
    (t
     (store-session store id session))))
