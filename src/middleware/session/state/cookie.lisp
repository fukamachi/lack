(in-package :cl-user)
(defpackage lack.middleware.session.state.cookie
  (:nicknames :lack.session.state.cookie)
  (:use :cl
        :lack.middleware.session.state)
  (:import-from :lack.request
                :make-request
                :request-cookies)
  (:import-from :lack.response
                :make-response
                :finalize-response
                :response-set-cookies)
  (:export :cookie-state
           :make-cookie-state
           :generate-sid
           :extract-sid
           :expire-state
           :finalize-session))
(in-package :lack.middleware.session.state.cookie)

(defstruct (cookie-state (:include state))
  (path "/" :type string)
  (domain nil :type (or string null))
  (expires (get-universal-time) :type integer)
  (secure nil :type boolean)
  (httponly nil :type boolean)
  (cookie-key "lack.session" :type string))

(defmethod extract-sid ((state cookie-state) env)
  (let ((req (make-request env)))
    (cdr (assoc (cookie-state-cookie-key state)
                (request-cookies req) :test #'string=))))

(defmethod expire-state ((state cookie-state) sid res options)
  (setf (getf options :expires) 0)
  (finalize-state state sid res options))

(defmethod finalize-state ((state cookie-state) sid (res function) options)
  (lambda (responder)
    (funcall res (lambda (actual-res)
                   (funcall responder (finalize-state state sid actual-res options))))))

(defmethod finalize-state ((state cookie-state) sid (res list) options)
  ;; Don't send Set-Cookie header when it's not necessary.
  (destructuring-bind (&key no-store new-session change-id expire &allow-other-keys)
      options
    (when (or no-store
              (not (or new-session change-id expire)))
      (return-from finalize-state res)))

  (let ((res (apply #'make-response res))
        (options (with-slots (path domain expires secure httponly) state
                   (list :path path
                         :domain domain
                         :secure secure
                         :httponly httponly
                         :expires (+ (get-universal-time)
                                     (getf options :expires expires))))))
    (setf (getf (response-set-cookies res) (cookie-state-cookie-key state))
          `(:value ,sid ,@options))
    (finalize-response res)))
