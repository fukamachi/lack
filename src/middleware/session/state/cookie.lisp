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
  (:import-from :alexandria
                :remove-from-plist)
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
  (httponly nil :type boolean))

(defmethod extract-sid ((state cookie-state) env)
  (let ((req (make-request env)))
    (cdr (assoc "lack.session" (request-cookies req) :test #'string=))))

(defmethod expire-state ((state cookie-state) sid res options)
  (setf (cookie-state-expires state) 0)
  (finalize-state state sid res options))

(defmethod finalize-state ((state cookie-state) sid (res function) options)
  (lambda (responder)
    (funcall res (lambda (actual-res)
		   (funcall responder (finalize-state state sid actual-res options))))))

(defmethod finalize-state ((state cookie-state) sid (res list) options)
  (let ((res (apply #'make-response res))
        (options (append (remove-from-plist options :id)
                         (with-slots (path domain expires secure httponly) state
                           (list :path path
                                 :domain domain
                                 :secure secure
                                 :httponly httponly
                                 :expires (+ (get-universal-time) expires))))))
    (setf (getf (response-set-cookies res) :|lack.session|)
          `(:value ,sid ,@options))
    (finalize-response res)))
