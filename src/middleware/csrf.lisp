(in-package :cl-user)
(defpackage lack.middleware.csrf
  (:use :cl)
  (:import-from :lack.request
                :make-request
                :request-body-parameters)
  (:import-from :lack.util
                :generate-random-id)
  (:export :*lack-middleware-csrf*
           :csrf-token
           :csrf-html-tag))
(in-package :lack.middleware.csrf)

(defvar *csrf-session-key*)

(defparameter *lack-middleware-csrf*
  (lambda (app &key (block-app #'return-400) one-time
            (session-key "_csrf_token"))
    (lambda (env)
      (let ((*csrf-session-key* session-key))
        (block nil
          (unless (danger-method-p (getf env :request-method))
            (return (funcall app env)))

          (let ((session (getf env :lack.session)))
            (unless session
              (error ":lack.session is missing in ENV. Wrap this app up with lack.middleware.session"))

            (if (valid-token-p env)
                (progn
                  (when one-time
                    (remhash *csrf-session-key* session))
                  (funcall app env))
                (funcall block-app env)))))))
  "Middleware for easy CSRF protection")

(defun return-400 (env)
  (declare (ignore env))
  '(400
    (:content-type "text/plain"
     :content-length 31)
    ("Bad Request: invalid CSRF token")))

(defun danger-method-p (request-method)
  (member request-method
          '(:POST :PUT :DELETE :PATCH)
          :test #'eq))

(defun valid-token-p (env)
  (let ((req (make-request env))
        (csrf-token (gethash *csrf-session-key*
                             (getf env :lack.session))))
    (and csrf-token
         (let ((received-csrf-token
                 (cdr (assoc "_csrf_token" (request-body-parameters req) :test #'string=))))
           ;; for multipart/form-data
           (when (listp received-csrf-token)
             (setf received-csrf-token (first received-csrf-token)))
           (equal csrf-token received-csrf-token)))))

(defun csrf-token (session)
  (unless (gethash *csrf-session-key* session)
    (setf (gethash *csrf-session-key* session) (generate-random-id)))
  (gethash *csrf-session-key* session))

(defun csrf-html-tag (session)
  (format nil "<input type=\"hidden\" name=\"_csrf_token\" value=\"~A\">"
          (csrf-token session)))
