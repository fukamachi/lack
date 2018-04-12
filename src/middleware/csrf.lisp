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

(defvar *csrf-token-key*)

(defparameter *lack-middleware-csrf*
  (lambda (app &key (block-app #'return-400) one-time
            (csrf-token-key :csrf-token))
    (lambda (env)
      (let ((*csrf-token-key* csrf-token-key))
        (block nil
          (unless (danger-method-p (getf env :request-method))
            (return (funcall app env)))

          (let ((session (getf env :lack.session)))
            (unless session
              (error ":lack.session is missing in ENV. Wrap this app up with lack.middleware.session"))

            (if (valid-token-p env)
                (progn
                  (when one-time
                    (remhash csrf-token-key session))
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
        (csrf-token (gethash *csrf-token-key*
                             (getf env :lack.session))))
    (and csrf-token
         (let ((recieved-csrf-token
                 (cdr (assoc "_csrf_token" (request-body-parameters req) :test #'string=))))
           (string= csrf-token recieved-csrf-token)))))

(defun csrf-token (session)
  (unless (gethash *csrf-token-key* session)
    (setf (gethash *csrf-token-key* session) (generate-random-id)))
  (gethash *csrf-token-key* session))

(defun csrf-html-tag (session)
  (format nil "<input type=\"hidden\" name=\"_csrf_token\" value=\"~A\">"
          (csrf-token session)))
