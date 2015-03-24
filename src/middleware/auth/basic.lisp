(in-package :cl-user)
(defpackage lack.middleware.auth.basic
  (:use :cl)
  (:import-from :cl-base64
                :base64-string-to-string)
  (:import-from :split-sequence
                :split-sequence))
(in-package :lack.middleware.auth.basic)

(defparameter *lack-middleware-auth-basic*
  (lambda (app &key authenticator (realm "restricted area"))
    (unless authenticator
      (error ":authenticator is required in lack-middleware-auth-basic"))
    (check-type authenticator function)
    (lambda (env)
      (block nil
        (let ((authorization (gethash "authorization" (getf env :headers))))
          (unless authorization
            (return (return-401 realm)))

          (destructuring-bind (user &optional (pass ""))
              (parse-authorization-header authorization)
            (if user
                (multiple-value-bind (result returned-user)
                    (funcall authenticator user pass)
                  (if result
                      (progn
                        (setf (getf env :remote-user)
                              (or returned-user user))
                        (funcall app env))
                      (return-401 realm)))
                (return-401 realm)))))))
  "Middleware for Basic Authentication")

(defun return-401 (realm)
  `(401
    (:content-type "text/plain"
     :content-length 22
     :www-authenticate ,(format nil "Basic realm=~A" realm))
    ("Authorization required")))

(defun parse-authorization-header (authorization)
  (when (string= authorization "Basic " :end1 6)
    (let ((user-and-pass (base64-string-to-string (subseq authorization 6))))
      (split-sequence #\: user-and-pass))))
