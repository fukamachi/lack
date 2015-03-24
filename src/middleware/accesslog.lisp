(in-package :cl-user)
(defpackage lack.middleware.accesslog
  (:use :cl)
  (:import-from :lack.util
                :funcall-with-cb
                :content-length)
  (:import-from :local-time
                :format-timestring
                :now)
  (:export :*lack-middleware-accesslog*
           :*time-format*
           :default-formatter))
(in-package :lack.middleware.accesslog)

(defparameter *lack-middleware-accesslog*
  (let ((no-body '#:no-body))
    (lambda (app &key
              (logger
               (lambda (output) (format t "~&~A~%" output)))
              (formatter #'default-formatter))
      (lambda (env)
        (funcall-with-cb
         app env
         (lambda (res)
           (funcall logger
                    (funcall formatter env res (now)))
           res)))))
  "Middleware for logging requests")

(defvar *time-format*
  '((:day 2) #\/ :short-month #\/ (:year 4) #\: (:hour 2) #\: (:min 2) #\: (:sec 2) #\Space :gmt-offset))

(defun default-formatter (env res now)
  (format nil "~A - [~A] \"~A ~A ~A\" ~A ~A \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
          (getf env :remote-addr)
          (local-time:format-timestring nil now :format *time-format*)
          (getf env :request-method)
          (getf env :request-uri)
          (getf env :server-protocol)
          (car res)
          (content-length res)
          (getf env :http-referer)
          (getf env :http-user-agent)))
