(in-package :cl-user)
(defpackage lack.middleware.mount
  (:use :cl)
  (:import-from :lack.component
                :to-app)
  (:export :*lack-middleware-mount*))
(in-package :lack.middleware.mount)

(defparameter *lack-middleware-mount*
  (lambda (app path mount-app)
    (let ((len (length path)))
      (lambda (env)
        (let ((path-info (getf env :path-info)))
          (cond
            ((string= path-info path)
             (setf (getf env :path-info) "/")
             (funcall (to-app mount-app) env))
            ((and (< len (length path-info))
                  (string= path-info path :end1 len)
                  (char= (aref path-info len) #\/))
             (setf (getf env :path-info)
                   (subseq path-info (length path)))
             (funcall (to-app mount-app) env))
            (t
             (funcall app env)))))))
  "Middleware for attaching another Lack application on a specific URL")
