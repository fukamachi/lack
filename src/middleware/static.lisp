(defpackage lack/middleware/static
  (:nicknames :lack.middleware.static)
  (:use :cl)
  (:import-from :lack/component
                :call)
  (:import-from :lack/app/file
                :lack-app-file)
  (:import-from :alexandria
                :starts-with-subseq
                :if-let)
  (:export :*lack-middleware-static*
           :call-app-file))
(in-package :lack/middleware/static)

(defparameter *lack-middleware-static*
  (lambda (app &key path (root #P"./") headers)
    (etypecase path
      (null app)
      (string
       (lambda (env)
         (let ((path-info (getf env :path-info)))
           (if (starts-with-subseq path path-info)
               (progn
                 (setf (getf env :path-info)
                       (subseq path-info (1- (length path))))
                 (call-app-file root env
                                :headers headers))
               (funcall app env)))))
      (function
       (lambda (env)
        (let ((path-info (getf env :path-info)))
          (if-let (new-path (funcall path path-info))
            (progn
              (setf (getf env :path-info) new-path)
              (call-app-file root env
                             :headers headers))
            (funcall app env)))))))
  "Middleware for serving static files")

(defun call-app-file (root env &key headers)
  (call (make-instance 'lack-app-file :root root :headers headers) env))
