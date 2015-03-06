(in-package :cl-user)
(defpackage lack
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :lack.handler
                :run
                :stop)
  (:import-from :alexandria
                :delete-from-plist)
  (:export :lackup
           :stop
           :builder))
(in-package :lack)

(defun lackup (app &rest args
               &key (server :hunchentoot)
                 (port 5000)
                 (debug t)
               &allow-other-keys)
  (apply #'lack.handler:run app server
         :port port
         :debug debug
         (delete-from-plist args :server :use-thread :port :debug)))
