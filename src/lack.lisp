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
                 silent
                 (use-thread #+thread-support t #-thread-support nil)
               &allow-other-keys)
  (flet ((start-message ()
           (unless silent
             (format t "~&~:(~A~) server is started.~%Listening on localhost:~A.~%" server port))))
    (unless use-thread
      (start-message))
    (prog1
        (apply #'lack.handler:run app server
               :port port
               :debug debug
               :use-thread use-thread
               (delete-from-plist args :server :port :debug :silent :use-thread))
      (start-message))))
