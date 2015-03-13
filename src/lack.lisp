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

(defun eval-file (file)
  "Safer way to read and eval a file content. This function returns the last value."
  (check-type file (or pathname string))
  (with-open-file (in file)
    (let ((*package* *package*)
          (*readtable* *readtable*)
          (*load-pathname* nil)
          (*load-truename* nil))
      (loop with results
            with eof = '#:eof
            for form = (read in nil eof)
            until (eq form eof)
            do (setf results (multiple-value-list (eval form)))
            finally
               (return (apply #'values results))))))

(defun lackup (app &rest args
               &key (server :hunchentoot)
                 (port 5000)
                 (debug t)
                 silent
                 (use-thread #+thread-support t #-thread-support nil)
                 (use-default-middlewares t)
               &allow-other-keys)
  (flet ((print-start-message ()
           (unless silent
             (format t "~&~:(~A~) server is started.~%Listening on localhost:~A.~%" server port)))
         (buildapp (app)
           (let ((app (etypecase app
                        ((or pathname string)
                         (eval-file app))
                        (function app))))
             (if use-default-middlewares
                 (builder :backtrace app)
                 app))))
    (unless use-thread
      (print-start-message))
    (prog1
        (apply #'lack.handler:run (buildapp app) server
               :port port
               :debug debug
               :use-thread use-thread
               (delete-from-plist args :server :port :debug :silent :use-thread))
      (print-start-message))))
