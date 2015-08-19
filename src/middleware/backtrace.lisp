(in-package :cl-user)
(defpackage lack.middleware.backtrace
  (:use :cl)
  (:import-from :uiop/image
                :print-condition-backtrace)
  (:export :*lack-middleware-backtrace*))
(in-package :lack.middleware.backtrace)

(defparameter *lack-middleware-backtrace*
  (lambda (app &key
            (output '*error-output*)
            result-on-error)
    (check-type output (or symbol stream pathname string))
    (check-type result-on-error (or function cons null))
    (flet ((error-handler (condition)
             (if (functionp result-on-error)
                 (funcall result-on-error condition)
                 result-on-error))
           (output-backtrace (condition env)
             (etypecase output
               (symbol (print-error condition env (symbol-value output)))
               (stream (print-error condition env output))
               ((or pathname string)
                (with-open-file (out output
                                     :direction :output
                                     :external-format #+clisp charset:utf-8 #-clisp :utf-8
                                     :if-exists :append
                                     :if-does-not-exist :create)
                  (print-error condition env out))))))
      (lambda (env)
        (block nil
          (handler-bind ((error (lambda (condition)
                                  (output-backtrace condition env)
                                  (when result-on-error
                                    (return (error-handler condition))))))
            (funcall app env))))))
  "Middleware for outputting backtraces when an error occured")

(defun print-error (error env &optional (stream *error-output*))
  (print-condition-backtrace error :stream stream)
  (format stream "~2&Request:~%")
  (loop for (k v) on env by #'cddr
        if (hash-table-p v) do
          (format stream "~&    ~A:~%" k)
          (maphash (lambda (k v)
                     (format stream "~&        ~A: ~S~%"
                             k v))
                   v)
        else do
          (format stream
                  "~&    ~A: ~S~%"
                  k v))
  (values))
