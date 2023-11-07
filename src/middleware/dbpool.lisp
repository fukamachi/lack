(defpackage #:lack/middleware/dbpool
  (:use #:cl)
  (:nicknames #:lack.middleware.dbpool)
  (:import-from #:dbi
                #:connect
                #:disconnect
                #:ping)
  (:import-from #:anypool
                #:make-pool
                #:too-many-open-connection)
  (:export #:*lack-middleware-dbpool*
           #:with-connection))
(in-package #:lack/middleware/dbpool)

(defparameter *connection-pool-storage*
  (make-hash-table :test 'eq))

(defun make-connection-pool (connect-args pool-args)
  (apply #'anypool:make-pool
         :connector (lambda () (apply #'dbi:connect connect-args))
         :disconnector #'dbi:disconnect
         :ping #'dbi:ping
         pool-args))

(defun get-connection-pool (database-id)
  (or (gethash database-id *connection-pool-storage*)
      (error "No connection pool found for ~S" database-id)))

(defmacro with-connection ((var database-id) &body body)
  (let ((e (gensym "E")))
    `(block nil
       (handler-bind ((anypool:too-many-open-connection
                        (lambda (,e)
                          (declare (ignore ,e))
                          (return '(503 (:content-type "text/plain") ("Service Temporarily Unavailable"))))))
         (anypool:with-connection (,var (get-connection-pool ,database-id))
           ,@body)))))

(defparameter *lack-middleware-dbpool*
  (lambda (app database-id &key connect-args pool-args)
    (check-type database-id symbol)
    (assert connect-args)
    (let ((pool (make-connection-pool connect-args pool-args)))
      (setf (gethash database-id *connection-pool-storage*) pool)
      (lambda (env)
        (funcall app env)))))
