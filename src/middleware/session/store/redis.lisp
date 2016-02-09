(in-package :cl-user)
(defpackage lack.middleware.session.store.redis
  (:nicknames :lack.session.store.redis)
  (:use :cl
        :lack.middleware.session.store)
  (:import-from :marshal
                :marshal
                :unmarshal)
  (:import-from :cl-base64
                :base64-string-to-usb8-array
                :usb8-array-to-base64-string)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string)
  (:export :redis-store
           :make-redis-store
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack.middleware.session.store.redis)

(defun open-connection (&key host port)
  (make-instance 'redis:redis-connection
                 :host host
                 :port port))

(defstruct (redis-store (:include store)
                        (:constructor %make-redis-store))
  (host "127.0.0.1")
  (port 6379)
  (namespace "session" :type string)
  (expires nil :type (or null integer))
  (serializer (lambda (data)
                (usb8-array-to-base64-string
                 (string-to-utf-8-bytes (prin1-to-string (marshal data))))))
  (deserializer (lambda (data)
                  (unmarshal (read-from-string
                              (utf-8-bytes-to-string (base64-string-to-usb8-array data))))))

  connection)

(defun make-redis-store (&rest args &key (host "127.0.0.1") (port 6379) connection namespace expires serializer deserializer)
  (declare (ignore namespace expires serializer deserializer))
  (if connection
      (setf (getf args :host) (redis::conn-host connection)
            (getf args :port) (redis::conn-port connection))
      (setf (getf args :connection)
            (open-connection :host host :port port)))
  (apply #'%make-redis-store args))

(defun redis-connection (store)
  (check-type store redis-store)
  (with-slots (host port connection) store
    (unless (redis::connection-open-p connection)
      (setf connection
            (open-connection :host host :port port)))
    connection))

(defmacro with-connection (store &body body)
  `(let ((redis::*connection* (redis-connection ,store)))
     ,@body))

(defmethod fetch-session ((store redis-store) sid)
  (let ((data (with-connection store
                (red:get (format nil "~A:~A"
                                 (redis-store-namespace store)
                                 sid)))))
    (if data
        (handler-case (funcall (redis-store-deserializer store) data)
          (error (e)
            (warn "Error (~A) occured while deserializing a session. Ignoring.~2%    Data:~%        ~A~2%    Error:~%        ~A"
                  (class-name (class-of e))
                  data
                  e)
            nil))
        nil)))

(defmethod store-session ((store redis-store) sid session)
  (let ((data (funcall (redis-store-serializer store) session))
        (key  (format nil "~A:~A" (redis-store-namespace store) sid)))
    (with-connection store
      (red:set key data)
      (when (redis-store-expires store)
        (red:expire key (redis-store-expires store))))))

(defmethod remove-session ((store redis-store) sid)
  (with-connection store
    (red:del (format nil "~A:~A"
                     (redis-store-namespace store)
                     sid))))
