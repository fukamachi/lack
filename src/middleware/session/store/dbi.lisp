(in-package :cl-user)
(defpackage lack.middleware.session.store.dbi
  (:nicknames :lack.session.store.dbi)
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
  (:export :dbi-store
           :make-dbi-store
           :fetch-session
           :store-session
           :remove-session))
(in-package :lack.middleware.session.store.dbi)

(defstruct (dbi-store (:include store))
  (connector nil :type function)
  (serializer (lambda (data)
                (usb8-array-to-base64-string
                 (string-to-utf-8-bytes (prin1-to-string (marshal data))))))
  (deserializer (lambda (data)
                  (unmarshal (read-from-string
                              (utf-8-bytes-to-string (base64-string-to-usb8-array data))))))
  (table-name "sessions"))

(defmethod fetch-session ((store dbi-store) sid)
  (let* ((conn (funcall (dbi-store-connector store)))
         (query (dbi:prepare conn
                             (format nil "SELECT session_data FROM ~A WHERE id = ?"
                                     (dbi-store-table-name store))))
         (result (dbi:fetch (dbi:execute query sid))))
    (if result
        (handler-case (funcall (dbi-store-deserializer store) (getf result :|session_data|))
          (error (e)
            (warn "Error (~A) occured while deserializing a session. Ignoring.~2%    Data:~%        ~A~2%    Error:~%        ~A"
                  (class-name (class-of e))
                  (getf result :|session_data|)
                  e)
            nil))
        nil)))

(defmethod store-session ((store dbi-store) sid session)
  (let ((conn (funcall (dbi-store-connector store)))
        (serialized-session (funcall (dbi-store-serializer store) session)))
    (dbi:with-transaction conn
      (let* ((query (dbi:prepare conn
                                 (format nil "SELECT session_data FROM ~A WHERE id = ?"
                                         (dbi-store-table-name store))))
             (current-session (getf (dbi:fetch (dbi:execute query sid)) :|session_data|)))
        (cond
          ;; Session exists but not changed
          ((equal current-session serialized-session))
          ;; Session exists and is going to be changed
          (current-session
           (dbi:do-sql conn (format nil "UPDATE ~A SET session_data = ? WHERE id = ?"
                                    (dbi-store-table-name store))
             serialized-session
             sid))
          ;; New session
          (t
           (dbi:do-sql conn (format nil "INSERT INTO ~A (id, session_data) VALUES (?, ?)"
                                    (dbi-store-table-name store))
             sid
             serialized-session)))))))

(defmethod remove-session ((store dbi-store) sid)
  (dbi:do-sql (funcall (dbi-store-connector store))
    (format nil "DELETE FROM ~A WHERE id = ?"
            (dbi-store-table-name store))
    sid))
