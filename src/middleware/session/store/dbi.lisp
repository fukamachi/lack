(in-package :cl-user)
(defpackage lack.middleware.session.store.dbi
  (:nicknames :lack.session.store.dbi)
  (:use :cl
        :lack.middleware.session.store)
  (:import-from :lack.util
                :generate-random-id
                :valid-id-p)
  (:import-from :marshal
                :marshal
                :unmarshal)
  (:import-from :cl-base64
                :base64-string-to-string
                :string-to-base64-string)
  (:export :make-dbi-store))
(in-package :lack.middleware.session.store.dbi)

(defstruct dbi-store
  (connector nil :type function)
  (serializer (lambda (data)
                (string-to-base64-string (prin1-to-string (marshal data)))))
  (deserializer (lambda (data)
                  (unmarshal (read-from-string (base64-string-to-string data)))))
  (table-name "sessions"))

(defmethod fetch-session ((store dbi-store) sid)
  (when (valid-id-p sid)
    (let* ((conn (funcall (dbi-store-connector store)))
           (query (dbi:prepare conn
                               (format nil "SELECT session_data FROM ~A WHERE id = ?"
                                       (dbi-store-table-name store))))
           (result (dbi:fetch (dbi:execute query sid)))
           (data (when result
                   (ignore-errors (funcall (dbi-store-deserializer store) (getf result :|session_data|))))))
      (when data
        (make-session :id sid :data data)))))

(defmethod store-session ((store dbi-store) session)
  (unless (session-id session)
    (setf (session-id session)
          (generate-random-id)))
  (let* ((sid (session-id session))
         (conn (funcall (dbi-store-connector store)))
         (query (dbi:prepare conn
                             (format nil "SELECT 1 FROM ~A WHERE id = ?"
                                     (dbi-store-table-name store))))
         (existsp (dbi:fetch (dbi:execute query sid)))
         (serialized-session (funcall (dbi-store-serializer store) (session-data session))))
    (if existsp
        (dbi:do-sql conn (format nil "UPDATE ~A SET session_data = ? WHERE id = ?"
                                 (dbi-store-table-name store))
          serialized-session
          sid)
        (dbi:do-sql conn (format nil "INSERT INTO ~A (id, session_data) VALUES (?, ?)"
                                 (dbi-store-table-name store))
          sid
          serialized-session))))

(defmethod remove-session ((store dbi-store) session)
  (when (session-id session)
    (dbi:do-sql (funcall (dbi-store-connector store))
      (format nil "DELETE FROM ~A WHERE id = ?"
              (dbi-store-table-name store))
      (session-id session))))
