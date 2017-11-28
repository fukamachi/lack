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

(defmacro with-db-connection (connection store &body body)
  `(let ((,connection (funcall (dbi-store-connector ,store))))
     (unwind-protect
          (progn ,@body)
       (when (dbi-store-disconnector ,store)
         (funcall (dbi-store-disconnector ,store) ,connection)))))

(defstruct (dbi-store (:include store))
  (connector nil :type function)
  (disconnector nil)
  (serializer (lambda (data)
                (usb8-array-to-base64-string
                 (string-to-utf-8-bytes (prin1-to-string (marshal data))))))
  (deserializer (lambda (data)
                  (unmarshal (read-from-string
                              (utf-8-bytes-to-string (base64-string-to-usb8-array data))))))
  (record-timestamps nil :type boolean)
  (table-name "sessions"))

(defmethod fetch-session ((store dbi-store) sid)
  (with-db-connection conn store
    (let* ((query (dbi:prepare conn
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
         nil))))

(defun current-timestamp ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    (format nil "~D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month date
            hour min sec)))

(defmethod store-session ((store dbi-store) sid session)
  (with-db-connection conn store
    (let ((serialized-session (funcall (dbi-store-serializer store) session)))
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
             (dbi:do-sql conn
               (format nil "UPDATE ~A SET session_data = ?~:[~*~;, updated_at = '~A'~] WHERE id = ?"
                       (dbi-store-table-name store)
                       (dbi-store-record-timestamps store)
                       (current-timestamp))
               serialized-session
               sid))
            ;; New session
            (t
             (dbi:do-sql conn (format nil "INSERT INTO ~A (id, session_data~:[~;, created_at, updated_at~]) VALUES (?, ?~:*~:[~*~;, '~A', ~:*'~A'~])"
                                      (dbi-store-table-name store)
                                      (dbi-store-record-timestamps store)
                                      (current-timestamp))
                         sid
                         serialized-session))))))))

(defmethod remove-session ((store dbi-store) sid)
  (with-db-connection conn store
    (dbi:do-sql conn
      (format nil "DELETE FROM ~A WHERE id = ?"
              (dbi-store-table-name store))
      sid)))
