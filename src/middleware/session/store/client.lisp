(in-package :cl-user)
(defpackage lack.middleware.session.store.client
  (:nicknames :lack.session.store.client)
  (:use :cl
        :lack.middleware.session.store)
  (:import-from :marshal
                :marshal
                :unmarshal)
  (:import-from :cl-base64
                :base64-string-to-string
                :string-to-base64-string)
  (:import-from :lack.util
                :hmac-signature
                :generate-random-id)
  (:export :make-client-store))
(in-package :lack.middleware.session.store.client)

(defstruct client-store
  (secret-key (generate-random-id))
  (serializer (lambda (data)
                (string-to-base64-string (prin1-to-string (marshal data)))))
  (deserializer (lambda (data)
                  (unmarshal (read-from-string (base64-string-to-string data))))))

(defmethod fetch-session ((store client-store) sid)
  (let ((separator (position #\| sid)))
    (when separator
      (let ((data (subseq sid 0 separator))
            (sign (subseq sid (1+ separator)))
            (secret (client-store-secret-key store)))
        (when (string= (hmac-signature secret data) sign)
          (make-session :data (funcall (client-store-deserializer store) data)))))))

(defmethod store-session ((store client-store) session)
  (setf (session-id session)
        (let ((data (funcall (client-store-serializer store) (session-data session)))
              (secret (client-store-secret-key store)))
          (concatenate 'string data "|" (hmac-signature secret data)))))

(defmethod remove-session ((store client-store) session)) ;; no-op
