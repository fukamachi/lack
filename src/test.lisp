(in-package :cl-user)
(defpackage lack.test
  (:use :cl)
  (:import-from :quri
                :uri
                :uri-path
                :uri-query
                :render-uri
                :url-encode-params)
  (:import-from :flexi-streams
                :make-in-memory-input-stream
                :string-to-octets)
  (:export :generate-env
           :parse-lack-session))
(in-package :lack.test)

(defun generate-env (uri &key (method :get) content headers cookies)
  ;; default headers
  (setf headers (append '(("host" . "localhost") ("accept" . "*/*")) headers))
  (when content
    (let ((content-type (or (cdr (assoc "content-type" headers :test #'string-equal))
                            (if (find-if #'pathnamep content :key #'cdr)
                                "multipart/form-data"
                                "application/x-www-form-urlencoded"))))
      (if (assoc "content-type" headers :test #'string-equal)
          (setf (cdr (assoc "content-type" headers :test #'string-equal))
                content-type)
          (setf headers (append headers `(("content-type" . ,content-type)))))))
  (when cookies
    (setf headers
          (append headers
                  `(("cookie" . ,(with-output-to-string (s)
                                   (format s "~A=~A" (caar cookies) (cdar cookies))
                                   (loop for (k . v) in (cdr cookies)
                                         do (format s "; ~A=~A" k v))))))))
  (when content
    (setf content (flex:string-to-octets
                   (quri:url-encode-params content))))
  (let ((uri (quri:uri uri)))
    (list :request-method method
          :request-uri (quri:render-uri uri)
          :script-name ""
          :path-info (quri:uri-path uri)
          :query-string (or (quri:uri-query uri) "")
          :server-name "localhost"
          :server-port 80
          :server-protocol :http/1.1
          :remote-addr "127.0.0.1"
          :remote-port 12345
          :content-type (cdr (assoc "content-type" headers :test #'string-equal))
          :content-length (and content
                               (length content))
          :headers (loop with hash = (make-hash-table :test 'equal)
                         for (k . v) in headers
                         do (setf (gethash k hash) v)
                         finally (return hash))
          :raw-body (and content
                         (flex:make-in-memory-input-stream content)))))

(defun parse-lack-session (headers)
  (let ((set-cookie (getf headers :set-cookie)))
    (when set-cookie
      (when (string= set-cookie "lack.session=" :end1 #.(length "lack.session="))
        (subseq set-cookie
                #.(length "lack.session=")
                (position #\; set-cookie))))))
