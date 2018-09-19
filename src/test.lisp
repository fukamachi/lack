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
  "Creates an ENV plist much like this do all Clack backends.
   
   Argument `uri' can be just a path or a full url with scheme and optional port."

  (let* ((uri (quri:uri uri))
         (path-with-params (quri:copy-uri uri
                                          :host nil
                                          :port nil
                                          :scheme nil))
         (host (or (quri:uri-host uri)
                   "localhost"))
         (port (or (quri:uri-port uri)
                   80))
         (scheme (or (quri.uri:uri-scheme uri)
                     "http")))

    ;; default headers
    (setf headers (append `(("host" . ,host) ("accept" . "*/*")) headers))

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
    (setf content
          (etypecase content
            (cons (flex:string-to-octets
                   (quri:url-encode-params content)
                   :external-format :utf-8))
            (string (flex:string-to-octets content
                                           :external-format :utf-8))
            (array content)
            (null nil)))
    (list :request-method method
          ;; Seems that all Clack handlers put into this field
          ;; only pathname with GET parameters
          :request-uri (quri:render-uri path-with-params)
          :script-name ""
          :path-info (quri:uri-path uri)
          :query-string (or (quri:uri-query uri) "")
          :server-name host
          :server-port port
          :server-protocol :http/1.1
          :url-scheme scheme
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
