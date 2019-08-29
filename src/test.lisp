(in-package :cl-user)
(defpackage lack.test
  (:use :cl)
  (:import-from :quri
                :uri
                :uri-path
                :uri-query
                :render-uri
                :url-encode-params)
  (:import-from :cl-cookie
                :make-cookie-jar
                :make-cookie
                :parse-set-cookie-header
                :merge-cookies
                :cookie-jar-cookies
                :write-cookie-header)
  (:import-from :flexi-streams
                :make-in-memory-input-stream
                :string-to-octets)
  (:export :generate-env
           :parse-lack-session
           :testing-app
           :request))
(in-package :lack.test)

(defun generate-env (uri &key (method :get) content headers cookie-jar cookies)
  "Creates an ENV plist much like this do all Clack backends.
   
   Argument `uri' can be just a path or a full url with scheme and optional port."

  (let* ((uri (quri:uri uri))
         (path (if (quri:uri-path uri)
                   (quri:url-decode (quri:uri-path uri) :lenient t)
                   "/"))
         (query (quri:uri-query uri))
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
                              (if (and (consp content)
                                       (find-if #'pathnamep content :key #'cdr))
                                  "multipart/form-data"
                                  "application/x-www-form-urlencoded"))))
        (if (assoc "content-type" headers :test #'string-equal)
            (setf (cdr (assoc "content-type" headers :test #'string-equal))
                  content-type)
            (setf headers (append headers `(("content-type" . ,content-type)))))))
    (when (or cookies cookie-jar)
      (let ((cookie-jar (or cookie-jar
                            (make-cookie-jar))))
        (merge-cookies cookie-jar
                       (loop for (k . v) in cookies
                             collect (make-cookie :name k :value v)))
        (let* ((cookie (assoc "cookie" headers :test 'equal))
               (new-cookie (format nil "~@[~A; ~]~A"
                                   (cdr cookie)
                                   (write-cookie-header (cookie-jar-cookies cookie-jar)))))
          (if cookie
              (setf (cdr cookie) new-cookie)
              (setf headers
                    (append headers
                            `(("cookie" . ,new-cookie))))))))
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
          :request-uri (format nil "~A~@[?~A~]"
                               (or path "/")
                               query)
          :script-name ""
          :path-info path
          :query-string query
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
                         do (setf (gethash (string-downcase k) hash) v)
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

(defvar *current-app*)

(defun request (uri &rest args &key (method :get) content headers cookie-jar
                                    (max-redirects 5))
  (let ((env (generate-env uri
                           :method method :content content :headers headers
                           :cookie-jar cookie-jar))
        (uri (quri:uri uri)))
    (unless (quri:uri-host uri)
      (setf (quri:uri-host uri) "localhost"))
    (unless (quri:uri-port uri)
      (setf (quri:uri-port uri) 80))
    (unless (quri:uri-scheme uri)
      (setf (quri:uri-scheme uri) "http"))
    (destructuring-bind (status headers body)
        (funcall *current-app* env)
      (when cookie-jar
        (merge-cookies cookie-jar
                       (loop for (k v) on headers by #'cddr
                             when (eq k :set-cookie)
                             collect
                                (parse-set-cookie-header v
                                                         (quri:uri-host uri)
                                                         (quri:uri-path uri)))))
      (when (and (member status '(301 302 303 307) :test #'=)
                 (getf headers :location)
                 (not (eq method :head))
                 (/= max-redirects 0))
        (return-from request
          (apply #'request (quri:merge-uris (quri:uri (getf headers :location)) uri)
                 :method (if (or (= status 307)
                                 (member method '(:head :get)))
                             method
                             :get)
                 :max-redirects (1- max-redirects)
                 args)))
      ;; XXX: Framework sometimes return '(NIL) as body
      (when (consp body)
        (setf body (remove nil body)))
      (values
       ;; TODO: support pathname
       ;; TODO: check if the response content-type is text/binary
       (typecase body
         (cons (apply #'concatenate (type-of (first body)) body))
         (null "")
         (otherwise body))
       status
       (loop with hash = (make-hash-table :test 'equal)
             for (k v) on headers by #'cddr
             for down-k = (string-downcase k)
             do (setf (gethash down-k hash)
                      (format nil "~@[~A, ~]~A"
                              (gethash down-k hash) v))
             finally (return hash))
       uri
       nil))))

(defmacro testing-app (app &body body)
  `(let ((*current-app* ,app))
     ,@body))
