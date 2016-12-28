(in-package :cl-user)
(defpackage lack.request
  (:use :cl)
  (:import-from :quri
                :url-decode-params)
  (:import-from :http-body
                :parse)
  (:import-from :circular-streams
                :make-circular-input-stream)
  (:import-from :cl-ppcre
                :split)
  (:export :request
           :make-request
           :request-env
           :request-method
           :request-script-name
           :request-path-info
           :request-server-name
           :request-server-port
           :request-server-protocol
           :request-uri
           :request-remote-addr
           :request-remote-port
           :request-query-string
           :request-raw-body
           :request-content-length
           :request-content-type
           :request-headers
           :request-cookies
           :request-body-parameters
           :request-query-parameters
           :request-parameters
           :request-content))
(in-package :lack.request)

(defstruct (request (:constructor %make-request))
  env

  method
  script-name
  path-info
  server-name
  server-port
  server-protocol
  uri
  remote-addr
  remote-port
  query-string
  raw-body
  content-length
  content-type
  headers

  cookies
  body-parameters
  query-parameters)

(defun make-request (env)
  (let ((req (apply #'%make-request :env env :allow-other-keys t env)))
    (with-slots (method uri) req
      (unless method
        (setf method (getf env :request-method)))
      (unless uri
        (setf uri (getf env :request-uri))))

    ;; Cookies
    (unless (request-cookies req)
      (let* ((headers (request-headers req))
             (cookie (and (hash-table-p headers)
                          (gethash "cookie" headers))))
        (when cookie
          (setf (request-cookies req)
                (loop for kv in (ppcre:split "\\s*[,;]\\s*" cookie)
                      append (quri:url-decode-params kv :lenient t)))
          (rplacd (last env) (list :cookies (request-cookies req))))))

    ;; GET parameters
    (with-slots (query-parameters query-string) req
      (when (and (null query-parameters)
                 query-string)
        (setf query-parameters
              (ignore-errors
                (quri:url-decode-params query-string :lenient t)))
        (rplacd (last env) (list :query-parameters query-parameters))))

    (with-slots (body-parameters raw-body content-length content-type) req
      (when raw-body
        (setf raw-body (make-circular-input-stream raw-body))

        ;; POST parameters
        (when (null body-parameters)
          (setf body-parameters
                (http-body:parse content-type content-length raw-body))
          (file-position raw-body 0)
          (rplacd (last env) (list :body-parameters body-parameters)))))

    (setf (request-env req) env)

    req))

(defun request-parameters (req)
  (append (request-query-parameters req)
          (request-body-parameters req)))

(defun request-content (req)
  (let ((raw-body (request-raw-body req)))
    (if (or (request-content-length req)
            (string= (gethash "transfer-encoding" (request-headers req)) "chunked"))
        (prog1
            (http-body.util:slurp-stream raw-body (request-content-length req))
          (file-position raw-body 0))
        #.(make-array 0 :element-type '(unsigned-byte 8)))))
