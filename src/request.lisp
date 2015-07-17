(in-package :cl-user)
(defpackage lack.request
  (:use :cl)
  (:import-from :alexandria
                :when-let*)
  (:import-from :quri
                :url-decode-params)
  (:import-from :http-body
                :parse)
  (:import-from :http-body.util
                :slurp-stream)
  (:import-from :split-sequence
                :split-sequence)
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
           :request-parameters))
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

(defun handle-multipart-body (body)
  (let ((encoding (babel-encodings:get-character-encoding :utf-8)))
    (labels ((strip (str)
               (string-trim '(#\Space #\Tab #\Newline) str))
             (handle-body (encoding name body)
               (if (gethash "filename" (cadr body) nil)
                   (cons name body)
                   (progn
                     (when-let* ((contentType
                                  (gethash "content-type" (caddr body) nil))
                                 (content-type-list
                                  (split-sequence #\; contentType))
                                 (type-subtype
                                  (car content-type-list)))
                       (when (string= type-subtype "text/" :end1 5)
                         (loop
                            for param in (split-sequence #\=
                                                         (cdr content-type-list))
                            for key = (strip (car param))
                            for val = (strip (cadr param))
                            do
                              (when (string= key "charset")
                                (setf encoding
                                      (babel-encodings:get-character-encoding
                                       val))
                                (return nil)))))
                     (cons name
                           (cons (babel:octets-to-string
                                  (slurp-stream (car body)) :encoding encoding)
                                 (cdr body)))))))
      (loop
         for pair in body
         for key = (car pair)
         for val = (cdr pair)
         collect
           (handle-body encoding key val)))))


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
              (quri:url-decode-params query-string :lenient t))
        (rplacd (last env) (list :query-parameters query-parameters))))

    ;; POST parameters
    (with-slots (body-parameters raw-body content-length content-type) req
      (when (and (null body-parameters)
                 raw-body)
        (let ((parsed-body
               (http-body:parse content-type content-length raw-body)))
          (if (string-equal content-type "multipart/form-data" :end1
                            (length "multipart/form-data"))
              (setf body-parameters (handle-multipart-body parsed-body))
              (setf body-parameters parsed-body)))
        (rplacd (last env) (list :body-parameters body-parameters))))
    (setf (request-env req) env)
    req))

(defun request-parameters (req)
  (append (request-query-parameters req)
          (request-body-parameters req)))
