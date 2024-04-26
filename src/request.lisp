(defpackage lack/request
  (:nicknames :lack.request)
  (:use :cl)
  (:import-from :quri
                :url-decode-params)
  (:import-from :http-body
                :parse)
  (:import-from :circular-streams
                :circular-input-stream
                :make-circular-input-stream)
  (:import-from :cl-ppcre
                :split)
  (:import-from :lack/media-type
                :media-type
                :make-media-type
                :match-media-type)
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
           :request-uri-scheme
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
           :request-content
           :request-has-body-p
           :request-accept
           :request-accepts-p))
(in-package :lack/request)

(defstruct (request (:constructor %make-request))
  (env nil :type list)

  (method nil :type (or null keyword))
  (script-name nil :type (or null string))
  (path-info nil :type (or null string))
  (server-name nil :type (or null string))
  (server-port nil :type (or null integer))
  (server-protocol nil :type (or null keyword))
  (uri nil :type (or null string))
  (uri-scheme nil :type (or null string keyword))
  (remote-addr nil :type (or null string))
  (remote-port nil :type (or null keyword integer))
  (query-string nil :type (or null string))
  (raw-body nil :type (or null stream))
  (content-length nil :type (or null integer))
  (content-type nil :type (or null string))
  (headers nil :type (or null hash-table))

  (cookies nil :type list)
  (body-parameters nil :type list)
  (query-parameters nil :type list)
  (accept nil :type list))

(declaim (inline request-has-body-p))
(defun request-has-body-p (req)
  (or (request-content-length req)
      (string= (gethash "transfer-encoding" (request-headers req)) "chunked")))

(defun make-request (env)
  (let ((req (apply #'%make-request :env env :allow-other-keys t env)))
    (with-slots (method uri uri-scheme) req
      (unless method
        (setf method (getf env :request-method)))
      (unless uri
        (setf uri (getf env :request-uri)))
      (unless uri-scheme
        ;; for some reason, it is called url-scheme in the environment plist :(
        (setf uri-scheme (getf env :url-scheme))))

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

    (with-slots (body-parameters raw-body content-length content-type) req
      (when raw-body
        (unless (typep raw-body 'circular-input-stream)
          (setf raw-body (make-circular-input-stream raw-body)))

        ;; POST parameters
        (when (and (null body-parameters)
                   (request-has-body-p req)
                   (stringp content-type))
          (let ((parsed (http-body:parse content-type content-length raw-body)))
            (when (and (consp parsed)
                       (every #'consp parsed))
              (setf body-parameters parsed)))
          (file-position raw-body 0)
          (setf (getf env :raw-body) raw-body)
          (rplacd (last env) (list :body-parameters body-parameters)))))

    (setf (request-accept req)
          (mapcar #'lack/media-type:make-media-type (ppcre:split "\\s*[,]\\s*" (gethash "accept" (request-headers req)))))

    (setf (request-env req) env)

    req))

(defun request-parameters (req)
  (append (request-query-parameters req)
          (request-body-parameters req)))

(defun request-content (req)
  (if (request-has-body-p req)
      (let ((raw-body (request-raw-body req)))
        (prog1
            (http-body.util:slurp-stream raw-body (request-content-length req))
          (file-position raw-body 0)))
      #.(make-array 0 :element-type '(unsigned-byte 8))))

(defun request-accepts-p (request media-type-string)
  "Attempt to match media-type string against the values in the request ACCEPT header"
  (let ((media-type-obj (lack/media-type:make-media-type media-type-string)))
    (some #'(lambda (request-media-type)
              (lack/media-type:match-media-type request-media-type media-type-obj))
          (request-accept request))))

(declaim (notinline request-has-body-p))
