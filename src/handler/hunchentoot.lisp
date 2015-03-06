#|
  This file was ported from Clack project and made some modifications.
  Copyright (c) 2011-2015 Clack contributors
  Copyright (c) 2015 Eitaro Fukamachi <e.arrows@gmail.com>
|#

(in-package :cl-user)
(defpackage lack.handler.hunchentoot
  (:use :cl
        :hunchentoot)
  (:shadow :handle-request)
  (:import-from :hunchentoot
                :acceptor-taskmaster
                :acceptor-shutdown-p)
  (:import-from :flexi-streams
                :make-external-format
                :string-to-octets)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :when-let)
  (:export :run))
(in-package :lack.handler.hunchentoot)

(defun initialize ()
  (setf *hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf)
        *default-content-type* "text/html; charset=utf-8"
        *catch-errors-p* nil))

(defun run (app &key debug (port 5000)
                  ssl ssl-key-file ssl-cert-file ssl-key-password
                  max-thread-count max-accept-count (persistent-connections-p t))
  (initialize)
  (setf *dispatch-table*
        (list
         (let ((stdout *standard-output*)
               (errout *error-output*))
           (lambda (req)
             (let ((env (handle-request req :ssl ssl)))
               (lambda ()
                 (let ((*standard-output* stdout)
                       (*error-output* errout))
                   (handle-response
                    (if debug
                        (funcall app env)
                        (handler-case (funcall app env)
                          (error (error)
                            (princ error *error-output*)
                            '(500 () ("Internal Server Error")))))))))))))
  (let* ((taskmaster (when (and max-thread-count max-accept-count)
                       (make-instance 'one-thread-per-connection-taskmaster
                                      :max-thread-count max-thread-count
                                      :max-accept-count max-accept-count)))
         (acceptor
           (if ssl
               (apply #'make-instance 'easy-ssl-acceptor
                      :port port
                      :ssl-certificate-file ssl-cert-file
                      :ssl-privatekey-file ssl-key-file
                      :ssl-privatekey-password ssl-key-password
                      :access-log-destination nil
                      :persistent-connections-p persistent-connections-p
                      (and taskmaster
                           (list :taskmaster taskmaster)))
               (apply #'make-instance 'easy-acceptor
                      :port port
                      :access-log-destination nil
                      :error-template-directory nil
                      :persistent-connections-p persistent-connections-p
                      (and taskmaster
                           (list :taskmaster taskmaster))))))
    (setf (hunchentoot::acceptor-shutdown-p acceptor) nil)
    (start-listening acceptor)
    (let ((taskmaster (acceptor-taskmaster acceptor)))
      (setf (taskmaster-acceptor taskmaster) acceptor)
      (accept-connections acceptor))))

(defun handle-response (res)
  "Convert Response from Clack application into a string
before passing to Hunchentoot."
  (let ((no-body '#:no-body))
    (flet ((handle-normal-response (res)
             (destructuring-bind (status headers &optional (body no-body)) res
               (setf (return-code*) status)
               (loop for (k v) on headers by #'cddr
                     with hash = (make-hash-table :test #'eq)
                     if (gethash k hash)
                       do (setf (gethash k hash)
                                (format nil "~:[~;~:*~A, ~]~A" (gethash k hash) v))
                     else do (setf (gethash k hash) v)
                     finally
                        (loop for k being the hash-keys in hash
                                using (hash-value v)
                              do (setf (header-out k) v)))

               (when (eq body no-body)
                 (return-from handle-normal-response
                   (let ((out (send-headers)))
                     (lambda (chunk)
                       (if (eq chunk :eof)
                           (finish-output out)
                           (write-sequence
                            (etypecase chunk
                              (string (flex:string-to-octets chunk))
                              ((vector (unsigned-byte 8)) chunk))
                            out))))))

               (etypecase body
                 (null) ;; nothing to response
                 (pathname
                  (hunchentoot:handle-static-file body (getf headers :content-type)))
                 (list
                  (let ((out (send-headers)))
                    (loop for chunk in body
                          do (etypecase chunk
                               (string (write-sequence (flex:string-to-octets chunk) out))
                               ((vector (unsigned-byte 8))
                                (write-sequence chunk out))))
                    (finish-output out)))))))
      (etypecase res
        (list (handle-normal-response res))
        (function (funcall res #'handle-normal-response))))))

(defun handle-request (req &key ssl)
  "Convert Request from server into a plist
before passing to Clack application."
  (destructuring-bind (server-name &optional (server-port "80"))
      (split-sequence #\: (host req) :from-end t)
    (list
     :method (request-method* req)
     :script-name ""
     :path-info (url-decode (script-name* req))
     :server-name server-name
     :server-port (parse-integer server-port :junk-allowed t)
     :server-protocol (server-protocol* req)
     :uri (request-uri* req)
     :url-scheme (if ssl :https :http)
     :remote-addr (remote-addr* req)
     :remote-port (remote-port* req)
     ;; Request params
     :query-string (query-string* req)
     :raw-body (raw-post-data :request req :want-stream t)
     :content-length (when-let (content-length (header-in* :content-length req))
                       (parse-integer content-length :junk-allowed t))
     :content-type (header-in* :content-type req)
     :lack.streaming t

     :headers (loop with headers = (make-hash-table :test 'equal)
                    for (k . v) in (hunchentoot:headers-in* req)
                    unless (or (eq k :content-length)
                               (eq k :content-type))
                      do (setf (gethash (string-downcase k) headers) v)
                    finally (return headers)))))
