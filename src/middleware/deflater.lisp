(defpackage #:lack/middleware/deflater
  (:use #:cl)
  (:import-from #:lack/util/writer-stream
                #:make-writer-stream)
  (:export #:*lack-middleware-deflater*))
(in-package #:lack/middleware/deflater)

(defparameter *supported-algorithms*
  '("zstd" "gzip"))

(defparameter *default-algorithm* "gzip")

(defun starts-with (prefix str)
  (and (<= (length prefix) (length str))
       (string= prefix str :end2 (length prefix))))

(defun parse-accept-encoding (accept-encoding)
  (let ((wanted-algorithms (ppcre:split "\\s*,\\s*" (string-trim '(#\Space #\Newline #\Tab)
                                                                 accept-encoding))))
    (or (find-if (lambda (alg)
                   (member alg wanted-algorithms :test 'string=))
                 *supported-algorithms*)
        (let ((sorted-algorithms
                (sort
                 (remove nil
                         (mapcar (lambda (alg)
                                   (ppcre:register-groups-bind (alg quality)
                                       ("^(.+);q=([01](?:\\.[0-9]{0,3})?)$" alg)
                                     (when (or (string= "*" alg)
                                               (member alg *supported-algorithms* :test 'string=))
                                       (cons alg (read-from-string quality)))))
                                 wanted-algorithms))
                 #'>
                 :key #'cdr)))
          (loop for (alg . nil) in sorted-algorithms
                if (member alg *supported-algorithms* :test 'string=)
                  do (return alg)
                else if (string= alg "*")
                       do (return *default-algorithm*))))))

(defun compress-octets (alg data)
  (cond
    ((string= "gzip" alg)
     (salza2:compress-data data 'salza2:gzip-compressor))
    ((string= "zstd" alg)
     (zstd:compress-buffer data))
    (t
     (error "Unsupported algorithm: ~S" alg))))

(defun compress-stream (alg input output)
  (cond
    ((string= "gzip" alg)
     (salza2:gzip-stream input output))
    ((string= "zstd" alg)
     (zstd:compress-stream input output))
    (t
     (error "Unsupported algorithm: ~S" alg))))

(defparameter *lack-middleware-deflater*
  (lambda (app &key content-type)
    (let ((test-function
            (if content-type
                (let ((content-types (if (consp content-type)
                                         content-type
                                         (list content-type))))
                  (lambda (res)
                    (destructuring-bind (status headers body) res
                      (declare (ignore status))
                      (let ((res-content-type (or (getf headers :content-type)
                                                  (and (pathnamep body)
                                                       (mimes:mime body)))))
                        (and res-content-type
                             (find-if (lambda (content-type)
                                        (starts-with content-type res-content-type))
                                      content-types))))))
                (constantly t))))
      (lambda (env)
        (block nil
          (let ((accept-encoding (gethash "accept-encoding" (getf env :headers))))
            (unless accept-encoding
              (return (funcall app env)))
            (let ((algorithm (parse-accept-encoding accept-encoding)))
              (unless algorithm
                (return (funcall app env)))
              (let ((res (funcall app env)))
                (unless (funcall test-function res)
                  (return res))
                (destructuring-bind (status headers body)
                    res
                  (remf headers :content-length)
                  (setf (getf headers :vary)
                        (format nil "~@[~A,~]accept-encoding"
                                (getf headers :vary)))
                  (setf (getf headers :content-encoding)
                        algorithm)
                  (etypecase body
                    (pathname
                     (lambda (responder)
                       (let* ((writer (funcall responder (list status headers)))
                              (stream (make-writer-stream writer)))
                         (with-open-file (in body :element-type '(unsigned-byte 8))
                           (compress-stream algorithm in stream))
                         (finish-output stream))))
                    ((vector (unsigned-byte 8))
                     (list status
                           headers
                           (compress-octets algorithm body)))
                    (list
                     (list status
                           headers
                           (compress-octets algorithm
                                            (babel:string-to-octets
                                             (format nil "~{~A~}" body)))))))))))))))
