(defpackage lack/tests/middleware/backtrace
  (:use :cl
        :lack
        :lack/test
        :rove))
(in-package :lack/tests/middleware/backtrace)

(deftest backtrace-middleware
  (let ((app
          (builder (:backtrace
                    :result-on-error `(500 (:content-type "text/plain") ("Internal Server Error")))
                   (lambda (env)
                     (if (string= (getf env :path-info) "/error")
                         (error "Error occured")
                         '(200 () ("ok")))))))
    (testing "normal case"
      (let ((*error-output* (make-string-output-stream)))
        (destructuring-bind (status headers body)
            (funcall app (generate-env "/"))
          (declare (ignore headers))
          (ok (eql status 200))
          (ok (equalp body '("ok"))))
        (ok (= 0 (length (get-output-stream-string *error-output*)))
            "No backtraces")))

    (testing "internal server error"
      (let ((*error-output* (make-string-output-stream)))
        (destructuring-bind (status headers body)
            (funcall app (generate-env "/error"))
          (ok (eql status 500))
          (ok (equal (getf headers :content-type) "text/plain"))
          (ok (equalp body '("Internal Server Error"))))
        (ok (< 0 (length (get-output-stream-string *error-output*)))
            "Got backtraces")))))

(define-condition test-error (error) ())

(deftest result-on-error
  (testing ":result-on-error is NIL"
    (let ((app
            (builder :backtrace (lambda (env)
                                  (declare (ignore env))
                                  (error 'test-error)))))
      (let ((*error-output* (make-string-output-stream)))
        (ok (signals (funcall app (generate-env "/")) 'test-error)
            "Raise an error if :result-on-error is NIL")
        (ok (< 0 (length (get-output-stream-string *error-output*)))
            "Got backtraces"))))

  (testing ":result-on-error is function"
    (let ((app
            (builder (:backtrace
                      :result-on-error (lambda (condition)
                                         (if (typep condition 'test-error)
                                             '(503 (:content-type "text/plain") ("Service Temporary Unavailable"))
                                             '(500 (:content-type "text/plain") ("Internal Server Error")))))
                     (lambda (env)
                       (if (string= (getf env :path-info) "/503")
                           (error 'test-error)
                           (error "Error occured")))))
          (*error-output* (make-broadcast-stream)))
      (ok (equalp (funcall app (generate-env "/"))
                  '(500 (:content-type "text/plain") ("Internal Server Error"))))
      (ok (equalp (funcall app (generate-env "/503"))
                  '(503 (:content-type "text/plain") ("Service Temporary Unavailable")))))))

(defparameter *test-error-output* (make-string-output-stream))

(deftest custom-output
  (testing "Custom :output (stream)"
    (let ((app
            (builder (:backtrace :output *test-error-output*
                      :result-on-error '(500 (:content-type "text/plain") ("Internal Server Error")))
                     (lambda (env)
                       (declare (ignore env))
                       (error "Error occured")))))
      (let ((*error-output* (make-string-output-stream)))
        (funcall app (generate-env "/"))
        (ok (= 0 (length (get-output-stream-string *error-output*)))
            "Don't output to *error-output*")
        (ok (< 0 (length (get-output-stream-string *test-error-output*)))
            "Output to the custom :output"))))

  (testing "Custom :output (pathname)"
    (let* ((log-file (asdf:system-relative-pathname :lack #P"data/test.log"))
           (app
             (builder (:backtrace :output log-file
                       :result-on-error '(500 (:content-type "text/plain") ("Internal Server Error")))
                      (lambda (env)
                        (declare (ignore env))
                        (error "Error occured")))))
      (when (probe-file log-file)
        (delete-file log-file))
      (let ((*error-output* (make-string-output-stream)))
        (funcall app (generate-env "/"))
        (ok (= 0 (length (get-output-stream-string *error-output*)))
            "Don't output to *error-output*")
        (ok (< 0 (length (alexandria:read-file-into-string log-file)))
            "Output to the custom :output")))))
