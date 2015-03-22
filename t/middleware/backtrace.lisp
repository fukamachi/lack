(in-package :cl-user)
(defpackage t.lack.middleware.backtrace
  (:use :cl
        :lack
        :lack.test
        :prove))
(in-package :t.lack.middleware.backtrace)

(plan 4)

(let ((app
        (builder (:backtrace
                  :result-on-error `(500 (:content-type "text/plain") ("Internal Server Error")))
                 (lambda (env)
                   (if (string= (getf env :path-info) "/error")
                       (error "Error occured")
                       '(200 () ("ok")))))))
  (subtest "normal case"
    (let ((*error-output* (make-string-output-stream)))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (declare (ignore headers))
        (is status 200)
        (is body '("ok")))
      (ok (= 0 (length (get-output-stream-string *error-output*)))
          "No backtraces")))

  (subtest "internal server error"
    (let ((*error-output* (make-string-output-stream)))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/error"))
        (is status 500)
        (is (getf headers :content-type) "text/plain")
        (is body '("Internal Server Error")))
      (ok (< 0 (length (get-output-stream-string *error-output*)))
          "Got backtraces"))))

(define-condition test-error (error) ())
(subtest ":result-on-error is NIL"
  (let ((app
          (builder :backtrace (lambda (env)
                                (declare (ignore env))
                                (error 'test-error)))))
    (let ((*error-output* (make-string-output-stream)))
      (is-error (funcall app (generate-env "/")) 'test-error
                "Raise an error if :result-on-error is NIL")
      (ok (< 0 (length (get-output-stream-string *error-output*)))
          "Got backtraces"))))

(defparameter *test-error-output* (make-string-output-stream))

(subtest "Custom :output"
  (let ((app
          (builder (:backtrace :output '*test-error-output*
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

(finalize)
