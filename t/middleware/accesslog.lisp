(in-package :cl-user)
(defpackage t.lack.middleware.accesslog
  (:use :cl
        :lack
        :lack.test
        :prove
        :split-sequence))
(in-package :t.lack.middleware.accesslog)

(plan 8)

(defmacro with-accesslogs ((var &rest forms) &body body)
  `(let* ((,var (string-right-trim '(#\Newline)
                                   (with-output-to-string (*standard-output*)
                                     ,@forms)))
          (,var (split-sequence #\Newline ,var)))
     ,@body))

(let ((app1 (builder :accesslog
                    (lambda (env)
                      (declare (ignore env))
                      '(200 () ("ok")))))
      (app2 (builder :accesslog
                    (lambda (env)
                      (declare (ignore env))
                      `(200 () ,(babel:string-to-octets "ok"))))))

  (with-accesslogs (logs (funcall app1 (generate-env "/")))
    (ok logs
        "Body of response is list of strings."))

  (with-accesslogs (logs (funcall app2 (generate-env "/")))
    (ok logs
        "Body of response is (vector (unsigned-byte 8))."))

  (with-accesslogs (logs (funcall app1 (generate-env "/")))
    (is (length logs) 1 "1 line") 
    (like (car logs) "^127.0.0.1 - \\[.+?\\] \"GET / "))

  (with-accesslogs (logs (funcall app1 (generate-env "/"))
                         (funcall app1 (generate-env "/users"))
                         (funcall app1 (generate-env "/new" :method :post)))
    (is (length logs) 3 "3 lines")
    (like (nth 0 logs) "^127.0.0.1 - \\[.+?\\] \"GET / ")
    (like (nth 1 logs) "^127.0.0.1 - \\[.+?\\] \"GET /users ")
    (like (nth 2 logs) "^127.0.0.1 - \\[.+?\\] \"POST /new ")))

(finalize)
