(defpackage lack/tests/middleware/accesslog
  (:use :cl
        :lack
        :lack/test
        :rove
        :split-sequence))
(in-package :lack/tests/middleware/accesslog)

(defmacro with-accesslogs ((var &rest forms) &body body)
  `(let* ((,var (string-right-trim '(#\Newline)
                                   (with-output-to-string (*standard-output*)
                                     ,@forms)))
          (,var (split-sequence #\Newline ,var)))
     ,@body))

(deftest accesslog-middleware
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
      (ok (eql (length logs) 1) "1 line")
      (ok (ppcre:scan "^127.0.0.1 - \\[.+?\\] \"GET / "
                      (car logs))))

    (with-accesslogs (logs (funcall app1 (generate-env "/"))
                           (funcall app1 (generate-env "/users"))
                           (funcall app1 (generate-env "/new" :method :post)))
      (ok (eql (length logs) 3) "3 lines")
      (ok (ppcre:scan "^127.0.0.1 - \\[.+?\\] \"GET / " (nth 0 logs)))
      (ok (ppcre:scan "^127.0.0.1 - \\[.+?\\] \"GET /users " (nth 1 logs)))
      (ok (ppcre:scan "^127.0.0.1 - \\[.+?\\] \"POST /new " (nth 2 logs))))

    (with-accesslogs (logs (funcall app1 (generate-env "/"))
                           (funcall app1 (generate-env "/" :headers '(("user-agent" . "Mozilla")))))
      (ok (ppcre:scan "^.+\"-\"$" (nth 0 logs)))
      (ok (ppcre:scan "^.+\"Mozilla\"$" (nth 1 logs))))

    (with-accesslogs (logs (funcall app1 (generate-env "/"))
                           (funcall app1 (generate-env "/" :headers '(("referer" . "http://website.com/index.html")))))
      (ok (ppcre:scan "^.+\"-\" \"-\"$" (nth 0 logs)))
      (ok (ppcre:scan "^.+\"http://website.com/index.html\" \"-\"$" (nth 1 logs))))))
