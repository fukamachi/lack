(in-package :cl-user)
(defpackage t.lack.middleware.accesslog
  (:use :cl
        :lack
        :lack.test
        :prove
        :split-sequence))
(in-package :t.lack.middleware.accesslog)

(plan 6)

(defmacro with-accesslogs ((var &rest forms) &body body)
  `(let* ((,var (string-right-trim '(#\Newline)
                                   (with-output-to-string (*standard-output*)
                                     ,@forms)))
          (,var (split-sequence #\Newline ,var)))
     ,@body))

(let ((app (builder :accesslog
                    (lambda (env)
                      (declare (ignore env))
                      '(200 () ("ok"))))))
  (with-accesslogs (logs (funcall app (generate-env "/")))
    (is (length logs) 1 "1 line") 
    (like (car logs) "^127.0.0.1 - \\[.+?\\] \"GET / "))

  (with-accesslogs (logs (funcall app (generate-env "/"))
                         (funcall app (generate-env "/users"))
                         (funcall app (generate-env "/new" :method :post)))
    (is (length logs) 3 "3 lines")
    (like (nth 0 logs) "^127.0.0.1 - \\[.+?\\] \"GET / ")
    (like (nth 1 logs) "^127.0.0.1 - \\[.+?\\] \"GET /users ")
    (like (nth 2 logs) "^127.0.0.1 - \\[.+?\\] \"POST /new ")))

(finalize)
