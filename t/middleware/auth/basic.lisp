(in-package :cl-user)
(defpackage t.lack.middleware.auth.basic
  (:use :cl
        :prove
        :lack
        :lack.test
        :cl-base64))
(in-package :t.lack.middleware.auth.basic)

(plan 2)

(subtest "lack-middleware-auth-basic"
  (let ((app
          (builder
           (:auth-basic :authenticator (lambda (user pass)
                                         (and (string= user "hoge")
                                              (string= pass "fuga"))))
           (lambda (env)
             `(200 () (,(format nil "Hello, ~A" (getf env :remote-user))))))))
    (generate-env "/")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (is status 401)
      (is body '("Authorization required"))
      (is (getf headers :www-authenticate) "Basic realm=restricted area"))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("authorization" . ,(format nil "Basic ~A"
                                                                 (string-to-base64-string "wrong:auth"))))) )
      (is status 401)
      (is body '("Authorization required"))
      (is (getf headers :www-authenticate) "Basic realm=restricted area"))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("authorization" . ,(format nil "Basic ~A"
                                                                 (string-to-base64-string "hoge:fuga"))))))
      (declare (ignore headers))
      (is status 200)
      (is body '("Hello, hoge")))))

(subtest "Use :remote-user"
  (let ((app
          (builder
           (:auth-basic :authenticator (lambda (user pass)
                                         (when (and (string= user "nitro_idiot")
                                                    (string= pass "password"))
                                           (values t "Eitaro Fukamachi"))))
           (lambda (env)
             `(200 () (,(format nil "Hello, ~A" (getf env :remote-user))))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (is status 401)
      (is body '("Authorization required"))
      (is (getf headers :www-authenticate) "Basic realm=restricted area"))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("authorization" . ,(format nil "Basic ~A"
                                                                 (string-to-base64-string "nitro_idiot:password"))))))
      (declare (ignore headers))
      (is status 200)
      (is body '("Hello, Eitaro Fukamachi")))))

(finalize)
