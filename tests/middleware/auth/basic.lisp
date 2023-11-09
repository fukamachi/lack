(defpackage lack/tests/middleware/auth/basic
  (:use :cl
        :rove
        :lack
        :lack/test
        :cl-base64))
(in-package :lack/tests/middleware/auth/basic)

(deftest lack-middleware-auth-basic
  (let ((app
          (builder
           (:auth/basic :authenticator (lambda (user pass)
                                         (and (string= user "hoge")
                                              (string= pass "fuga"))))
           (lambda (env)
             `(200 () (,(format nil "Hello, ~A" (getf env :remote-user))))))))
    (generate-env "/")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (ok (eql status 401))
      (ok (equalp body '("Authorization required")))
      (ok (equal (getf headers :www-authenticate) "Basic realm=restricted area")))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("authorization" . ,(format nil "Basic ~A"
                                                                 (string-to-base64-string "wrong:auth"))))) )
      (ok (eql status 401))
      (ok (equalp body '("Authorization required")))
      (ok (equal (getf headers :www-authenticate) "Basic realm=restricted area")))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("authorization" . ,(format nil "Basic ~A"
                                                                 (string-to-base64-string "hoge:fuga"))))))
      (declare (ignore headers))
      (ok (eql status 200))
      (ok (equalp body '("Hello, hoge"))))))

(deftest remote-user
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
      (ok (eql status 401))
      (ok (equalp body '("Authorization required")))
      (ok (equal (getf headers :www-authenticate) "Basic realm=restricted area")))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("authorization" . ,(format nil "Basic ~A"
                                                                 (string-to-base64-string "nitro_idiot:password"))))))
      (declare (ignore headers))
      (ok (eql status 200))
      (ok (equalp body '("Hello, Eitaro Fukamachi"))))))
