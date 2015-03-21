(in-package :cl-user)
(defpackage t.lack.middleware.auth.basic
  (:use :cl
        :prove
        :lack
        :lack.test
        :cl-base64))
(in-package :t.lack.middleware.auth.basic)

(plan 2)

(subtest-app "lack-middleware-auth-basic"
    (builder
     (:auth.basic :authenticator (lambda (user pass)
                                   (and (string= user "hoge")
                                        (string= pass "fuga"))))
     (lambda (env)
       `(200 () (,(format nil "Hello, ~A" (getf env :remote-user))))))
  (multiple-value-bind (body status headers)
      (dex:get (localhost))
    (is status 401)
    (is body "Authorization required")
    (is (gethash "www-authenticate" headers)
        "Basic realm=restricted area"))
  (is (dex:get (localhost)
               :headers `(("Authorization" . ,(format nil "Basic ~A"
                                                      (string-to-base64-string "wrong:auth")))))
      "Authorization required")
  (is (dex:get (localhost)
               :headers `(("Authorization" . ,(format nil "Basic ~A"
                                                      (string-to-base64-string "hoge:fuga")))))
      "Hello, hoge"))

(subtest-app "Use :remote-user"
    (builder
     (:auth.basic :authenticator (lambda (user pass)
                                   (when (and (string= user "nitro_idiot")
                                              (string= pass "password"))
                                     (values t "Eitaro Fukamachi"))))
     (lambda (env)
       `(200 () (,(format nil "Hello, ~A" (getf env :remote-user))))))
  (is (dex:get (localhost)) "Authorization required")
  (is (dex:get (localhost)
               :headers `(("Authorization" . ,(format nil "Basic ~A"
                                                      (string-to-base64-string "nitro_idiot:password")))))
      "Hello, Eitaro Fukamachi"))

(finalize)
