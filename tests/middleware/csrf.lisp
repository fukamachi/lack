(defpackage lack/tests/middleware/csrf
  (:use :cl
        :rove
        :lack
        :lack/test
        :lack/request
        :lack/middleware/csrf)
  (:shadowing-import-from :lack/test
                          :request))
(in-package :lack/tests/middleware/csrf)

(defun html-form (env)
  (concatenate
   'string
   "
<html>
<body>
<form name=\"test\" method=\"post\" action=\"/\">
<input name=\"name\" value=\"\" />
"
   (csrf-html-tag (getf env :lack.session))
   "
<input type=\"submit\" />
</form>
</body>
</html>
"))

(defun parse-csrf-token (body &optional (name "_csrf_token"))
  (let ((match (nth-value
                1
                (ppcre:scan-to-strings
                 (concatenate 'string "name=\"" name "\" value=\"(.+?)\"")
                 body))))
    (and match (elt match 0))))

(deftest csrf-middleware
  (testing "CSRF middleware"
    (let ((app
            (builder
             :session
             :csrf
             #'(lambda (env)
                 (let ((req (make-request env)))
                   `(200
                     (:content-type "text/html")
                     (,(if (and (eq :post (request-method req))
                                (assoc "name" (request-body-parameters req) :test #'string=))
                           (cdr (assoc "name" (request-body-parameters req) :test #'string=))
                           (html-form env))))))))
          csrf-token
          session)
      (diag "first POST request")
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/" :method :post))
        (ok (eql status 400))
        (ok (equalp body '("Bad Request: invalid CSRF token")))
        (ok (ppcre:scan
             "^lack.session=.+; path=/; expires="
             (getf headers :set-cookie)))

        (setf session (parse-lack-session headers)))

      (diag "first GET request")
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"
                                     :headers
                                     `(("cookie" . ,(format nil "lack.session=~A" session)))))
        (ok (eql status 200) "Status is 200")
        (ok (ppcre:scan "^text/html" (getf headers :content-type))
            "Content-Type is text/html")
        (setf csrf-token (parse-csrf-token (car body)))
        (ok csrf-token "can get CSRF token")
        (ok (typep csrf-token 'string) "CSRF token is string")
        (ok (eql (length csrf-token) 40) "CSRF token is 40 chars"))

      (diag "bad POST request (no token)")
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"
                                     :method :post
                                     :headers
                                     `(("cookie" . ,(format nil "lack.session=~A" session)))))
        (ok (eql status 400) "Status is 400")
        (ok (ppcre:scan "^text/plain" (getf headers :content-type))
            "Content-Type is text/plain")
        (ok (equalp body '("Bad Request: invalid CSRF token")) "Body is 'forbidden'"))

      (diag "bad POST request (wrong token)")
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"
                                     :method :post
                                     :headers
                                     `(("cookie" . ,(format nil "lack.session=~A" session)))))
        (ok (eql status 400) "Status is 400")
        (ok (ppcre:scan "^text/plain" (getf headers :content-type))
            "Content-Type is text/plain")
        (ok (equalp body '("Bad Request: invalid CSRF token")) "Body is 'forbidden'"))

      (diag "valid POST request")
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"
                                     :method :post
                                     :cookies `(("lack.session" . ,session))
                                     :content
                                     `(("name" . "Eitaro Fukamachi")
                                       ("_csrf_token" . ,csrf-token))))
        (ok (eql status 200) "Status is 200")
        (ok (ppcre:scan "^text/html" (getf headers :content-type))
            "Content-Type is text/html")
        (ok (equalp body '("Eitaro Fukamachi")) "can read body-parameter")))))

(deftest one-time-token
  (let (csrf-token
        session
        (app
          (builder
           :session
           (:csrf :one-time t)
           #'(lambda (env)
               (let ((req (make-request env)))
                 `(200
                   (:content-type "text/html")
                   (,(if (and (eq :post (request-method req))
                              (assoc "name" (request-body-parameters req) :test #'string=))
                         (cdr (assoc "name" (request-body-parameters req) :test #'string=))
                         (html-form env)))))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (declare (ignore status))
      (setf csrf-token (parse-csrf-token (car body)))
      (setf session (parse-lack-session headers)))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :content `(("name" . "Eitaro Fukamachi")
                                              ("_csrf_token" . ,csrf-token))
                                   :cookies `(("lack.session" . ,session))))
      (declare (ignore headers body))
      (ok (eql status 200)))

    (diag "send a request with an expired token")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :content `(("name" . "Eitaro Fukamachi")
                                              ("_csrf_token" . ,csrf-token))
                                   :cookies `(("lack.session" . ,session))))
      (ok (eql status 400))
      (ok (equal (getf headers :content-type) "text/plain"))
      (ok (equalp body '("Bad Request: invalid CSRF token"))))))

(deftest alternate-input-name
  (let (csrf-token
        session
        (app
          (builder
           :session
           (:csrf :form-token "test_input_name")
           #'(lambda (env)
               (let ((req (make-request env)))
                 `(200
                   (:content-type "text/html")
                   (,(if (and (eq :post (request-method req))
                              (assoc "name" (request-body-parameters req) :test #'string=))
                         (cdr (assoc "name" (request-body-parameters req) :test #'string=))
                         (html-form env)))))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (declare (ignore status))
      (setf csrf-token (parse-csrf-token (car body) "test_input_name"))
      (setf session (parse-lack-session headers)))

    (diag "bad POST request (wrong token)")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :content `(("name" . "Eitaro Fukamachi")
                                              ("test_input_name" . "invalid token"))
                                   :cookies `(("lack.session" . ,session))))
      (ok (eql status 400) "Status is 400")
      (ok (ppcre:scan "^text/plain" (getf headers :content-type))
          "Content-Type is text/plain")
      (ok (equalp body '("Bad Request: invalid CSRF token")) "Body is 'forbidden'"))

    (diag "Valid POST request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :content `(("name" . "Eitaro Fukamachi")
                                              ("test_input_name" . ,csrf-token))
                                   :cookies `(("lack.session" . ,session))))
      (declare (ignore headers body))
      (ok (eql status 200)))))
