(in-package :cl-user)
(defpackage t.lack.middleware.csrf
  (:use :cl
        :prove
        :lack
        :lack.test
        :lack.request
        :lack.middleware.csrf)
  (:shadowing-import-from :lack.test
                          :request))
(in-package :t.lack.middleware.csrf)

(plan 2)

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

(defun parse-csrf-token (body)
  (let ((match (nth-value
                1
                (ppcre:scan-to-strings
                 "name=\"_csrf_token\" value=\"(.+?)\"" body))))
    (and match (elt match 0))))

(subtest "CSRF middleware"
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
      (is status 400)
      (is body '("Bad Request: invalid CSRF token"))
      (like (getf headers :set-cookie)
            "^lack.session=.+; path=/; expires=")

      (setf session (parse-lack-session headers)))

    (diag "first GET request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :headers
                                   `(("cookie" . ,(format nil "lack.session=~A" session)))))
      (is status 200 "Status is 200")
      (like (getf headers :content-type) "^text/html" "Content-Type is text/html")
      (setf csrf-token (parse-csrf-token (car body)))
      (ok csrf-token "can get CSRF token")
      (is-type csrf-token 'string "CSRF token is string")
      (is (length csrf-token) 40 "CSRF token is 40 chars"))

    (diag "bad POST request (no token)")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :headers
                                   `(("cookie" . ,(format nil "lack.session=~A" session)))))
      (is status 400 "Status is 400")
      (like (getf headers :content-type) "^text/plain" "Content-Type is text/plain")
      (is body '("Bad Request: invalid CSRF token") "Body is 'forbidden'"))

    (diag "bad POST request (wrong token)")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :headers
                                   `(("cookie" . ,(format nil "lack.session=~A" session)))))
      (is status 400 "Status is 400")
      (like (getf headers :content-type) "^text/plain" "Content-Type is text/plain")
      (is body '("Bad Request: invalid CSRF token") "Body is 'forbidden'"))

    (diag "valid POST request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :cookies `(("lack.session" . ,session))
                                   :content
                                   `(("name" . "Eitaro Fukamachi")
                                     ("_csrf_token" . ,csrf-token))))
      (is status 200 "Status is 200")
      (like (getf headers :content-type) "^text/html" "Content-Type is text/html")
      (is body '("Eitaro Fukamachi") "can read body-parameter"))))

(subtest "enable one-time token"
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
      (is status 200))

    (diag "send a request with an expired token")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"
                                   :method :post
                                   :content `(("name" . "Eitaro Fukamachi")
                                              ("_csrf_token" . ,csrf-token))
                                   :cookies `(("lack.session" . ,session))))
      (is status 400)
      (is (getf headers :content-type) "text/plain")
      (is body '("Bad Request: invalid CSRF token")))))

(finalize)
