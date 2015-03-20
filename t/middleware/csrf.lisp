(in-package :cl-user)
(defpackage t.lack.middleware.csrf
  (:use :cl
        :prove
        :lack
        :lack.request
        :lack.test
        :lack.middleware.csrf
        :cl-cookie))
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

(subtest-app "CSRF middleware"
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
                   (html-form env)))))))
  (let (csrf-token
        (cookie-jar (make-instance 'cookie-jar)))
    (diag "first POST request")
    (is (nth-value 1 (dex:post "http://localhost:4242/"
                               :cookie-jar cookie-jar))
        400)
    (diag "first GET request")
    (multiple-value-bind (body status headers)
        (dex:get "http://localhost:4242/"
                 :cookie-jar cookie-jar)
      (is status 200 "Status is 200")
      (is (gethash "content-type" headers) "text/html" "Content-Type is text/html")
      (setf csrf-token (parse-csrf-token body))
      (ok csrf-token "can get CSRF token")
      (is-type csrf-token 'string "CSRF token is string")
      (is (length csrf-token) 40 "CSRF token is 40 chars"))
    (diag "bad POST request (no token)")
    (multiple-value-bind (body status headers)
        (dex:post "http://localhost:4242/"
                  :cookie-jar cookie-jar)
      (is status 400 "Status is 400")
      (is (gethash "content-type" headers) "text/plain" "Content-Type is text/plain")
      (is body "Bad Request: invalid CSRF token" "Body is 'forbidden'"))
    (diag "bad POST request (wrong token)")
    (is (nth-value
         1
         (dex:post "http://localhost:4242/"
                   :content '(("name" . "Eitaro Fukamachi")
                              ("_csrf_token" . "wrongtokeniknow"))
                   :cookie-jar cookie-jar))
        400)
    (diag "valid POST request")
    (multiple-value-bind (body status headers)
        (dex:post "http://localhost:4242/"
                  :content `(("name" . "Eitaro Fukamachi")
                             ("_csrf_token" . ,csrf-token))
                  :cookie-jar cookie-jar)
      (is status 200 "Status is 200")
      (is (gethash "content-type" headers) "text/html" "Content-Type is text/html")
      (is body "Eitaro Fukamachi" "can read body-parameter"))))

(subtest-app "enable one-time token"
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
                   (html-form env)))))))
  (let (csrf-token
        (cookie-jar (make-instance 'cookie-jar)))
    (setf csrf-token
          (parse-csrf-token
           (dex:get "http://localhost:4242/"
                    :cookie-jar cookie-jar)))
    (dex:post "http://localhost:4242/"
              :content `(("name" . "Eitaro Fukamachi")
                         ("_csrf_token" . ,csrf-token))
              :cookie-jar cookie-jar)
    (diag "bad POST request with before token")
    (multiple-value-bind (body status headers)
        (dex:post "http://localhost:4242/"
                  :content `(("name" . "Eitaro Fukamachi")
                             ("_csrf_token" . ,csrf-token))
                  :cookie-jar cookie-jar)
      (declare (ignore body))
      (is status 400 "Status is 400")
      (is (gethash "content-type" headers) "text/plain" "Content-Type is text/plain"))))

(finalize)
