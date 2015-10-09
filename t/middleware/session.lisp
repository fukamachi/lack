(in-package :cl-user)
(defpackage t.lack.middleware.session
  (:use :cl
        :prove
        :lack
        :lack.test))
(in-package :t.lack.middleware.session)

(plan 5)

(ok (lack.session.state:make-state)
    "Base class of session state")

(subtest "session middleware"
  (let ((app
          (builder
           :session
           (lambda (env)
             (unless (gethash :counter (getf env :lack.session))
               (setf (gethash :counter (getf env :lack.session)) 0))
             `(200
               (:content-type "text/plain")
               (,(format nil "Hello, you've been here for ~Ath times!"
                         (incf (gethash :counter (getf env :lack.session)))))))))
        session)
    (diag "1st request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (is status 200)
      (setf session (parse-lack-session headers))
      (ok session)
      (is body '("Hello, you've been here for 1th times!")))

    (diag "2nd request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (is status 200)
      (is body '("Hello, you've been here for 2th times!")))))

(subtest "session with delayed response"
  (let ((app
          (builder
           :session
           (lambda (env)
             (unless (gethash :counter (getf env :lack.session))
               (setf (gethash :counter (getf env :lack.session)) 0))
             (lambda (responder)
               (funcall responder
                 `(200
                   (:content-type "text/plain")
                   (,(format nil "Hello, you've been here for ~Ath times!"
                             (incf (gethash :counter (getf env :lack.session)))))))))))
        session)
    (diag "1st request")
    (funcall (funcall app (generate-env "/"))
             (lambda (response)
               (destructuring-bind (status headers body) response
                 (is status 200)
                 (setf session (parse-lack-session headers))
                 (ok session)
                 (is body '("Hello, you've been here for 1th times!")))))

    (diag "2nd request")
    (funcall (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
             (lambda (response)
               (destructuring-bind (status headers body) response
                 (declare (ignore headers))
                 (is status 200)
                 (is body '("Hello, you've been here for 2th times!")))))))

(subtest "Set-Cookie header"
  (let ((app (builder
              :session
              (lambda (env)
                (when (string= (getf env :path-info) "/expire")
                  (setf (getf (getf env :lack.session.options) :expire) t))
                '(200 () ("hi")))))
        session)
    ;; 1st
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies '(("lack.session" . nil))))
      (is status 200 "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (setf session
            (ppcre:scan-to-strings "(?<=lack.session=)[^;]+" (getf headers :set-cookie "")))
      (is-type session 'string
               "Set-Cookie header value is valid")
      (is body '("hi") "body"))
    ;; 2nd
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (is status 200 "status")
      (is (getf headers :set-cookie) nil
          "Set-Cookie header doesn't exist")
      (is body '("hi") "body"))
    ;; invalid lack.session
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies '(("lack.session" . "<invalid session here>"))))
      (is status 200 "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (is body '("hi") "body"))

    ;; expires
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/expire" :cookies `(("lack.session" . ,session))))
      (is status 200 "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (let ((cookie (cookie:parse-set-cookie-header (getf headers :set-cookie) "" "")))
        (ok (<= (cookie:cookie-expires cookie) (get-universal-time)) "session expired"))
      (is body '("hi") "body"))

    ;; with expired session
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (is status 200 "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (let ((cookie (cookie:parse-set-cookie-header (getf headers :set-cookie) "" "")))
        (is (cookie:cookie-expires cookie)
            (get-universal-time)
            :test #'>
            "new session is not expired"))
      (is body '("hi") "body"))))

(subtest ":keep-empty nil"
  (let ((app (builder
              (:session :keep-empty nil)
              (lambda (env)
                (when (string= (getf env :path-info) "/session")
                  (setf (gethash "user" (getf env :lack.session)) "Eitaro"))
                '(200 () ("hi"))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (declare (ignore status body))
      (is headers nil))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/session"))
      (declare (ignore status body))
      (is-type (getf headers :set-cookie) 'string))))

(finalize)
