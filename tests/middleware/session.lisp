(defpackage lack/tests/middleware/session
  (:use :cl
        :rove
        :lack
        :lack/test))
(in-package :lack/tests/middleware/session)

(deftest session-middleware
  (ok (lack/session/state:make-state)
      "Base class of session state")
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
      (ok (eql status 200))
      (setf session (parse-lack-session headers))
      (ok session)
      (ok (equalp body '("Hello, you've been here for 1th times!"))))

    (diag "2nd request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (ok (eql status 200))
      (ok (equalp body '("Hello, you've been here for 2th times!"))))))

(deftest session-with-delayed-response
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
                 (ok (eql status 200))
                 (setf session (parse-lack-session headers))
                 (ok session)
                 (ok (equalp body '("Hello, you've been here for 1th times!"))))))

    (diag "2nd request")
    (funcall (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
             (lambda (response)
               (destructuring-bind (status headers body) response
                 (declare (ignore headers))
                 (ok (eql status 200))
                 (ok (equalp body '("Hello, you've been here for 2th times!"))))))))

(deftest set-cookie-header
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
      (ok (eql status 200) "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (setf session
            (ppcre:scan-to-strings "(?<=lack.session=)[^;]+" (getf headers :set-cookie "")))
      (ok (typep session 'string)
          "Set-Cookie header value is valid")
      (ok (equalp body '("hi")) "body"))
    ;; 2nd
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (ok (eql status 200) "status")
      (ng (getf headers :set-cookie)
          "Set-Cookie header doesn't exist")
      (ok (equalp body '("hi")) "body"))
    ;; invalid lack.session
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies '(("lack.session" . "<invalid session here>"))))
      (ok (eql status 200) "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (ok (equalp body '("hi")) "body"))

    ;; expires
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/expire" :cookies `(("lack.session" . ,session))))
      (ok (eql status 200) "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (let ((cookie (cookie:parse-set-cookie-header (getf headers :set-cookie) "" "")))
        (ok (<= (cookie:cookie-expires cookie) (get-universal-time)) "session expired"))
      (ok (equalp body '("hi")) "body"))

    ;; with expired session
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (ok (eql status 200) "status")
      (ok (getf headers :set-cookie)
          "Set-Cookie header exists")
      (let ((cookie (cookie:parse-set-cookie-header (getf headers :set-cookie) "" "")))
        (ok (> (cookie:cookie-expires cookie)
               (get-universal-time))
            "new session is not expired"))
      (ok (equalp body '("hi")) "body")))

  (testing "session expiration with delayed response"
    (let ((app (builder
                :session
                (lambda (env)
                  (if (equal (getf env :path-info) "/delayed-expire")
                      (lambda (responder)
                        (setf (getf (getf env :lack.session.options) :expire) t)
                        (funcall responder '(200 () ("hi"))))
                      (lambda (responder)
                        (funcall responder '(200 () "hi")))))))
          session)
      ;; Get a session.
      (funcall (funcall app (generate-env "/"))
               (lambda (result)
                 (destructuring-bind (status headers body) result
                   (declare (ignore status body))
                   (setf session
                         (ppcre:scan-to-strings "(?<=lack.session=)[^;]+"
                                                (getf headers :set-cookie ""))))))
      ;; Make sure it expires when expiration is set in a delayed response.
      (funcall (funcall app (generate-env "/delayed-expire" :cookies `(("lack.session" . ,session))))
               (lambda (result)
                 (destructuring-bind (status headers body) result
                   (declare (ignore status body))
                   (let ((cookie (cookie:parse-set-cookie-header (getf headers :set-cookie) "" "")))
                     (ok (equal (cookie:cookie-value cookie) session)
                         "Set-Cookie header is for existing session")
                     (ok (<= (cookie:cookie-expires cookie) (get-universal-time))
                         "Session expired")))))))

  (testing ":keep-empty nil"
    (let ((app (builder
                (:session :keep-empty nil)
                (lambda (env)
                  (when (string= (getf env :path-info) "/session")
                    (setf (gethash "user" (getf env :lack.session)) "Eitaro"))
                  '(200 () ("hi"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (declare (ignore status body))
        (ng headers))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/session"))
        (declare (ignore status body))
        (ok (typep (getf headers :set-cookie) 'string)))))

  (testing "cookie-key other than lack.session="
    (let ((app (builder
                (:session :state (lack.session.state.cookie:make-cookie-state
                                  :cookie-key "_myapp_cookie"))
                (lambda (env)
                  (declare (ignore env))
                  '(200 () ("hi"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (declare (ignore status body))
        (ok (ppcre:scan "^_myapp_cookie=" (getf headers :set-cookie)))))))
