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
    ;; session fixation: valid-format but unknown session ID must not be reused
    (let ((attacker-sid "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0"))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/" :cookies `(("lack.session" . ,attacker-sid))))
        (ok (eql status 200) "status")
        (let ((new-sid (ppcre:scan-to-strings "(?<=lack.session=)[^;]+"
                                              (getf headers :set-cookie ""))))
          (ok (getf headers :set-cookie) "Set-Cookie header exists for unknown SID")
          (ok (and (typep new-sid 'string)
                   (string/= new-sid attacker-sid))
              "new session ID must differ from attacker-supplied ID"))
        (ok (equalp body '("hi")) "body")))

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
        (ok (or (null (cookie:cookie-expires cookie))
                (> (cookie:cookie-expires cookie)
                   (get-universal-time)))
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
        (ok (ppcre:scan "^_myapp_cookie=" (getf headers :set-cookie))))))

  (testing "session cookie with :expires nil (browser session cookie)"
    (let ((app (builder
                (:session :state (lack.session.state.cookie:make-cookie-state
                                  :expires nil))
                (lambda (env)
                  (declare (ignore env))
                  '(200 () ("hi"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (declare (ignore status body))
        (let ((set-cookie (getf headers :set-cookie)))
          (ok set-cookie "Set-Cookie header exists")
          (ng (ppcre:scan "(?i)expires=" set-cookie)
              "session cookie with :expires nil should omit expires attribute")
          (let ((cookie (cookie:parse-set-cookie-header set-cookie "" "")))
            (ok (null (cookie:cookie-expires cookie))
                "parsed cookie should have no expires attribute"))))))

  (testing "default session cookie should not expire in the distant future"
    (let ((app (builder
                :session
                (lambda (env)
                  (declare (ignore env))
                  '(200 () ("hi"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (declare (ignore status body))
        (let* ((set-cookie (getf headers :set-cookie))
               (cookie (cookie:parse-set-cookie-header set-cookie "" "")))
          (ok set-cookie "Set-Cookie header exists")
          (if (cookie:cookie-expires cookie)
              (ok (< (- (cookie:cookie-expires cookie) (get-universal-time))
                     (* 60 60 24 365))
                  "cookie expiration must not be more than 1 year in the future")
              (pass "default session cookie has no expires attribute"))))))

  (testing "session cookie with explicit integer :expires"
    (let ((app (builder
                (:session :state (lack.session.state.cookie:make-cookie-state
                                  :expires 86400))
                (lambda (env)
                  (declare (ignore env))
                  '(200 () ("hi"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (declare (ignore status body))
        (let* ((set-cookie (getf headers :set-cookie))
               (cookie (cookie:parse-set-cookie-header set-cookie "" "")))
          (ok set-cookie "Set-Cookie header exists")
          (ok (cookie:cookie-expires cookie)
              "cookie with explicit :expires has an expires attribute")
          (ok (> (cookie:cookie-expires cookie) (get-universal-time))
              "cookie expires in the future")
          (ok (<= (- (cookie:cookie-expires cookie) (get-universal-time)) 86400)
              "cookie expires within 86400 seconds"))))))
