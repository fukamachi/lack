(defpackage lack/tests/session/store/redis
  (:use :cl
        :lack
        :lack/test
        :lack/session/store/redis
        :rove)
  (:import-from :lack/session/store/redis
                :redis-store-connection))
(in-package :lack/tests/session/store/redis)

(defvar *namespace* "session_test")
(defvar *connection*)

(setup
  (setf *connection* (redis-store-connection (make-redis-store)))

  (let ((redis::*connection* *connection*))
    (let ((keys (red:keys (format nil "~A:*" *namespace*))))
      (when keys
        (apply #'red:del keys)))))

(deftest session-middleware
  (let ((app
          (builder
           (:session
            :store (make-redis-store :namespace *namespace* :connection *connection*))
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
      (ok (equalp body '("Hello, you've been here for 2th times!")))))

  (testing "utf-8 session data"
    (let ((app
            (builder
             (:session
              :store (make-redis-store :namespace *namespace* :connection *connection*))
             (lambda (env)
               (unless (gethash :user (getf env :lack.session))
                 (setf (gethash :user (getf env :lack.session)) "深町英太郎"))
               (unless (gethash :counter (getf env :lack.session))
                 (setf (gethash :counter (getf env :lack.session)) 0))
               `(200
                 (:content-type "text/plain")
                 (,(format nil "Hello, ~A! You've been here for ~Ath times!"
                           (gethash :user (getf env :lack.session))
                           (incf (gethash :counter (getf env :lack.session)))))))))
          session)
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (ok (eql status 200))
        (setf session (parse-lack-session headers))
        (ok session)
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 1th times!"))))

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
        (declare (ignore headers))
        (ok (eql status 200))
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 2th times!"))))))

  (testing "expires"
    (let ((app
            (builder
             (:session
              :store (make-redis-store :namespace *namespace* :connection *connection*
                                       :expires 3))
             (lambda (env)
               (unless (gethash :user (getf env :lack.session))
                 (setf (gethash :user (getf env :lack.session)) "深町英太郎"))
               (unless (gethash :counter (getf env :lack.session))
                 (setf (gethash :counter (getf env :lack.session)) 0))
               `(200
                 (:content-type "text/plain")
                 (,(format nil "Hello, ~A! You've been here for ~Ath times!"
                           (gethash :user (getf env :lack.session))
                           (incf (gethash :counter (getf env :lack.session)))))))))
          session)

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (ok (eql status 200))
        (setf session (parse-lack-session headers))
        (ok session)
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 1th times!"))))

      (let ((body (nth 2 (funcall app (generate-env "/" :cookies `(("lack.session" . ,session)))))))
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 2th times!"))))

      (sleep 2)

      (let ((body (nth 2 (funcall app (generate-env "/" :cookies `(("lack.session" . ,session)))))))
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 3th times!"))
            "Still the session is alive"))

      (sleep 2)

      (let ((body (nth 2 (funcall app (generate-env "/" :cookies `(("lack.session" . ,session)))))))
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 4th times!"))
            "Reset the expiration when accessed"))

      (sleep 3.5)

      (let ((body (nth 2 (funcall app (generate-env "/" :cookies `(("lack.session" . ,session)))))))
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 1th times!"))
            "Session has expired after 3 seconds since the last access"))))

  (let ((redis::*connection* *connection*))
    (ok (eql (length (red:keys (format nil "~A:*" *namespace*)))
             3)
        "'session' has three records")))

(teardown
  (redis:close-connection *connection*))
