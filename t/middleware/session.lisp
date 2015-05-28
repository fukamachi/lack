(in-package :cl-user)
(defpackage t.lack.middleware.session
  (:use :cl
        :prove
        :lack
        :lack.test))
(in-package :t.lack.middleware.session)

(plan 3)

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

(finalize)
