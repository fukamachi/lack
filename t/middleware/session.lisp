(in-package :cl-user)
(defpackage t.lack.middleware.session
  (:use :cl
        :prove
        :lack
        :lack.test)
  (:import-from :lack.session.store.client
                :make-client-store))
(in-package :t.lack.middleware.session)

(plan 4)

(ok (lack.session.state:make-state)
    "Base class of session state")

(defgeneric with-response% (response handler)
  (:method ((response list) handler)
    (apply handler response))
  (:method ((response function) handler)
    (funcall response (lambda (actual-response)
                        (apply handler actual-response)))))

(defmacro with-response ((app env) &body test-body)
  `(with-response% (funcall ,app ,env)
     (lambda (status headers body)
       ,@test-body)))

(defun basic-scenario (app)
  (let (session)
    (diag "1st request")
    (with-response (app (generate-env "/"))
      (is status 200)
      (setf session (parse-lack-session headers))
      (ok session)
      (is body '("Hello, you've been here for 1th times!")))

    (diag "2nd request")
    (with-response (app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (is status 200)
      (ok headers)
      (is body '("Hello, you've been here for 2th times!")))))

(subtest "session middleware"
  (let ((app (builder
              :session
              (lambda (env)
                (unless (gethash :counter (getf env :lack.session))
                  (setf (gethash :counter (getf env :lack.session)) 0))
                `(200
                  (:content-type "text/plain")
                  (,(format nil "Hello, you've been here for ~Ath times!"
                            (incf (gethash :counter (getf env :lack.session))))))))))
    (basic-scenario app)))

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
                             (incf (gethash :counter (getf env :lack.session))))))))))))
    (basic-scenario app)))

(subtest "session middleware with client store"
  (let ((app
          (builder
           (:session :store (make-client-store))
           (lambda (env)
             (unless (gethash :counter (getf env :lack.session))
               (setf (gethash :counter (getf env :lack.session)) 0))
             `(200
               (:content-type "text/plain")
               (,(format nil "Hello, you've been here for ~Ath times!"
                         (incf (gethash :counter (getf env :lack.session))))))))))
    (basic-scenario app)))

(finalize)
