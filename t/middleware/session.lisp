(in-package :cl-user)
(defpackage t.lack.middleware.session
  (:use :cl
        :prove
        :lack
        :lack.test
        :cl-cookie))
(in-package :t.lack.middleware.session)

(plan nil)

#+thread-support
(subtest-app "session middleware"
    (builder
     :session
     (lambda (env)
       (unless (gethash :counter (getf env :lack.session))
         (setf (gethash :counter (getf env :lack.session)) 0))
       `(200
         (:content-type "text/plain")
         (,(format nil "Hello, you've been here for ~Ath times!"
                   (incf (gethash :counter (getf env :lack.session))))))))
  (let ((cookie-jar (make-cookie-jar)))
    (multiple-value-bind (body status)
        (dex:get (localhost) :cookie-jar cookie-jar :verbose t)
      (diag "1st request")
      (is status 200)
      (is body "Hello, you've been here for 1th times!"))
    (multiple-value-bind (body status)
        (dex:get (localhost) :cookie-jar cookie-jar :verbose t)
      (diag "2nd request")
      (is status 200)
      (is body "Hello, you've been here for 2th times!"))))
#-thread-support
(skip 4 "because your lisp doesn't support threads")

(finalize)
