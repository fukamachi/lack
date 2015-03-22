(in-package :cl-user)
(defpackage t.lack.middleware.static
  (:use :cl
        :prove
        :lack
        :lack.test))
(in-package :t.lack.middleware.static)

(plan 1)

(subtest "static middleware"
  (let ((app
          (builder
           (:static :path "/public/"
                    :root (asdf:system-relative-pathname :lack #P"data/"))
           (lambda (env)
             (declare (ignore env))
             `(200 (:content-type "text/plain") ("Happy Valentine!"))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/public/jellyfish.jpg"))
      (is status 200)
      (is (getf headers :content-type) "image/jpeg")
      (is body (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg")))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/public/hoge.png"))
      (declare (ignore headers))
      (is status 404)
      (is body '("Not Found")))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (is status 200)
      (is (getf headers :content-type) "text/plain")
      (is body '("Happy Valentine!")))))

(finalize)
