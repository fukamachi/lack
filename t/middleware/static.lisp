(in-package :cl-user)
(defpackage t.lack.middleware.static
  (:use :cl
        :prove
        :lack
        :lack.test)
  (:import-from :alexandria
                :starts-with-subseq))
(in-package :t.lack.middleware.static)

(plan 4)

(subtest ":path is string"
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
      (is (namestring body)
          (namestring (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg"))))

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

(subtest ":path is NIL"
  (let ((app
          (builder
           (:static :path nil
                    :root (asdf:system-relative-pathname :lack #P"data/"))
           (lambda (env)
             (declare (ignore env))
             `(200 (:content-type "text/plain") ("ok"))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/public/jellyfish.jpg"))
      (is status 200)
      (is (getf headers :content-type) "text/plain")
      (is body '("ok")))))

(subtest ":path is function"
  (let ((app
          (builder
           (:static :path (lambda (path-info)
                            (when (starts-with-subseq "/static/" path-info)
                              (subseq path-info #.(length "/static"))))
                    :root (asdf:system-relative-pathname :lack #P"data/"))
           (lambda (env)
             (declare (ignore env))
             `(200 (:content-type "text/plain") ("ok"))))))
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/static/jellyfish.jpg"))
      (is status 200)
      (is (getf headers :content-type) "image/jpeg")
      (is (namestring body)
          (namestring (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg"))))

    (is (car (funcall app (generate-env "/static/not-found.png"))) 404)))

(subtest "special character in path-info"
  (let ((app
          (builder
           (:static :path (lambda (path-info)
                            (when (starts-with-subseq "/static/" path-info)
                              (subseq path-info #.(length "/static"))))
                    :root (asdf:system-relative-pathname :lack #P"data/"))
           (lambda (env)
             (declare (ignore env))
             `(200 (:content-type "text/plain") ("ok"))))))
    (is (first (funcall app (generate-env "/static/?broken=yup"))) 404)
    (is (first (funcall app (generate-env "/static/%3Fbroken=yup"))) 404)))

(finalize)
