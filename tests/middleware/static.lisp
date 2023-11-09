(defpackage lack/tests/middleware/static
  (:use :cl
        :rove
        :lack
        :lack/test)
  (:import-from :alexandria
                :starts-with-subseq))
(in-package :lack/tests/middleware/static)

(deftest static-middleware
  (testing ":path is string"
    (let ((app
            (builder
             (:static :path "/public/"
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("Happy Valentine!"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/public/jellyfish.jpg"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "image/jpeg"))
        (ok (equal (namestring body)
                   (namestring (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg")))))

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/public/hoge.png"))
        (declare (ignore headers))
        (ok (eql status 404))
        (ok (equalp body '("Not Found"))))

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "text/plain"))
        (ok (equalp body '("Happy Valentine!"))))))

  (testing ":path is NIL"
    (let ((app
            (builder
             (:static :path nil
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("ok"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/public/jellyfish.jpg"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "text/plain"))
        (ok (equalp body '("ok"))))))

  (testing ":path is function"
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
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "image/jpeg"))
        (ok (equal (namestring body)
                   (namestring (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg")))))

      (ok (eql (car (funcall app (generate-env "/static/not-found.png"))) 404))))

  (testing "special character in path-info"
    (let ((app
            (builder
             (:static :path (lambda (path-info)
                              (when (starts-with-subseq "/static/" path-info)
                                (subseq path-info #.(length "/static"))))
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("ok"))))))
      (ok (eql (first (funcall app (generate-env "/static/?broken=yup"))) 404))
      (ok (eql (first (funcall app (generate-env "/static/%3Fbroken=yup"))) 404)))))
