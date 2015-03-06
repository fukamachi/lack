(in-package :cl-user)
(defpackage t.lack.middleware.static
  (:use :cl
        :prove
        :lack
        :lack.test))
(in-package :t.lack.middleware.static)

(defun localhost (path)
  (format nil "http://localhost:~D~A"
          *lack-test-port* path))

(plan nil)

#+thread-support
(subtest-app "static middleware"
    (builder
     (:static :path "/public/"
              :root (asdf:system-relative-pathname :lack #P"data/"))
     (lambda (env)
       (declare (ignore env))
       `(200 (:content-type "text/plain") ("Happy Valentine!"))))
  (multiple-value-bind (body status headers)
      (dex:get (localhost "/public/jellyfish.jpg"))
    (is status 200)
    (is (gethash "content-type" headers) "image/jpeg")
    (is (length body) 139616))
  (multiple-value-bind (body status)
      (dex:get (localhost "/public/hoge.png"))
    (is status 404)
    (is body "Not Found"))
  (multiple-value-bind (body status headers)
      (dex:get (localhost "/"))
    (is status 200)
    (is (gethash "content-type" headers) "text/plain")
    (is body "Happy Valentine!")))
#-thread-support
(skip 1 "because your lisp doesn't support threads")

(finalize)
