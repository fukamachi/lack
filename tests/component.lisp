(defpackage lack/tests/component
  (:use :cl
        :lack/component
        :lack/test
        :rove))
(in-package :lack/tests/component)

(defclass myapp (lack-component) ())
(defmethod call ((comp myapp) env)
  (declare (ignore env))
  '(200
    (:content-type "text/plain")
    ("ok from myapp")))

(defvar *fn-app*
  (lambda (env)
    `(200 (:content-type "text/plain") ("ok" ,(getf env :path-info)))))

(deftest lack-component
  (ok (equalp (call *fn-app* (generate-env "/hello"))
              '(200 (:content-type "text/plain") ("ok" "/hello"))))

  (ok (equalp (call (make-instance 'myapp) (generate-env "/"))
              '(200 (:content-type "text/plain") ("ok from myapp"))))

  (ok (typep (to-app *fn-app*) 'function))
  (ok (typep (to-app (make-instance 'myapp)) 'function)))
