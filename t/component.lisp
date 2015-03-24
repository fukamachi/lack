(in-package :cl-user)
(defpackage t.lack.component
  (:use :cl
        :lack.component
        :lack.test
        :prove))
(in-package :t.lack.component)

(plan 4)

(defclass myapp (lack-component) ())
(defmethod call ((comp myapp) env)
  (declare (ignore env))
  '(200
    (:content-type "text/plain")
    ("ok from myapp")))

(defvar *fn-app*
  (lambda (env)
    `(200 (:content-type "text/plain") ("ok" ,(getf env :path-info)))))

(is (call *fn-app* (generate-env "/hello"))
    '(200 (:content-type "text/plain") ("ok" "/hello")))

(is (call (make-instance 'myapp) (generate-env "/"))
    '(200 (:content-type "text/plain") ("ok from myapp")))

(is-type (to-app *fn-app*) 'function)
(is-type (to-app (make-instance 'myapp)) 'function)

(finalize)
