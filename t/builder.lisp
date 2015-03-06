(in-package :cl-user)
(defpackage t.lack.builder
  (:use :cl
        :prove
        :lack.builder))
(in-package :t.lack.builder)

(plan 8)

(defvar *app*
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/html") ("ok from app"))))

(is-type (builder (lambda (app)
                    (lambda (env)
                      (funcall app env)))
                  *app*)
         'function
         "builder")

(is-type (builder
          (if t
              (lambda (app)
                (lambda (env)
                  (funcall app env)))
              nil)
          *app*)
         'function
         "Can embed CL code")

(is-type (builder nil (if t nil nil) nil *app*)
         'function
         "NIL is ignored")

(let ((mount-app (builder
                  (:mount "/admin" (lambda (env) `(200 () ("admin" ,(getf env :path-info)))))
                  *app*)))
  (is (nth 2 (funcall mount-app '(:path-info "/login")))
      '("ok from app"))
  (is (nth 2 (funcall mount-app '(:path-info "/admin/login")))
      '("admin" "/login"))
  (is (nth 2 (funcall mount-app '(:path-info "/admin")))
      '("admin" "/"))
  (is (nth 2 (funcall mount-app '(:path-info "/admin/")))
      '("admin" "/"))
  (is (nth 2 (funcall mount-app '(:path-info "/administrators")))
      '("ok from app")))

(finalize)
