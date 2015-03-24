(in-package :cl-user)
(defpackage t.lack.builder
  (:use :cl
        :prove
        :lack.builder))
(in-package :t.lack.builder)

(plan 12)

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
          (lambda (app)
            (lambda (env)
              (funcall app env)))
          *app*)
         'function
         "Inline middleware")

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

(defpackage lack.middleware.sample
  (:use :cl))

(defvar lack.middleware.sample::*lack-middleware-sample*
  (lambda (app &key (out (lambda (out) (princ out))))
    (lambda (env)
      (funcall out "sample")
      (funcall app env))))

(subtest "Embedded CL code with Middleware without keyword option."
  (let ((app (builder (when t :sample) *app*)))
    (is-type app
             'function
             "Can build.")

    (is-print (funcall app '(:path-info "/"))
              "sample"
              "Can work.")))

(subtest "Embedded CL code with Middleware with keyword option."
  (let ((app (builder (when t `(:sample :out ,(lambda (out) (format t "Got: ~a" out)))) *app*)))
    (is-type app
             'function
             "Can build.")

    (is-print (funcall app '(:path-info "/"))
              "Got: sample"
              "Can work.")))

(subtest "Old Clack middlewares"
  (let ((app
          (builder
           (clack.middleware.accesslog:<clack-middleware-accesslog>
            :logger (lambda (output) (format t "~&~A~%" output)))
           clack.middleware.session:<clack-middleware-session>
           *app*)))
    (is-type app 'function
             "Can build with old Clack middlewares")))

(finalize)
