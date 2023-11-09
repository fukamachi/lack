(defpackage lack/tests/builder
  (:use :cl
        :rove
        :lack/builder))
(in-package :lack/tests/builder)

(defvar *app*
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/html") ("ok from app"))))

(deftest builder
  (ok (typep
       (builder (lambda (app)
                  (lambda (env)
                    (funcall app env)))
                *app*)
       'function)
      "Inline middleware")
  (ok (typep
       (builder
        (lambda (app)
          (lambda (env)
            (funcall app env)))
        *app*)
       'function)
      "Can embed CL code")
  (ok (typep
       (builder
        (if t
            (lambda (app)
              (lambda (env)
                (funcall app env)))
            nil)
        *app*)
       'function)
      "Can embed CL code")

  (ok (typep
       (builder nil (if t nil nil) nil *app*)
       'function)
      "NIL is ignored"))

(deftest mount
  (let ((mount-app (builder
                    (:mount "/admin" (lambda (env) `(200 () ("admin" ,(getf env :path-info)))))
                    *app*)))
    (ok (equalp (nth 2 (funcall mount-app '(:path-info "/login")))
                '("ok from app")))
    (ok (equalp (nth 2 (funcall mount-app '(:path-info "/admin/login")))
                '("admin" "/login")))
    (ok (equalp (nth 2 (funcall mount-app '(:path-info "/admin")))
                '("admin" "/")))
    (ok (equalp (nth 2 (funcall mount-app '(:path-info "/admin/")))
                '("admin" "/")))
    (ok (equalp (nth 2 (funcall mount-app '(:path-info "/administrators")))
                '("ok from app")))))

(deftest auth-basic
  (ok (builder
       (:auth-basic
        :authenticator (lambda (user pass)
                         (declare (ignore user pass))
                         t))
       *app*))
  (ok (builder
       (:auth/basic
        :authenticator (lambda (user pass)
                         (declare (ignore user pass))
                         t))
       *app*)))

(uiop:define-package lack/middleware/sample
  (:use :cl))

(defvar lack/middleware/sample::*lack-middleware-sample*
  (lambda (app &key (out (lambda (out) (princ out))))
    (lambda (env)
      (funcall out "sample")
      (funcall app env))))

(deftest embedded-code-when
  (testing "Embedded CL code with Middleware without keyword option."
    (let ((app (builder (when t :sample) *app*)))
      (ok (typep app 'function)
          "Can build.")

      (ok (outputs (funcall app '(:path-info "/"))
                   "sample")
          "Can work."))))

(deftest embedded-code-sample
  (testing "Embedded CL code with Middleware with keyword option."
    (let ((app (builder (when t `(:sample :out ,(lambda (out) (format t "Got: ~a" out)))) *app*)))
      (ok (typep app 'function)
          "Can build.")

      (ok (outputs (funcall app '(:path-info "/"))
                   "Got: sample")
          "Can work."))))
