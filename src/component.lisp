(in-package :cl-user)
(defpackage lack.component
  (:use :cl)
  (:export :lack-component
           :call
           :to-app))
(in-package :lack.component)

(defclass lack-component () ())

(defgeneric call (component env)
  (:method ((component function) env)
    (funcall component env)))

(defun to-app (component)
  (if (typep component 'lack-component)
      (lambda (env) (call component env))
      component))
