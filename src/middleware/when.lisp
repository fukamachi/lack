(defpackage #:lack/middleware/when
  (:use #:cl)
  (:import-from #:lack/util
                #:find-middleware)
  (:export #:*lack-middleware-when*))
(in-package #:lack/middleware/when)

(defparameter *lack-middleware-when*
  (lambda (app test middleware)
    (check-type test (or symbol function))
    (check-type middleware (or symbol function))
    (let* ((mw (etypecase middleware
                 (keyword (lack/util:find-middleware middleware))
                 ((or symbol function) middleware)))
           (wrapped-app (funcall mw app)))
      (lambda (env)
        (funcall
         (if (funcall test env)
             wrapped-app
             app)
         env)))))
