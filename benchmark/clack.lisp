(in-package :cl-user)

(ql:quickload '(:clack :cl-ppcre))

(defpackage lack-benchmark.clack
  (:use :cl
        :clack.middleware.session
        :clack.middleware.static
        :clack.middleware.backtrace))
(in-package :lack-benchmark.clack)

(clack:clackup
 (clack.builder:builder
  (<clack-middleware-static>
   :path (lambda (path)
           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
               path
               nil))
   :root (asdf:system-relative-pathname :lack #P"data/"))
  <clack-middleware-backtrace>
  <clack-middleware-session>
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain; charset=utf-8") ("Hello, World"))))
 :server :hunchentoot
 :debug nil
 :use-thread nil
 :use-default-middlewares nil)
