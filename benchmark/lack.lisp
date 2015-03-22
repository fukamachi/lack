(in-package :cl-user)

(ql:quickload '(:lack :clack))

(defpackage lack-benchmark.lack
  (:use :cl))
(in-package :lack-benchmark.lack)

(clack:lackup
 (lack:builder
  (:static :path (lambda (path)
                   (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
                       path
                       nil))
           :root (asdf:system-relative-pathname :lack #P"data/"))
  :backtrace
  :session
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain; charset=utf-8") ("Hello, World"))))
 :server :hunchentoot
 :debug nil
 :use-thread nil)
