(in-package :cl-user)

(ql:quickload :hunchentoot)

(defpackage lack-benchmark.hunchentoot
  (:use :cl
        :hunchentoot))
(in-package :lack-benchmark.hunchentoot)

(setf *session-secret* "abcd")

(define-easy-handler (index :uri "/") ()
  (start-session)
  "Hello, World")

(start (make-instance 'easy-acceptor
                      :port 5000
                      :access-log-destination nil
                      :document-root (asdf:system-relative-pathname :lack #P"data/")))
