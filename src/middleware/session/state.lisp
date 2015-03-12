(in-package :cl-user)
(defpackage lack.middleware.session.state
  (:nicknames :lack.session.state)
  (:use :cl)
  (:import-from :lack.util
                :generate-random-id)
  (:import-from :cl-ppcre
                :scan)
  (:export :state
           :make-state
           :generate-sid
           :extract-sid
           :expire-state
           :finalize-state))
(in-package :lack.middleware.session.state)

(defstruct state
  (sid-generator (lambda (env)
                   (declare (ignore env))
                   (generate-random-id)))
  (sid-validator (lambda (sid)
                   (not (null (ppcre:scan "\\A[0-9a-f]{40}\\Z" sid))))))

(defun generate-sid (state env)
  (funcall (state-sid-generator state) env))

(defgeneric extract-sid (state env)
  (:method ((state state) env)
    (let ((sid (getf env :|lack.session|)))
      (and sid
           (funcall (state-sid-validator state) sid)
           sid))))

(defgeneric expire-state (state sid res options)
  (:method ((state state) sid res options)
    (declare (ignore state sid options))
    res))

(defgeneric finalize-state (state sid res options)
  (:method ((state state) sid res options)
    (declare (ignore state sid options))
    res))
