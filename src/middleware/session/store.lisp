(defpackage lack/middleware/session/store
  (:nicknames :lack.middleware.session.store
              :lack.session.store
              :lack/session/store)
  (:use :cl)
  (:export :store
           :fetch-session
           :store-session
           :remove-session
           :safe-read-from-string))
(in-package :lack/middleware/session/store)

(defstruct store)

(defgeneric fetch-session (store sid))
(defgeneric store-session (store sid session))
(defgeneric remove-session (store sid))

(defun safe-read-from-string (string)
  "Read STRING with *read-eval* disabled and several dangerous reader macros
   overridden to signal an error, preventing code execution and DoS via the reader."
  (when (or (null string)
            (zerop (length (string-trim '(#\Space #\Tab #\Newline) string))))
    (return-from safe-read-from-string nil))
  (let ((*read-eval* nil)
        (*readtable* (copy-readtable)))
    (set-dispatch-macro-character #\# #\.
      (lambda (stream char n)
        (declare (ignore stream char n))
        (error "#. is not allowed in session data")))
    (set-dispatch-macro-character #\# #\=
      (lambda (stream char n)
        (declare (ignore stream char n))
        (error "Circular-structure label (#=) is not allowed in session data")))
    (set-dispatch-macro-character #\# #\#
      (lambda (stream char n)
        (declare (ignore stream char n))
        (error "Circular-structure reference (##) is not allowed in session data")))
    (read-from-string string)))
