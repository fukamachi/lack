(in-package :cl-user)
(defpackage lack.util
  (:use :cl)
  (:import-from :ironclad
                :ascii-string-to-byte-array
                :byte-array-to-hex-string
                :digest-sequence
                :make-digest)
  (:import-from :alexandria
                :when-let)
  (:export :find-package-or-load
           :find-middleware
           :funcall-with-cb
           :content-length
           :generate-random-id))
(in-package :lack.util)

(defun find-package-or-load (package-name)
  (check-type package-name string)
  (let ((package (find-package package-name)))
    (or package
        (let ((system-name (string-downcase (substitute #\- #\. package-name :test #'char=))))
          #+quicklisp (handler-case (ql:quickload system-name :silent t)
                        (ql:system-not-found ()))
          #-quicklisp (when (asdf:find-system system-name nil)
                        (asdf:load-system system-name :verbose nil))
          (find-package package-name)))))

(defun find-middleware (identifier)
  (let* ((package-name (concatenate 'string
                                    #.(string '#:lack.middleware.)
                                    (substitute #\. #\- (symbol-name identifier))))
         (package (find-package-or-load package-name)))
    (unless package
      (error "Middleware ~S is not found" package-name))
    (let ((mw-symbol (intern (format nil "*~A*"
                                     (substitute #\- #\. package-name
                                                 :test #'char=))
                             package)))
      (if (and (boundp mw-symbol)
               (functionp (symbol-value mw-symbol)))
          (symbol-value mw-symbol)
          (error "Middleware ~S is unbound or not a function" mw-symbol)))))

(defun funcall-with-cb (app env cb)
  (let ((res (funcall app env)))
    (typecase res
      (cons (funcall cb res))
      (function
       (lambda (responder)
        (funcall res (lambda (res)
                       (funcall responder (funcall cb res))))))
      (otherwise res))))

(defun content-length (res)
  (destructuring-bind (status headers &optional body)
      res
    (declare (ignore status))
    (or (getf headers :content-length)
        (etypecase body
          (list (reduce #'+ body :key #'length))
          (pathname (with-open-file (in body)
                      (file-length in)))
          ((vector (unsigned-byte 8))
           (length body))))))

(defun generate-random-id ()
  "Generates a random token."
  (byte-array-to-hex-string
   (digest-sequence
    (make-digest :SHA1)
    (ascii-string-to-byte-array
     (format nil "~A~A"
      (random 1.0) (get-universal-time))))))
