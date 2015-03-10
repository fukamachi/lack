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
           :funcall-with-cb
           :content-length
           :generate-random-id))
(in-package :lack.util)

(defun find-package-or-load (package-name)
  (check-type package-name string)
  (let ((package (find-package package-name)))
    (or package
        (let ((system-name (string-downcase (substitute #\- #\. package-name :test #'char=))))
          #+quicklisp (ql:quickload system-name)
          #-quicklisp (when (asdf:find-system system-name nil)
                        (asdf:load-system system-name :verbose nil))
          (find-package package-name)))))

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
                      (file-length in)))))))

(defun generate-random-id ()
  "Generates a random token."
  (byte-array-to-hex-string
   (digest-sequence
    (make-digest :SHA1)
    (ascii-string-to-byte-array
     (format nil "~A~A"
      (random 1.0) (get-universal-time))))))
