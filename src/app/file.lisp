(in-package :cl-user)
(defpackage lack.app.file
  (:use :cl)
  (:import-from :trivial-mimes
                :mime)
  (:import-from :local-time
                :format-rfc1123-timestring
                :universal-to-timestamp)
  (:import-from :uiop
                :file-exists-p
                :directory-exists-p)
  (:import-from :alexandria
                :starts-with-subseq)
  (:export :make-app))
(in-package :lack.app.file)

(define-condition bad-request (simple-condition) ())
(define-condition not-found (simple-condition) ())

(defun make-app (&key file (root #P"./") (encoding "utf-8"))
  (lambda (env)
    (handler-case
        (serve-path
         (locate-file (or file
                          ;; remove "/"
                          (subseq (getf env :path-info) 1))
                      root)
         encoding)
      (bad-request ()
        '(400 (:content-type "text/plain"
               :content-length 11)
          ("Bad Request")))
      (not-found ()
        '(404 (:content-type "text/plain"
               :content-length 9)
          ("Not Found"))))))

(defun locate-file (path root)
  (when (find :up (pathname-directory path) :test #'eq)
    (error 'bad-request))

  (let ((file (merge-pathnames path root)))
    (cond
      ((position #\Null (namestring file))
       (error 'bad-request))
      ((not (and (ignore-errors
                  ;; Ignore simple-file-error in a case that
                  ;; the file path contains some special characters like "?".
                  ;; See https://github.com/fukamachi/clack/issues/111
                  (uiop:file-exists-p file))
                 (not (uiop:directory-exists-p file))))
       (error 'not-found))
      (t file))))

(defun serve-path (file encoding)
  (let ((content-type (or (mimes:mime-lookup file)
                          "application/octet-stream"))
        (univ-time (or (file-write-date file)
                       (get-universal-time))))
    (when (starts-with-subseq "text" content-type)
      (setf content-type
            (format nil "~A~:[~;~:*; charset=~A~]"
                    content-type encoding)))
    (with-open-file (stream file
                            :direction :input
                            :if-does-not-exist nil)
      `(200
        (:content-type ,content-type
         :content-length ,(file-length stream)
         :last-modified
         ,(format-rfc1123-timestring nil
                                     (universal-to-timestamp univ-time)))
        ,file))))
