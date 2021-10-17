(defpackage #:lack.app.file
  (:use #:cl)
  (:import-from #:lack.component
                #:lack-component
                #:call)
  (:import-from #:trivial-mimes
                #:mime)
  (:import-from #:local-time
                #:format-rfc1123-timestring
                #:universal-to-timestamp)
  (:import-from #:uiop
                #:file-exists-p
                #:directory-exists-p)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:lack-app-file
           #:should-handle
           #:serve-path))
(in-package #:lack.app.file)

(define-condition bad-request (simple-condition) ())
(define-condition not-found (simple-condition) ())

(defclass lack-app-file (lack-component)
  ((file :initarg :file
         :initform nil)
   (root :initarg :root
         :initform #P"./")
   (encoding :initarg :encoding
             :initform "utf-8")))

(defmethod call ((app lack-app-file) env)
  (with-slots (file root encoding) app
    (handler-case
        (serve-path
          app
          env
          (locate-file app
                       (or file
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

(defgeneric should-handle (app file)
  (:method ((app lack-app-file) file)
    (and (ignore-errors
           ;; Ignore simple-file-error in a case that
           ;; the file path contains some special characters like "?".
           ;; See https://github.com/fukamachi/clack/issues/111
           (uiop:file-exists-p file))
         (not (uiop:directory-exists-p file)))))

(defgeneric locate-file (app path root)
  (:method ((app lack-app-file) path root)
    (when (find :up (pathname-directory path) :test #'eq)
      (error 'bad-request))

    (let ((file (merge-pathnames path root)))
      (cond
        ((position #\Null (namestring file))
         (error 'bad-request))
        ((not (should-handle app file))
         (error 'not-found))
        (t file)))))

(defgeneric serve-path (app env file encoding)
  (:method ((app lack-app-file) env file encoding)
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
          ,file)))))
