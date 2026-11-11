(defpackage #:lack/app/file
  (:nicknames #:lack.app.file)
  (:use #:cl)
  (:import-from #:lack/component
                #:lack-component
                #:call)
  (:import-from #:trivial-mimes
                #:mime)
  (:import-from #:trivial-rfc-1123
                #:parse-date
                #:as-rfc-1123)
  (:import-from #:uiop
                #:file-exists-p
                #:directory-exists-p)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:lack-app-file
           #:should-handle
           #:serve-path))
(in-package #:lack/app/file)

(define-condition bad-request (simple-condition) ())
(define-condition not-found (simple-condition) ())

(defun escapes-root-p (path)
  (or (find-if (lambda (c) (find c '(#\Null #\\))) path)
      (starts-with-subseq "/" path)
      ;; CCL's native parser expands it to the home directory.
      (starts-with-subseq "~" path)
      (member ".." (uiop:split-string path :separator "/") :test #'string=)))

(defun wild-path-p (path)
  ;; Rejects real files such as "photo[1].jpg" too: where UIOP has no native
  ;; parser and the OS is not Unix, these characters parse as wildcards.
  (find-if (lambda (c) (find c '(#\* #\? #\[ #\]))) path))

(defun plain-relative-pathname-p (pathname)
  "True if PATHNAME is relative and has no drive and no logical host."
  (and (not (typep pathname 'logical-pathname))
       (null (pathname-device pathname))
       (member (first (pathname-directory pathname)) '(nil :relative))))

(defun request-pathname (path)
  "Parse PATH from a request as a relative pathname, or signal an error."
  (when (escapes-root-p path)
    (error 'bad-request))
  (when (wild-path-p path)
    (error 'not-found))
  (let ((pathname (handler-case (uiop:parse-native-namestring path)
                    ;; CCL on Windows reads "x:" as an undefined logical host.
                    (error () (error 'bad-request)))))
    (unless (plain-relative-pathname-p pathname)
      (error 'bad-request))
    pathname))

(defclass lack-app-file (lack-component)
  ((file :initarg :file
         :initform nil)
   (root :initarg :root
         :initform #P"./")
   (encoding :initarg :encoding
             :initform "utf-8")
   (headers :initarg :headers
            :initform '())))

(defmethod call ((app lack-app-file) env)
  (with-slots (file root encoding headers) app
    (handler-case
        (serve-path
          app
          env
          (locate-file app
                       (or file
                           (request-pathname
                             (subseq (getf env :path-info) 1)))
                       root)
          encoding
          headers)
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

(defgeneric serve-path (app env file encoding &optional headers)
  (:method ((app lack-app-file) env file encoding &optional headers)
    (let ((content-type (or (mimes:mime-lookup file)
                            "application/octet-stream"))
          (file-modified-at (or (file-write-date file)
                                (get-universal-time)))
          (if-modified-since (gethash "if-modified-since" (getf env :headers))))
      (when (and if-modified-since
                 (<= file-modified-at (parse-date if-modified-since)))
        (return-from serve-path
                     '(304 () ())))
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
           ,(as-rfc-1123 file-modified-at)
           ,@headers)
          ,file)))))
