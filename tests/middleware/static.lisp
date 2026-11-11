(defpackage lack/tests/middleware/static
  (:use :cl
        :rove
        :lack
        :lack/test)
  (:import-from :alexandria
                :starts-with-subseq))
(in-package :lack/tests/middleware/static)

(defun call-with-lack-test-host (fn)
  "Call FN with the logical host LACK-TEST mapped to the lack source tree."
  (setf (logical-pathname-translations "LACK-TEST")
        `(("**;*.*.*" ,(asdf:system-relative-pathname :lack #P"**/*.*"))))
  (unwind-protect (funcall fn)
    (setf (logical-pathname-translations "LACK-TEST") nil)))

(deftest static-middleware
  (testing ":path is string"
    (let ((app
            (builder
             (:static :path "/public/"
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("Happy Valentine!"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/public/jellyfish.jpg"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "image/jpeg"))
        (ok (equal (namestring body)
                   (namestring (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg")))))

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/public/hoge.png"))
        (declare (ignore headers))
        (ok (eql status 404))
        (ok (equalp body '("Not Found"))))

      (testing "a directory is not served"
        (ok (eql (first (funcall app (generate-env "/public/"))) 404))
        (let ((file-app (make-instance 'lack/app/file:lack-app-file
                                       :root (asdf:system-relative-pathname :lack ""))))
          (ok (eql (first (lack/component:call file-app (generate-env "/data"))) 404))
          (ok (eql (first (lack/component:call file-app (generate-env "/data/file.txt"))) 200))))

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "text/plain"))
        (ok (equalp body '("Happy Valentine!"))))))

  (testing ":path is NIL"
    (let ((app
            (builder
             (:static :path nil
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("ok"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/public/jellyfish.jpg"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "text/plain"))
        (ok (equalp body '("ok"))))))

  (testing ":path is function"
    (let ((app
            (builder
             (:static :path (lambda (path-info)
                              (when (starts-with-subseq "/static/" path-info)
                                (subseq path-info #.(length "/static"))))
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("ok"))))))
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/static/jellyfish.jpg"))
        (ok (eql status 200))
        (ok (equal (getf headers :content-type) "image/jpeg"))
        (ok (equal (namestring body)
                   (namestring (asdf:system-relative-pathname :lack #P"data/jellyfish.jpg")))))

      (ok (eql (car (funcall app (generate-env "/static/not-found.png"))) 404)))

    (testing "the function returns a path without the leading slash"
      (let ((app
              (builder
               (:static :path (lambda (path-info)
                                (when (starts-with-subseq "/static" path-info)
                                  (string-left-trim "/" (subseq path-info #.(length "/static")))))
                :root (asdf:system-relative-pathname :lack #P"data/"))
               (lambda (env)
                 (declare (ignore env))
                 `(200 (:content-type "text/plain") ("ok"))))))
        (ok (eql (car (funcall app (generate-env "/static"))) 404))
        (ok (eql (car (funcall app (generate-env "/static/"))) 404))
        (ok (eql (car (funcall app (generate-env "/static/jellyfish.jpg"))) 200)))))

  (testing "special character in path-info"
    (let ((app
            (builder
             (:static :path (lambda (path-info)
                              (when (starts-with-subseq "/static/" path-info)
                                (subseq path-info #.(length "/static"))))
              :root (asdf:system-relative-pathname :lack #P"data/"))
             (lambda (env)
               (declare (ignore env))
               `(200 (:content-type "text/plain") ("ok"))))))
      (ok (eql (first (funcall app (generate-env "/static/?broken=yup"))) 404))
      (ok (eql (first (funcall app (generate-env "/static/%3Fbroken=yup"))) 404))))

  (testing "path traversal"
    (let* ((root (asdf:system-relative-pathname :lack #P"data/"))
           (outside (asdf:system-relative-pathname :lack #P"LICENSE"))
           (app
             (builder
              (:static :path "/public/" :root root)
              (lambda (env)
                (declare (ignore env))
                `(200 (:content-type "text/plain") ("app"))))))
      (labels ((status (path-info)
                 (first (funcall app (generate-env path-info))))
               (rejected-p (path-info)
                 ;; 404 where a logical host is part of a plain file name,
                 ;; 400 where it parses as a host (CCL on Windows).
                 (member (status path-info) '(400 404))))
        (ok (probe-file outside))
        (testing "parent directory"
          (ok (eql (status "/public/../LICENSE") 400)))
        (testing "backslash-escaped parent directory"
          (ok (eql (status "/public/\\.\\./LICENSE") 400))
          (ok (eql (status "/public/\\.\\./\\.\\./lack/LICENSE") 400)))
        (testing "absolute path after the prefix"
          (ok (eql (status (format nil "/public/~A" (namestring outside))) 400)))
        (testing "home directory"
          (ok (eql (status "/public/~/.bashrc") 400))
          (ok (eql (status "/public/~root/.bashrc") 400)))
        (testing "NUL byte"
          (ok (eql (status (format nil "/public/jellyfish.jpg~C" #\Null)) 400)))
        (testing "wildcard characters"
          (ok (eql (status "/public/*.jpg") 404))
          (ok (eql (status "/public/jellyfish.jp[g") 404)))
        (testing "logical pathname host"
          (call-with-lack-test-host
           (lambda ()
             (ok (rejected-p "/public/LACK-TEST:LACK.ASD"))
             (ok (rejected-p "/public/lack-test:lack.asd"))
             (ok (rejected-p "/public/LACK-TEST:"))
             (testing "\";\" as the logical directory separator"
               (ok (rejected-p (format nil "/public/~{~A;~}~A"
                                       (rest (pathname-directory outside))
                                       (file-namestring outside)))))))
          (testing "undefined logical host"
            (ok (rejected-p "/public/NO-SUCH-HOST:x.txt")))
          (ok (rejected-p "/public/SYS:SRC;CODE;TARGET-PATHNAME.LISP"))
          (ok (rejected-p "/public/SYS:")))
        (testing "trailing slash on a file"
          (ok (eql (status "/public/jellyfish.jpg/") 404))
          (ok (eql (status "/public/jellyfish.jpg//") 404))
          (let ((directory-app (make-instance 'lack/app/directory:lack-app-directory
                                              :root root)))
            (ok (eql (first (lack/component:call directory-app
                                                 (generate-env "/jellyfish.jpg/")))
                     404))))
        (testing "files under root are still served"
          (ok (eql (status "/public/jellyfish.jpg") 200))
          (ok (eql (status "/public/./jellyfish.jpg") 200)))))))

(deftest plain-relative-pathname
  (let ((plain-relative-p #'lack/app/file::plain-relative-pathname-p))
    (testing "relative pathnames pass"
      (ok (funcall plain-relative-p #P"jellyfish.jpg"))
      (ok (funcall plain-relative-p
                   (make-pathname :directory '(:relative "dir") :name "a" :type "txt"))))
    (testing "absolute pathname"
      (ng (funcall plain-relative-p #P"/etc/passwd")))
    (testing "Windows drive"
      (ng (funcall plain-relative-p
                   (make-pathname :device "C" :directory '(:absolute "Windows")
                                  :name "win" :type "ini")))
      (ng (funcall plain-relative-p
                   (make-pathname :device "C" :directory '(:relative "Windows")
                                  :name "win" :type "ini"))))
    (testing "logical pathname"
      (call-with-lack-test-host
       (lambda ()
         (ng (funcall plain-relative-p (logical-pathname "LACK-TEST:LACK.ASD"))))))))

(deftest directory-app
  (let ((app (make-instance 'lack/app/directory:lack-app-directory
                            :root (asdf:system-relative-pathname :lack ""))))
    (flet ((request (path-info)
             (lack/component:call app (generate-env path-info))))
      (testing "a directory without the trailing slash redirects"
        (destructuring-bind (status headers body) (request "/data")
          (declare (ignore body))
          (ok (eql status 301))
          (ok (equal (getf headers :location) "data/")))
        (destructuring-bind (status headers body) (request "/data/.")
          (declare (ignore body))
          (ok (eql status 301))
          (ok (equal (getf headers :location) "./"))))
      (testing "a directory with the trailing slash is listed"
        (ok (eql (first (request "/data/")) 200)))
      (testing "a file is served"
        (ok (eql (first (request "/data/file.txt")) 200))))))
