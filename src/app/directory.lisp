(defpackage #:lack/app/directory
  (:nicknames #:lack.app.directory)
  (:use #:cl)
  (:import-from #:lack/app/file
                #:lack-app-file
                #:should-handle
                #:serve-path)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:trivial-rfc-1123
                #:as-rfc-1123)
  (:import-from #:trivial-mimes
                #:mime-lookup)
  (:import-from #:quri
                #:url-encode)
  (:export #:lack-app-directory))
(in-package #:lack/app/directory)

(defun html-encode (str)
  (ppcre:regex-replace-all
   "([&><\"'])"
   str
   #'(lambda (match &rest regs)
       (declare (ignore regs))
       (cond
         ((string= "&" match) "&amp;")
         ((string= ">" match) "&gt;")
         ((string= "<" match) "&lt;")
         ((string= "\"" match) "&quot;")
         ((string= "'" match) "&#39;")))
   :simple-calls t))

(defun dir-file (file &key uri name)
  (let* ((dir-p (uiop:directory-pathname-p file))
         (uri (or uri
                  (if dir-p
                      (car (last (pathname-directory file)))
                      (file-namestring file)))))
    (format nil "<tr><td class='name'><a href='~A~A'>~A~A</a></td><td class='size'>~:[--~;~:*~:D bytes~]</td><td class='type'>~A</td><td class='mtime'>~A</td></tr>"
            (quri:url-encode uri)
            (if dir-p "/" "")
            (html-encode (or name uri))
            (if dir-p "/" "")
            (unless dir-p
              (with-open-file (in file)
                (file-length in)))
            (if dir-p
                "directory"
                (or (mime-lookup file) "text/plain"))
            (as-rfc-1123 (file-write-date file)))))

(defun dir-page (path-info body)
  (format nil "<html><head>
  <title>Index of ~A</title>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
  <style type='text/css'>
table { width: 100%; }
tr, td { white-space: nowrap; }
.name { text-align: left; }
.size, .mtime { text-align: right; }
.type { width: 11em; text-align: center; }
.mtime { width: 15em; }
  </style>
</head><body>
<h1>Index of ~:*~A</h1>
<hr />
<table>
  <tr>
    <th class='name'>Name</th>
    <th class='size'>Size</th>
    <th class='type'>Type</th>
    <th class='mtime'>Last Modified</th>
  </tr>
~A
</table>
<hr />
</body></html>"
          (html-encode path-info)
          body))

(defun list-directory (dir)
  (sort (nconc (uiop:subdirectories dir) (uiop:directory-files dir))
        #'string<
        :key (lambda (path)
               (if (uiop:directory-pathname-p path)
                   (car (last (pathname-directory path)))
                   (file-namestring path)))))

(defclass lack-app-directory (lack-app-file) ())

(defmethod should-handle ((this lack-app-directory) file)
  (or (uiop:file-exists-p file)
      (uiop:directory-exists-p file)))

(defun index-file-exists-p (path)
  (assert (uiop:directory-pathname-p path))
  (or (uiop:file-exists-p (merge-pathnames #P"index.html" path))
      (uiop:file-exists-p (merge-pathnames #P"index.htm" path))))

(defmethod serve-path ((app lack-app-directory) env file encoding &optional headers)
  (declare (ignore headers))
  (if (uiop:directory-pathname-p file)
      (let ((index-file (index-file-exists-p file)))
        (if index-file
            (call-next-method app env index-file encoding)
            `(200 nil (,(dir-page
                          (getf env :path-info)
                          (format nil "~A~{~A~}"
                                  (dir-file (merge-pathnames "../" file)
                                            :uri ".."
                                            :name "Parent Directory")
                                  (mapcar #'dir-file (list-directory file))))))))
      (call-next-method)))
