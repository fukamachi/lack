(in-package #:cl-user)
(defpackage #:t.lack.request
  (:use #:cl
        #:prove
        #:lack.request
        #:clack.test
        #:flexi-streams)
  (:import-from #:dexador)
  (:import-from #:alexandria
                #:alist-hash-table))
(in-package #:t.lack.request)

(plan nil)

(defparameter *request*
  (make-request `(:content-type "application/x-www-form-urlencoded; charset=utf-8"
                  :content-length 20
                  :uri-scheme :http
                  :query-string "ediweitz=weitzedi&name=eitaro&q=C%2B%2B"
                  :raw-body
                  ,(flex:make-flexi-stream
                    (flex:make-in-memory-input-stream
                     #(110 97 109 101 61 230 183 177 231 148 186 232 139 177 229 164 170 233 131 142))
                    :external-format :utf-8)
                  :headers ,(alexandria:alist-hash-table
                             '(("referer" . "http://github.com/fukamachi/clack")
                               ("user-agent" . "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-US)")
                               ("cookie" . "hoge=1;fuga=semi;colon"))
                             :test 'equal))))

(is-type *request* 'request)

(ok (request-env *request*) "request-env")

(is (request-content-type *request*) "application/x-www-form-urlencoded; charset=utf-8"
    "request-content-type")

(is (request-query-parameters *request*)
    '(("ediweitz" . "weitzedi") ("name" . "eitaro") ("q" . "C++"))
    "request-query-parameters")

(is (request-body-parameters *request*)
    `(("name" . ,(flex:octets-to-string
                  #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                  :external-format :utf-8)))
    "request-body-parameters")

(is (request-cookies *request*)
    '(("hoge" . "1") ("fuga" . "semi") ("colon"))
    "request-cookies")

#+thread-support
(subtest-app "make-request"
    (lambda (env)
      (make-request env)
      `(200 nil (,(third (assoc "file" (request-body-parameters (make-request env)) :test #'string=)))))
  (multiple-value-bind (body status)
      (dex:post (localhost)
                :content
                `(("file" . ,(asdf:system-relative-pathname :lack #P"data/jellyfish.jpg"))))
    (is status 200)
    (is body "jellyfish.jpg"))

  (multiple-value-bind (body status)
      (dex:post (localhost)
                :content
                `(("file" . ,(asdf:system-relative-pathname :lack #P"data/jellyfish.jpg"))))
    (is status 200)
    (is body "jellyfish.jpg")))
#-thread-support
(skip 4 "because your lisp doesn't support threads")

(finalize)
