(defpackage #:lack/tests/request
  (:use #:cl
        #:rove
        #:lack/request
        #:clack.test
        #:flexi-streams)
  (:import-from #:dexador)
  (:import-from #:alexandria
                #:alist-hash-table))
(in-package #:lack/tests/request)

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
                               ("accept" . "text/html")
                               ("cookie" . "hoge=1;fuga=semi;colon"))
                             :test 'equal))))

(deftest lack-request
  (ok (typep *request* 'request))

  (ok (request-env *request*) "request-env")

  (ok (equal (request-content-type *request*) "application/x-www-form-urlencoded; charset=utf-8")
      "request-content-type")

  (ok (equalp (request-query-parameters *request*)
              '(("ediweitz" . "weitzedi") ("name" . "eitaro") ("q" . "C++")))
      "request-query-parameters")

  (ok (equalp (request-body-parameters *request*)
              `(("name" . ,(flex:octets-to-string
                            #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                            :external-format :utf-8))))
      "request-body-parameters")

  (ok (equalp (request-cookies *request*)
              '(("hoge" . "1") ("fuga" . "semi") ("colon")))
      "request-cookies")

  (ok (request-accepts-p *request* "text/html"))

  (ng (request-accepts-p *request* "application/json"))

  (testing-app "make-request"
      (lambda (env)
        (make-request env)
        `(200 nil (,(third (assoc "file" (request-body-parameters (make-request env)) :test #'string=)))))
    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(asdf:system-relative-pathname :lack #P"data/jellyfish.jpg"))))
      (ok (eql status 200))
      (ok (equal body "jellyfish.jpg")))

    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(asdf:system-relative-pathname :lack #P"data/jellyfish.jpg"))))
      (ok (eql status 200))
      (ok (equal body "jellyfish.jpg")))))
