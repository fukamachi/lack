(defpackage #:lack/tests/media-type
  (:use #:cl
        #:rove
        #:lack/media-type))
(in-package #:lack/tests/media-type)

(defparameter *media-type* (make-media-type "application/json;q=0.8"))

(deftest media-type
  (ok (equal (media-type-main-type *media-type*)
             "application"))

  (ok (equal (media-type-sub-type *media-type*)
             "json"))

  (ok (equalp (media-type-params *media-type*)
              '(("q" . "0.8"))))

  (ok (match-media-type (make-media-type "*/*;q=0.8")
                        (make-media-type "text/html")))

  (ok (match-media-type (make-media-type "application/*")
                        (make-media-type "application/json")))

  (ng (match-media-type (make-media-type "application/*")
                        (make-media-type "text/html")))

  (ok (match-media-type (make-media-type "application/json")
                        (make-media-type "application/json")))

  (ng (match-media-type (make-media-type "application/json")
                        (make-media-type "application/xml"))))
