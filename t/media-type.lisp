(in-package #:cl-user)
(defpackage #:t.lack.media-type
  (:use #:cl
        #:prove
        #:lack.media-type))
(in-package #:t.lack.media-type)

(plan nil)

(defparameter *media-type* (make-media-type "application/json;q=0.8"))

(is (media-type-main-type *media-type*)
    "application")

(is (media-type-sub-type *media-type*)
    "json")

(is (media-type-params *media-type*)
    '(("q" . "0.8")))

(is (match-media-type (make-media-type "*/*;q=0.8")
                      (make-media-type "text/html"))
    t)

(is (match-media-type (make-media-type "application/*")
                      (make-media-type "application/json"))
    t)

(is (match-media-type (make-media-type "application/*")
                      (make-media-type "text/html"))
    nil)

(is (match-media-type (make-media-type "application/json")
                      (make-media-type "application/json"))
    t)

(is (match-media-type (make-media-type "application/json")
                      (make-media-type "application/xml"))
    nil)

(finalize)
