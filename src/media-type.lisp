(defpackage lack/media-type
  (:nicknames :lack.media-type)
  (:use :cl)
  (:import-from :quri
                :url-decode-params)
  (:import-from :cl-ppcre
                :split)
  (:export :media-type
           :make-media-type
           :media-type-main-type
           :media-type-sub-type
           :media-type-params
           :match-media-type))
(in-package :lack/media-type)

(defstruct (media-type (:constructor %make-media-type))
  (main-type nil :type (or null string))
  (sub-type nil :type (or null string))
  (params nil :type list))

(defun make-media-type (media-type-string)
  (let* ((media-type-pair (ppcre:split "\\s*[;]\\s*" media-type-string))
         (media-type (ppcre:split "\\s*[/]\\s*" (first media-type-pair)))
         (params (if (second media-type-pair)
                     (quri:url-decode-params (second media-type-pair))
                     nil)))
    (%make-media-type :main-type (first media-type)
                      :sub-type (second media-type)
                      :params params)))

(defun match-media-type (request-media-type other-media-type)
  (with-slots ((request-main-type main-type) (request-sub-type sub-type)) request-media-type
    (with-slots ((other-main-type main-type) (other-sub-type sub-type)) other-media-type
      (cond ((and (string= request-main-type "*")
                  (string= request-sub-type "*"))
             t)
            ((and (string= request-main-type other-main-type)
                  (member request-sub-type (list "*" other-sub-type) :test #'string=))
             t)
            (t nil)))))
