(in-package :cl-user)
(defpackage lack.util.writer-stream
  (:use :cl)
  (:import-from :trivial-gray-streams
                :fundamental-output-stream
                :stream-write-byte
                :stream-write-sequence
                :stream-write-char
                :stream-write-string
                :stream-element-type
                :stream-finish-output
                :open-stream-p)
  (:import-from :babel
                :string-to-octets)
  (:export :writer-stream
           :make-writer-stream))
(in-package :lack.util.writer-stream)

(defclass writer-stream (fundamental-output-stream)
  ((writer :type function
           :initarg :writer
           :accessor writer-stream-writer)
   (closed-p :type boolean
             :initform nil
             :accessor writer-stream-closed-p)))

(defun make-writer-stream (writer)
  (check-type writer function)
  (make-instance 'writer-stream :writer writer))

(defmethod stream-write-byte ((stream writer-stream) byte)
  (funcall (writer-stream-writer stream)
           (make-array 1 :element-type '(unsigned-byte 8) :initial-contents (list byte))))

(defmethod stream-write-sequence ((stream writer-stream) sequence start end &key)
  (funcall (writer-stream-writer stream) sequence :start start :end end))

(defmethod stream-write-char ((stream writer-stream) char)
  (let ((string (make-string 1 :initial-element char)))
    (funcall (writer-stream-writer stream) (babel:string-to-octets string))))

(defmethod stream-write-string ((stream writer-stream) string &optional (start 0) (end (length string)))
  (funcall (writer-stream-writer stream) (babel:string-to-octets string :start start :end end)))

(defmethod stream-finish-output ((stream writer-stream))
  (funcall (writer-stream-writer stream) nil :close t)
  (setf (writer-stream-closed-p stream) t)
  nil)

(defmethod stream-element-type ((stream writer-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream writer-stream))
  (not (writer-stream-closed-p stream)))
