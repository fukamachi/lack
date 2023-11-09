(defpackage lack/tests/util/writer-stream
  (:use :cl
        :lack/util/writer-stream
        :rove))
(in-package :lack/tests/util/writer-stream)

(let* ((bodies '())
       (writer
         (lambda (body &key &allow-other-keys)
           (push body bodies)))
       (stream (make-writer-stream writer)))
  (ok (typep stream 'writer-stream))
  (ok (open-stream-p stream))
  (write-sequence #(72 101 108 108 111) stream)
  (write-string "World" stream)
  (ok (equalp bodies '(#(87 111 114 108 100) #(72 101 108 108 111))))
  (write-char #\! stream)
  (ok (equalp bodies '(#(33) #(87 111 114 108 100) #(72 101 108 108 111))))
  (ok (open-stream-p stream))
  (finish-output stream)
  (ok (not (open-stream-p stream))))
