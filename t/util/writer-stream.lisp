(in-package :cl-user)
(defpackage t-lack.util.writer-stream
  (:use :cl
        :lack.util.writer-stream
        :prove))
(in-package :t-lack.util.writer-stream)

(plan 6)

(let* ((bodies '())
       (writer
         (lambda (body &key &allow-other-keys)
           (push body bodies)))
       (stream (make-writer-stream writer)))
  (is-type stream 'writer-stream)
  (ok (open-stream-p stream))
  (write-sequence #(72 101 108 108 111) stream)
  (write-string "World" stream)
  (is bodies '(#(87 111 114 108 100) #(72 101 108 108 111))
      :test #'equalp)
  (write-char #\! stream)
  (is bodies '(#(33) #(87 111 114 108 100) #(72 101 108 108 111))
      :test #'equalp)
  (ok (open-stream-p stream))
  (finish-output stream)
  (ok (not (open-stream-p stream))))

(finalize)
