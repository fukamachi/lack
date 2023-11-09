(defpackage lack/tests/util
  (:use :cl
        :rove
        :lack/util
        :lack/test))
(in-package :lack/tests/util)

(deftest find-package-or-load
  (ok (eq (find-package-or-load "LACK")
          (find-package :lack)))
  (ok (eq (find-package-or-load "hoge") nil)))

(deftest funcall-with-cb
  (let ((cb (lambda (res)
              (rplacd (car (last res)) (list "(ok from cb)"))
              res)))
    ;; cons
    (let ((app (lambda (env)
                 (declare (ignore env))
                 '(200 (:content-type "text/plain") ("ok")))))
      (ok (equalp (funcall-with-cb app (generate-env "/") cb)
                  '(200 (:content-type "text/plain") ("ok" "(ok from cb)")))))
    ;; function
    (let* ((app (lambda (env)
                  (declare (ignore env))
                  (lambda (responder)
                    (funcall responder '(200 (:content-type "text/plain") ("ok"))))))
           (cb-res (funcall-with-cb app (generate-env "/") cb)))
      (ok (typep cb-res 'function))
      (let (res)
        (funcall cb-res (lambda (r) (setf res r)))
        (ok (equalp res '(200 (:content-type "text/plain") ("ok" "(ok from cb)"))))))
    ;; otherwise
    (let ((app (lambda (env)
                 (declare (ignore env))
                 1)))
      (ok (eql (funcall-with-cb app (generate-env "/") cb) 1)))))
