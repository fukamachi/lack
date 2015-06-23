(in-package :cl-user)
(defpackage t.lack.util
  (:use :cl
        :prove
        :lack.util
        :lack.test))
(in-package :t.lack.util)

(plan 4)

(subtest "find-package-or-load"
  (is (find-package-or-load "LACK")
      (find-package :lack))
  (is (find-package-or-load "hoge") nil))

(subtest "funcall-with-cb"
  (let ((cb (lambda (res)
              (rplacd (car (last res)) (list "(ok from cb)"))
              res)))
    ;; cons
    (let ((app (lambda (env)
                 (declare (ignore env))
                 '(200 (:content-type "text/plain") ("ok")))))
      (is (funcall-with-cb app (generate-env "/") cb)
          '(200 (:content-type "text/plain") ("ok" "(ok from cb)"))))
    ;; function
    (let* ((app (lambda (env)
                  (declare (ignore env))
                  (lambda (responder)
                    (funcall responder '(200 (:content-type "text/plain") ("ok"))))))
           (cb-res (funcall-with-cb app (generate-env "/") cb)))
      (is-type cb-res 'function)
      (let (res)
        (funcall cb-res (lambda (r) (setf res r)))
        (is res '(200 (:content-type "text/plain") ("ok" "(ok from cb)")))))
    ;; otherwise
    (let ((app (lambda (env)
                 (declare (ignore env))
                 1)))
      (is (funcall-with-cb app (generate-env "/") cb) 1))))

(subtest "generate-random-id / valid-id-p"
  (is (valid-id-p (generate-random-id))
      t)
  (is (valid-id-p "invalid id")
      nil))

(subtest "hmac-signature"
  (is (hmac-signature "secret" "Hello, World!")
      "fcfaffa7fef86515c7beb6b62d779fa4ccf092f2e61c164376054271252821ff")
  (is (hmac-signature "secret" "Hello, World!" :sha1)
      "883a982dc2ae46d20f7f106c786a9241b60dc340"))

(finalize)
