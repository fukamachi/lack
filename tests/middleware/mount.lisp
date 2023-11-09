(defpackage lack/tests/middleware/mount
  (:use :cl
        :lack
        :lack/test
        :lack/component
        :rove))
(in-package :lack/tests/middleware/mount)

(defclass test-component (lack-component) ())

(defmethod call ((app test-component) env)
  (declare (ignore env))
  '(200 () "mount2"))

(deftest dispatch
  (macrolet ((mount-test (component)
               `(let* ((not-mounted '(200 () ("not-mounted")))
                       (app
                         (builder
                          (:mount "/mount" ,component)
                          (lambda (env)
                            (declare (ignore env))
                            not-mounted))))
                  (let ((expected (funcall (to-app ,component) (generate-env "/")))
                        (result1 (funcall app (generate-env "/mount")))
                        (result2 (funcall app (generate-env "/mount/test")))
                        (result3 (funcall app (generate-env "/test"))))
                    (ok (equal result1 expected)
                        "string=.")

                    (ok (equal result2
                               expected)
                        "subseq.")

                    (ok (equal result3
                               not-mounted)
                        "t.")))))
    (testing "(lambda (env) ...)"
      (mount-test
       (lambda (env)
         (declare (ignore env))
         '(200 () ("mount")))))
    (testing "lack-component"
      (mount-test (make-instance 'test-component)))))

(deftest path-info
  (macrolet ((is-path-info (env expected &optional comment)
               `(ok (equal (getf ,env :path-info)
                           ,expected)
                    ,@(when comment (list comment)))))
  (let* ((response '(200 () ("ok")))
         (app
          (builder
           (:mount "/mount1"
                   (lambda (env)
                     (is-path-info env "/" "string=.")
                     response))
           (:mount "/mount2"
                   (lambda (env)
                     (is-path-info env "/test" "subseq.")
                     response))
           (lambda (env)
             (is-path-info env "/test" "t.")
             response))))
    (dolist (path (list "/mount1" "/mount2/test" "/test"))
      (funcall app (generate-env path))))))
