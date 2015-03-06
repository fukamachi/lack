(in-package :cl-user)
(defpackage lack.handler
  (:use :cl)
  (:import-from :lack.util
                :find-package-or-load)
  (:export :run
           :stop))
(in-package :lack.handler)

(defstruct handler
  server
  acceptor)

(defun find-handler (server)
  (flet ((find-with-prefix (prefix)
           (find-package-or-load (concatenate 'string
                                              prefix
                                              (symbol-name server)))))
    (or (find-with-prefix #.(string '#:lack.handler.))
        (error "~S is unknown handler."
               server))))

(defun run (app server &rest args)
  (let ((handler-package (find-handler server)))
    (make-handler
     :server server
     :acceptor
     (apply (intern #.(string '#:run) handler-package)
            app
            :allow-other-keys t
            args))))

(defun stop (handler)
  (let ((acceptor (handler-acceptor handler)))
    (if (bt:threadp acceptor)
        (progn
          (bt:destroy-thread acceptor)
          (sleep 0.5))
        (let ((package (find-handler (handler-server handler))))
          (funcall (intern #.(string '#:stop) package) acceptor)))
    t))
