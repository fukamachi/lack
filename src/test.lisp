(in-package :cl-user)
(defpackage lack.test
  (:use :cl)
  (:import-from :lack
                :lackup
                :stop)
  (:import-from :prove
                :subtest)
  (:import-from :bordeaux-threads
                :thread-alive-p
                :destroy-thread)
  (:import-from :usocket
                :socket-listen
                :socket-close
                :address-in-use-error)
  (:export :subtest-app
           :localhost
           :*lack-test-handler*
           :*lack-test-port*))
(in-package :lack.test)

(defvar *lack-test-handler* :hunchentoot)
(defvar *lack-test-port* 4242)

(defvar *enable-debug-p* t)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
           (usocket:address-in-use-error () nil))
      (when socket
        (usocket:socket-close socket)
        t))))

(defun localhost (&optional (path "/"))
  (format nil "http://localhost:~D~A"
          *lack-test-port* path))

(defun %subtest-app (desc app client)
  (loop repeat 5
        until (port-available-p *lack-test-port*)
        do (sleep 0.1)
        finally
        (unless (port-available-p *lack-test-port*)
          (error "Port ~D is already in use." *lack-test-port*)))
  (let ((acceptor (lackup app
                          :server *lack-test-handler*
                          :use-thread t
                          :silent t
                          :port *lack-test-port*
                          :debug *enable-debug-p*)))
    (subtest desc
      (sleep 0.5)
      (unwind-protect
           (funcall client)
        (stop acceptor)))))

(defmacro subtest-app (desc app &body client)
  `(%subtest-app ,desc ,app (lambda () ,@client)))
