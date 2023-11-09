(defpackage lack/util
  (:nicknames :lack.util)
  (:use :cl)
  #+(or windows mswindows win32 cormanlisp)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :random-data)
  (:export :find-package-or-load
           :find-middleware
           :funcall-with-cb
           :content-length
           :generate-random-id))
(in-package :lack/util)

(defun locate-symbol (symbol pkg)
  (check-type symbol (or symbol string))
  (let* ((sym-name (if (symbolp symbol) (symbol-name symbol) symbol))
         (sym (find-symbol sym-name pkg)))
    (unless sym
      (error "Unable to find symbol ~A in package ~S." symbol pkg))
    sym))

(defun load-with-quicklisp (system)
  (let ((error-sym (locate-symbol '#:system-not-found '#:ql)))
    ;; We're going to trap on every condition, but only actually
    ;; handle ones of the type we're interested in. Conditions that we
    ;; don't explicitly handle will be propagated normally, because
    ;; HANDLER-BIND is cool like that.
    (handler-bind
        ((t (lambda (c)
              (when (and (typep c error-sym)
                         (string-equal system (uiop:symbol-call :ql :system-not-found-name c)))
                (return-from load-with-quicklisp (values))))))
      (uiop:symbol-call :ql :quickload system :silent t))))

(defun find-package-or-load (package-name)
  (check-type package-name string)
  (let ((package (find-package package-name)))
    (or package
        (let ((system-name (string-downcase (substitute #\- #\. package-name :test #'char=))))
          (if (member :quicklisp *features*)
              (load-with-quicklisp system-name)
              (when (asdf:find-system system-name nil)
                (asdf:load-system system-name :verbose nil)))
          (find-package package-name)))))

(defun find-middleware (identifier)
  (let* ((package-name (concatenate 'string
                                    #.(string '#:lack/middleware/)
                                    (symbol-name identifier)))
         (backward-compatible-package-name
           (concatenate 'string
                        #.(string '#:lack.middleware.)
                        (substitute #\. #\- (symbol-name identifier))))
         (package (or (find-package-or-load package-name)
                      (find-package-or-load backward-compatible-package-name))))
    (unless package
      (error "Middleware ~S is not found" package-name))
    (let ((mw-symbol (intern (format nil "*~A*"
                                     (substitute-if #\-
                                                    (lambda (c)
                                                      (member c '(#\. #\/) :test 'char=))
                                                    package-name))
                             package)))
      (if (and (boundp mw-symbol)
               (functionp (symbol-value mw-symbol)))
          (symbol-value mw-symbol)
          (error "Middleware ~S is unbound or not a function" mw-symbol)))))

(defun funcall-with-cb (app env cb)
  (let ((res (funcall app env)))
    (typecase res
      (cons (funcall cb res))
      (function
       (lambda (responder)
        (funcall res (lambda (res)
                       (funcall responder (funcall cb res))))))
      (otherwise res))))

(defun content-length (res)
  (destructuring-bind (status headers &optional body)
      res
    (declare (ignore status))
    (or (getf headers :content-length)
        (etypecase body
          (list (reduce #'+ body :key #'length))
          (pathname (with-open-file (in body)
                      (file-length in)))
          ((vector (unsigned-byte 8))
           (length body))))))

;; cl-isaac supports ISAAC-64 solely for implementations with x86-64
;; capabilities. Use whichever-best supported capability
#-(or windows mswindows win32 cormanlisp)
(defparameter *isaac-ctx*
  (isaac:init-self-seed :count 5
                        :is64 #+:X86-64 t #-:X86-64 nil))

(defun generate-random-id ()
  "Generates a random token."
  #+(or windows mswindows win32 cormanlisp)
  (ironclad:byte-array-to-hex-string
    (ironclad:random-data 20))
  #-(or windows mswindows win32 cormanlisp)
  (format nil "~(~40,'0x~)" (#+:X86-64 isaac:rand-bits-64
                             #-:X86-64 isaac:rand-bits
                             *isaac-ctx* 160)))
