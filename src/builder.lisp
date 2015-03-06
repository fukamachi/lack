(in-package :cl-user)
(defpackage lack.builder
  (:use :cl)
  (:import-from :lack.util
                :find-package-or-load)
  (:export :builder))
(in-package :lack.builder)

(defun find-middleware (identifier)
  (let* ((package-name (concatenate 'string
                                    #.(string '#:lack.middleware.)
                                    (symbol-name identifier)))
         (package (find-package-or-load package-name)))
    (unless package
      (error "Middleware ~S is not found" package-name))
    (let ((mw-symbol (intern (format nil "*~A*"
                                     (substitute #\- #\. package-name
                                                 :test #'char=))
                             package)))
      (if (and (boundp mw-symbol)
               (functionp (symbol-value mw-symbol)))
          (symbol-value mw-symbol)
          (error "Middleware ~S is unbound or not a function" mw-symbol)))))

(defmacro builder (&rest app-or-middlewares)
  (let ((middlewares (butlast app-or-middlewares)))
    `(reduce #'funcall
             (list
              ,@(loop for mw in middlewares
                      when mw
                        collect (typecase mw
                                  (function mw)
                                  (keyword `(find-middleware ,mw))
                                  (cons (if (keywordp (car mw))
                                            (let ((app (gensym "APP")))
                                              `(lambda (,app)
                                                 (funcall (find-middleware ,(car mw)) ,app
                                                          ,@(cdr mw))))
                                            mw))
                                  (otherwise mw))))
             :initial-value ,(car (last app-or-middlewares))
             :from-end t)))
