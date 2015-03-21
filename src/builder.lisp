(in-package :cl-user)
(defpackage lack.builder
  (:use :cl)
  (:import-from :lack.component
                :to-app)
  (:import-from :lack.util
                :find-package-or-load)
  (:export :builder))
(in-package :lack.builder)

(defun find-middleware (identifier)
  (let* ((package-name (concatenate 'string
                                    #.(string '#:lack.middleware.)
                                    (substitute #\. #\- (symbol-name identifier))))
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

(defun clack-middleware-symbol-p (symbol)
  (and (symbolp symbol)
       (find-package :clack.middleware)
       (find-class symbol nil)
       (subtypep (find-class symbol)
                 (intern (string :<middleware>)
                         :clack.middleware))))

(defun convert-to-middleware-form (mw)
  (let ((app (gensym "APP")))
    (typecase mw
      (null)
      (function mw)
      (keyword `(find-middleware ,mw))
      ;; for old Clack middlewares
      (symbol (if (clack-middleware-symbol-p mw)
                  `(lambda (,app)
                     (funcall (intern (string :wrap) :clack)
                              (make-instance ',mw)
                              ,app))
                  mw))
      (cons
       (typecase (car mw)
         (keyword `(lambda (,app)
                     (funcall (find-middleware ,(car mw)) ,app
                              ,@(cdr mw))))
         (symbol
          ;; for old Clack middlewares
          (if (clack-middleware-symbol-p (car mw))
              `(lambda (,app)
                 (funcall (intern (string :wrap) :clack)
                          (make-instance ',mw ,@(cdr mw))
                          ,app))
              ;; Normal form
              (let ((res (gensym "RES")))
                ;; reconvert the result of the form
                `(let ((,res ,mw))
                   (typecase ,res
                     (keyword (find-middleware ,res))
                     (cons (if (keywordp (car ,res))
                               `(lambda (,',app)
                                  (apply (find-middleware (car ,,res)) ,',app
                                         (cdr ,,res)))
                               ,res))
                     (otherwise ,res))))))
         (otherwise mw)))
      (otherwise mw))))

(defmacro builder (&rest app-or-middlewares)
  (let ((middlewares (butlast app-or-middlewares)))
    `(reduce #'funcall
             (remove-if
              #'null
              (list
               ,@(loop for mw in middlewares
                       when mw
                         collect (convert-to-middleware-form mw))))
             :initial-value (to-app ,(car (last app-or-middlewares)))
             :from-end t)))
