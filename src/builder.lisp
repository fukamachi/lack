(in-package :cl-user)
(defpackage lack.builder
  (:use :cl)
  (:import-from :lack.component
                :to-app)
  (:import-from :lack.util
                :find-middleware)
  (:export :builder))
(in-package :lack.builder)

(defun clack-middleware-symbol-p (symbol)
  (and (symbolp symbol)
       (find-package :clack.middleware)
       (find-class symbol nil)
       (subtypep (find-class symbol)
                 (intern (string :<middleware>)
                         :clack.middleware))))

(defun convert-to-middleware-form (mw)
  (let ((app (gensym "APP"))
        (res-mw (gensym "RES-MW")))
    (etypecase mw
      (null)
      (function mw)
      (keyword `(find-middleware ,mw))
      ;; for old Clack middlewares
      (symbol (if (clack-middleware-symbol-p mw)
                  `(lambda (,app)
                     (funcall (intern (string :wrap) :clack.middleware)
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
                 (funcall (intern (string :wrap) :clack.middleware)
                          (make-instance ',(car mw) ,@(cdr mw))
                          ,app))
              ;; Normal form
              (let ((res (gensym "RES")))
                ;; reconvert the result of the form
                `(let ((,res ,mw))
                   (typecase ,res
                     (keyword (find-middleware ,res))
                     (cons (if (keywordp (car ,res))
                               (let ((,res-mw (find-middleware (car ,res))))
                                 (lambda (,app)
                                   (apply ,res-mw ,app (cdr ,res))))
                               ,res))
                     (standard-object
                      (lambda (,app)
                        (funcall (intern (string :wrap) :clack.middleware) ,res ,app)))
                     (otherwise ,res))))))
         (otherwise mw))))))

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
