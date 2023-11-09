(defpackage lack/tests/session/store/dbi
  (:use :cl
        :lack
        :lack/test
        :lack/session/store/dbi
        :dbi
        :rove))
(in-package :lack/tests/session/store/dbi)

(defvar *test-db* (asdf:system-relative-pathname :lack "data/test.db"))
(defparameter *conn* nil)

(defun init-db ()
  (when (probe-file *test-db*)
    (delete-file *test-db*))

  (setf *conn* (dbi:connect :sqlite3 :database-name *test-db*)))

(defun clearup-db ()
  (dbi:disconnect *conn*)
  (setf *conn* nil)
  (delete-file *test-db*))

(deftest session-middleware
  (init-db)
  (dbi:do-sql *conn*
    "CREATE TABLE sessions (id CHAR(72) PRIMARY KEY, session_data TEXT)")
  (let ((app
          (builder
           (:session
            :store (make-dbi-store
                    :connector (lambda () *conn*)))
           (lambda (env)
             (unless (gethash :counter (getf env :lack.session))
               (setf (gethash :counter (getf env :lack.session)) 0))
             `(200
               (:content-type "text/plain")
               (,(format nil "Hello, you've been here for ~Ath times!"
                         (incf (gethash :counter (getf env :lack.session)))))))))
        session)
    (diag "1st request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (ok (eql status 200))
      (setf session (parse-lack-session headers))
      (ok session)
      (ok (equalp body '("Hello, you've been here for 1th times!"))))

    (diag "2nd request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (ok (eql status 200))
      (ok (equalp body '("Hello, you've been here for 2th times!")))))

  (testing "utf-8 session data"
    (let ((app
            (builder
             (:session
              :store (make-dbi-store
                      :connector (lambda () *conn*)))
             (lambda (env)
               (unless (gethash :user (getf env :lack.session))
                 (setf (gethash :user (getf env :lack.session)) "深町英太郎"))
               (unless (gethash :counter (getf env :lack.session))
                 (setf (gethash :counter (getf env :lack.session)) 0))
               `(200
                 (:content-type "text/plain")
                 (,(format nil "Hello, ~A! You've been here for ~Ath times!"
                           (gethash :user (getf env :lack.session))
                           (incf (gethash :counter (getf env :lack.session)))))))))
          session)
      (destructuring-bind (status headers body)
          (funcall app (generate-env "/"))
        (ok (eql status 200))
        (setf session (parse-lack-session headers))
        (ok session)
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 1th times!"))))

      (destructuring-bind (status headers body)
          (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
        (declare (ignore headers))
        (ok (eql status 200))
        (ok (equalp body '("Hello, 深町英太郎! You've been here for 2th times!"))))))

  (let ((session (dbi:fetch (dbi:execute (dbi:prepare *conn* "SELECT COUNT(*) AS count FROM sessions")))))
    (ok (eql (getf session :|count|) 2)
        "'sessions' has two records"))
  (clearup-db))

;;
;; record-timestamps t

(deftest record-timestamps
  (init-db)
  (dbi:do-sql *conn*
    "CREATE TABLE sessions (id CHAR(72) PRIMARY KEY, session_data TEXT, created_at DATETIME, updated_at DATETIME)")
  (let ((app
          (builder
           (:session
            :store (make-dbi-store
                    :connector (lambda () *conn*)
                    :record-timestamps t))
           (lambda (env)
             (unless (gethash :counter (getf env :lack.session))
               (setf (gethash :counter (getf env :lack.session)) 0))
             `(200
               (:content-type "text/plain")
               (,(format nil "Hello, you've been here for ~Ath times!"
                         (incf (gethash :counter (getf env :lack.session)))))))))
        session
        now)
    (diag "1st request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/"))
      (ok (eql status 200))
      (setf session (parse-lack-session headers))
      (ok session)
      (ok (equalp body '("Hello, you've been here for 1th times!"))))

    (let ((records (dbi:fetch-all
                    (dbi:execute
                     (dbi:prepare *conn* "SELECT * FROM sessions")))))
      (ok (eql (length records) 1))
      (ok (equal (getf (first records) :|id|) session))
      (setf now (getf (first records) :|created_at|))
      (ok (equal (getf (first records) :|updated_at|) now)))

    (sleep 2)

    (diag "2nd request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (ok (eql status 200))
      (ok (equalp body '("Hello, you've been here for 2th times!"))))

    (let ((records (dbi:fetch-all
                    (dbi:execute
                     (dbi:prepare *conn* "SELECT * FROM sessions")))))
      (ok (eql (length records) 1))
      (ok (equal (getf (first records) :|id|) session))
      (ok (equal (getf (first records) :|created_at|) now))
      (ng (equal (getf (first records) :|updated_at|) now))))
  (clearup-db))
