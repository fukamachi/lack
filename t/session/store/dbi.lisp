(in-package :cl-user)
(defpackage t.lack.session.store.dbi
  (:use :cl
        :lack
        :lack.test
        :lack.session.store.dbi
        :dbi
        :prove))
(in-package :t.lack.session.store.dbi)

(plan 4)

(defvar *test-db* (asdf:system-relative-pathname :lack "data/test.db"))
(when (probe-file *test-db*)
  (delete-file *test-db*))

(defparameter *conn*
  (dbi:connect :sqlite3 :database-name *test-db*))

(dbi:do-sql *conn*
  "CREATE TABLE sessions (id CHAR(72) PRIMARY KEY, session_data TEXT)")

(subtest "session middleware"
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
      (is status 200)
      (setf session (parse-lack-session headers))
      (ok session)
      (is body '("Hello, you've been here for 1th times!")))

    (diag "2nd request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (is status 200)
      (is body '("Hello, you've been here for 2th times!")))))

(subtest "utf-8 session data"
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
      (is status 200)
      (setf session (parse-lack-session headers))
      (ok session)
      (is body '("Hello, 深町英太郎! You've been here for 1th times!")))

    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (is status 200)
      (is body '("Hello, 深町英太郎! You've been here for 2th times!")))))

(let ((session (dbi:fetch (dbi:execute (dbi:prepare *conn* "SELECT COUNT(*) AS count FROM sessions")))))
  (is (getf session :|count|) 2
      "'sessions' has two records"))

(dbi:disconnect *conn*)
(delete-file *test-db*)

(setf *conn*
      (dbi:connect :sqlite3 :database-name *test-db*))

;;
;; record-timestamps t

(dbi:do-sql *conn*
  "CREATE TABLE sessions (id CHAR(72) PRIMARY KEY, session_data TEXT, created_at DATETIME, updated_at DATETIME)")

(subtest "session middleware"
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
      (is status 200)
      (setf session (parse-lack-session headers))
      (ok session)
      (is body '("Hello, you've been here for 1th times!")))

    (let ((records (dbi:fetch-all
                    (dbi:execute
                     (dbi:prepare *conn* "SELECT * FROM sessions")))))
      (is (length records) 1)
      (is (getf (first records) :|id|) session)
      (setf now (getf (first records) :|created_at|))
      (is (getf (first records) :|updated_at|) now))

    (sleep 2)

    (diag "2nd request")
    (destructuring-bind (status headers body)
        (funcall app (generate-env "/" :cookies `(("lack.session" . ,session))))
      (declare (ignore headers))
      (is status 200)
      (is body '("Hello, you've been here for 2th times!")))

    (let ((records (dbi:fetch-all
                    (dbi:execute
                     (dbi:prepare *conn* "SELECT * FROM sessions")))))
      (is (length records) 1)
      (is (getf (first records) :|id|) session)
      (is (getf (first records) :|created_at|) now)
      (isnt (getf (first records) :|updated_at|) now))))

(dbi:disconnect *conn*)

(finalize)
