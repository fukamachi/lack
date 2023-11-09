(defsystem "lack"
  :version "0.3.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-component"
               "lack-util")
  :pathname "src"
  :components ((:file "lack" :depends-on ("builder"))
               (:file "builder"))
  :description "A minimal Clack"
  :in-order-to ((test-op (test-op "lack/tests"))))

(defsystem "lack/app/directory" :depends-on ("lack-app-directory"))
(defsystem "lack/app/file" :depends-on ("lack-app-file"))
(defsystem "lack/component" :depends-on ("lack-component"))
(defsystem "lack/middleware/accesslog" :depends-on ("lack-middleware-accesslog"))
(defsystem "lack/middleware/auth/basic" :depends-on ("lack-middleware-auth-basic"))
(defsystem "lack/middleware/backtrace" :depends-on ("lack-middleware-backtrace"))
(defsystem "lack/middleware/csrf" :depends-on ("lack-middleware-csrf"))
(defsystem "lack/middleware/dbpool" :depends-on ("lack-middleware-dbpool"))
(defsystem "lack/middleware/mount" :depends-on ("lack-middleware-mount"))
(defsystem "lack/middleware/session" :depends-on ("lack-middleware-session"))
(defsystem "lack/middleware/static" :depends-on ("lack-middleware-static"))
(defsystem "lack/request" :depends-on ("lack-request"))
(defsystem "lack/response" :depends-on ("lack-response"))
(defsystem "lack/session/store/dbi" :depends-on ("lack-session-store-dbi"))
(defsystem "lack/session/store/redis" :depends-on ("lack-session-store-redis"))
(defsystem "lack/test" :depends-on ("lack-test"))
(defsystem "lack/util/writer/stream" :depends-on ("lack-util-writer-stream"))
(defsystem "lack/util" :depends-on ("lack-util"))

(defsystem "lack/tests"
  :depends-on ("lack"
               "lack/request"
               "lack/component"
               "lack/test"
               "lack/util"
               "lack/middleware/static"
               "lack/middleware/accesslog"
               "lack/middleware/session"
               "lack/middleware/mount"
               "lack/middleware/csrf"
               "lack/middleware/auth/basic"
               "lack/session/store/redis"
               "lack/session/store/dbi"
               "clack"
               "clack-test"
               "hunchentoot"
               "dexador"
               "cl-cookie"
               "flexi-streams"
               "dbi"
               "sqlite"
               "cl-ppcre"
               "cl-base64"
               "rove"
               "alexandria"
               "split-sequence")
  :pathname "tests"
  :serial t
  :components ((:file "builder")
               (:file "util")
               (:file "request")
               (:file "component")
               (:file "media-type")
               (:module "middleware"
                :components
                ((:file "static")
                 (:file "session")
                 (:file "mount")
                 (:file "backtrace")
                 (:file "csrf")
                 (:file "auth/basic")
                 (:file "accesslog")))
               (:module "session"
                :components
                ((:module "store"
                  :components
                  ((:file "dbi")
                   #+todo
                   (:file "redis"))))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
