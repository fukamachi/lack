(defsystem "lack"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack-component"
               "lack-util")
  :components ((:module "src"
                :components
                ((:file "lack" :depends-on ("builder"))
                 (:file "builder"))))
  :description "A minimal Clack"
  :in-order-to ((test-op (test-op "t-lack"))))

(register-system-packages "lack-component" '(#:lack.component))
(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-response" '(#:lack.response))
(register-system-packages "lack-test" '(#:lack.test))
(register-system-packages "lack-middleware-accesslog" '(#:lack.middleware.accesslog))
(register-system-packages "lack-middleware-auth-basic" '(#:lack.middleware.auth.basic))
(register-system-packages "lack-middleware-backtrace" '(#:lack.middleware.backtrace))
(register-system-packages "lack-middleware-csrf" '(#:lack.middleware.csrf))
(register-system-packages "lack-middleware-mount" '(#:lack.middleware.mount))
(register-system-packages "lack-middleware-session" '(#:lack.middleware.session))
(register-system-packages "lack-middleware-static" '(#:lack.middleware.static))
(register-system-packages "lack-session-store-dbi" '(#:lack.session.store.dbi))
(register-system-packages "lack-session-store-redis" '(#:lack.session.store.redis))
(register-system-packages "lack-util" '(#:lack.util))
(register-system-packages "lack-util-writer-stream" '(#:lack.util.writer-stream))
