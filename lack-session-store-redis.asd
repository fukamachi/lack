(defsystem "lack-session-store-redis"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-middleware-session"
               "cl-redis"
               "marshal"
               "cl-base64"
               "trivial-utf-8")
  :components ((:file "src/middleware/session/store/redis"))
  :in-order-to ((test-op (test-op "t-lack-session-store-redis"))))

(register-system-packages "lack-session-store-redis" '(:lack.session.store.redis))
