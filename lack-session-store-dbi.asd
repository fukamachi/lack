(defsystem "lack-session-store-dbi"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-middleware-session"
               "dbi"
               "marshal"
               "trivial-utf-8"
               "cl-base64")
  :components ((:file "src/middleware/session/store/dbi")))

(register-system-packages "lack-session-store-dbi" '(:lack.session.store.dbi))
