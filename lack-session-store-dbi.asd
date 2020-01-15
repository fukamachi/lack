(defsystem "lack-session-store-dbi"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("lack-middleware-session"
               "dbi"
               "marshal"
               "trivial-utf-8"
               "cl-base64")
  :components ((:file "src/middleware/session/store/dbi"))
  :in-order-to ((test-op (test-op "t-lack-session-store-dbi"))))
