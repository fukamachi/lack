(defsystem "lack-middleware-mount"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-component")
  :components ((:file "src/middleware/mount")))

(register-system-packages "lack-middleware-mount" '(:lack.middleware.mount))
