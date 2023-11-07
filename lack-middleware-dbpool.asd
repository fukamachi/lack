(defsystem "lack-middleware-dbpool"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("dbi"
               "anypool")
  :components ((:file "src/middleware/dbpool")))

(register-system-packages "lack-middleware-dbpool" '(:lack.middleware.dbpool
                                                     :lack/middleware/dbpool))
