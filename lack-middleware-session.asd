(defsystem "lack-middleware-session"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack-request"
               "lack-response"
               "lack-util"
               "bordeaux-threads"
               "cl-ppcre")
  :components ((:module "src/middleware"
                :components
                ((:file "session" :depends-on ("store" "state"))
                 (:module "store"
                  :pathname "session"
                  :components
                  ((:file "store")
                   (:file "store/memory")))
                 (:module "state"
                  :pathname "session"
                  :components
                  ((:file "state")
                   (:file "state/cookie")))))))

(register-system-packages "lack-middleware-session" '(:lack.middleware.session))
