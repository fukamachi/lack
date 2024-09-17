(defsystem "lack-request"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("quri"
               "http-body"
               "circular-streams"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "request" :depends-on ("media-type"))
                 (:file "media-type")))))

(register-system-packages "lack-request" '(:lack.request))
