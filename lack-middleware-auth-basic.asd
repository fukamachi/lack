(defsystem "lack-middleware-auth-basic"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("cl-base64"
               "split-sequence")
  :components ((:module "src"
                :components
                ((:file "middleware/auth/basic"))))
  :in-order-to ((test-op (test-op "t-lack-middleware-auth-basic"))))
