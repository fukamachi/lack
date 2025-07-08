(defsystem "lack-middleware-deflater"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("salza2"
               "zstd"
               "trivial-mimes"
               "babel"
               "cl-ppcre"
               "lack-util-writer-stream")
  :components ((:module "src"
                :components
                ((:file "middleware/deflater")))))

(register-system-packages "lack-middleware-deflater" '(:lack.middleware.deflater))
