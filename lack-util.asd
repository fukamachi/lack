(defsystem "lack-util"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ((:feature (:or :windows :mswindows :win32 :cormanlisp) "ironclad")
               (:feature (:not (:or :windows :mswindows :win32 :cormanlisp)) "cl-isaac"))
  :components ((:file "src/util"))
  :in-order-to ((test-op (test-op "t-lack-util"))))

(register-system-packages "lack-util" '(:lack.util))
