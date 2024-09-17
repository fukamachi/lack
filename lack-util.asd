(defsystem "lack-util"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ((:feature (:or :windows :mswindows :win32 :cormanlisp) "ironclad")
               (:feature (:not (:or :windows :mswindows :win32 :cormanlisp)) "cl-isaac")
	       "bordeaux-threads")
  :components ((:file "src/util")))

(register-system-packages "lack-util" '(:lack.util))
