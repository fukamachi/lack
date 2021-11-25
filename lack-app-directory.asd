(defsystem "lack-app-directory"
  :depends-on ("lack-app-file"
               "cl-ppcre"
               "trivial-rfc-1123"
               "trivial-mimes"
               "quri")
  :components ((:file "src/app/directory")))

(register-system-packages "lack-app-directory" '(:lack.app.directory))
