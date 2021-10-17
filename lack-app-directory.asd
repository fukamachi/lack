(defsystem "lack-app-directory"
  :depends-on ("lack-app-file"
               "cl-ppcre"
               "local-time"
               "trivial-mimes"
               "quri")
  :components ((:file "src/app/directory")))

(register-system-packages "lack-app-directory" '(:lack.app.directory))
