(defsystem "lack-app-file"
  :depends-on ("lack-component"
               "trivial-mimes"
               "local-time"
               "alexandria")
  :components ((:file "src/app/file")))

(register-system-packages "lack-app-file" '(:lack.app.file))
