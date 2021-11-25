(defsystem "lack-app-file"
  :depends-on ("lack-component"
               "trivial-mimes"
               "trivial-rfc-1123"
               "alexandria")
  :components ((:file "src/app/file")))

(register-system-packages "lack-app-file" '(:lack.app.file))
