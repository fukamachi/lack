(defsystem "lack-util-writer-stream"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("trivial-gray-streams"
               "babel")
  :components ((:file "src/util/writer-stream")))

(register-system-packages "lack-util-writer-stream" '(:lack.util.writer-stream))
