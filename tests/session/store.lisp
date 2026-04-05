(defpackage lack/tests/session/store
  (:use :cl
        :lack/middleware/session/store
        :rove))
(in-package :lack/tests/session/store)

(deftest safe-read-from-string-normal
  (testing "parses a normal S-expression"
    (ok (equal '(1 2 3) (safe-read-from-string "(1 2 3)"))))
  (testing "parses a keyword symbol"
    (ok (eq :hello (safe-read-from-string ":hello"))))
  (testing "parses a string"
    (ok (equal "world" (safe-read-from-string "\"world\"")))))

(deftest safe-read-from-string-nil-input
  (testing "nil input returns nil"
    (ok (null (safe-read-from-string nil))))
  (testing "empty string returns nil"
    (ok (null (safe-read-from-string ""))))
  (testing "whitespace-only string returns nil"
    (ok (null (safe-read-from-string "   ")))))

(deftest safe-read-from-string-blocks-read-eval
  (testing "blocks #. read-time eval"
    (ok (signals (safe-read-from-string "#.(error \"exploit\")") 'error))))

(deftest safe-read-from-string-blocks-circular
  (testing "blocks #= circular-structure label"
    (ok (signals (safe-read-from-string "#1=(a . #1#)") 'error)))
  (testing "blocks ## circular-structure reference"
    (ok (signals (safe-read-from-string "#1# ") 'error))))
