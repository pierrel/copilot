;;; test-copilot.el --- Tests for copilot.el -*- lexical-binding: t; -*-

(require 'ert)
(load-file "./copilot.el")

(ert-deftest copilot-truncate-words ()
  (should (equal (copilot--truncate-words "one two three four" 2) "one two"))
  (should (equal (copilot--truncate-words "one" 10) "one")))

(ert-deftest copilot-buffer-name ()
  (let ((default-directory "/tmp/"))
    (should (string-match "^\*Copilot" (copilot--buffer-name "hello world")))))

;;; test-copilot.el ends here
