;;; agentel-diff-test.el --- Tests for agentel-diff  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-diff)

(ert-deftest agentel-diff-shows-an-edit-of-a-snippet ()
  (should (equal (agentel-diff "/tmp/a" "x" "y")
                 "--- /tmp/a\n+++ /tmp/a\n@@ -1 +1 @@\n-x\n+y")))

(ert-deftest agentel-diff-shows-a-new-file-as-added-lines ()
  (should (equal (agentel-diff "/tmp/b" nil "a\nb\n")
                 "--- /tmp/b\n+++ /tmp/b\n@@ -0,0 +1,2 @@\n+a\n+b")))

(ert-deftest agentel-diff-keeps-the-context-of-a-hunk ()
  (should (equal (agentel-diff "/tmp/c" "a\nb\nc" "a\nB\nc")
                 "--- /tmp/c\n+++ /tmp/c\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c")))

(ert-deftest agentel-diff-of-the-same-text-is-nil ()
  (should-not (agentel-diff "/tmp/d" "a" "a")))

(ert-deftest agentel-diff-colors-lines-as-diff-mode-does ()
  (let ((diff (agentel-diff "/tmp/c" "a\nb\nc" "a\nB\nc")))
    (dolist (case '(("--- " . diff-file-header) ("+++ " . diff-file-header)
                    ("@@ " . diff-hunk-header) ("-b" . diff-removed)
                    ("+B" . diff-added) (" a" . nil)))
      (should (eq (get-text-property (string-search (car case) diff) 'face diff)
                  (cdr case))))))

(provide 'agentel-diff-test)
;;; agentel-diff-test.el ends here
