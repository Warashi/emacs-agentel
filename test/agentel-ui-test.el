;;; agentel-ui-test.el --- Tests for agentel-ui  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-ui)

(ert-deftest agentel-ui-one-line-keeps-a-short-line ()
  (should (equal (agentel-ui-one-line "▸ " "Read README.org")
                 "▸ Read README.org")))

(ert-deftest agentel-ui-one-line-fits-the-whole-line-in-the-width ()
  (let* ((agentel-ui-max-line-width 10)
         (line (agentel-ui-one-line "> " "abcdefghijkl")))
    (should (equal line "> abcdefg…"))
    (should (= (string-width line) 10))))

(ert-deftest agentel-ui-one-line-marks-the-lines-left-out ()
  (should (equal (agentel-ui-one-line "$ " "cat <<'EOF'\nhello\nEOF")
                 "$ cat <<'EOF'…")))

(ert-deftest agentel-ui-one-line-skips-leading-blank-lines-of-the-text ()
  (should (equal (agentel-ui-one-line "    " "\n\n  Look around\n")
                 "    Look around")))

(ert-deftest agentel-ui-one-line-accepts-no-text ()
  (should (equal (agentel-ui-one-line "⎇ x" nil) "⎇ x")))

(provide 'agentel-ui-test)
;;; agentel-ui-test.el ends here
