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

(defmacro agentel-ui-test-with-narrow-window (&rest body)
  "Run BODY in a buffer shown in a window of about 20 columns."
  (declare (indent 0))
  `(save-window-excursion
     (delete-other-windows)
     (split-window-right 21)
     (with-temp-buffer
       (set-window-buffer (selected-window) (current-buffer))
       ,@body)))

(ert-deftest agentel-ui-line-width-follows-a-narrow-window ()
  (agentel-ui-test-with-narrow-window
    (let ((columns (window-max-chars-per-line)))
      (should (< columns agentel-ui-max-line-width))
      (should (= (agentel-ui-line-width) columns))
      (should (= (string-width (agentel-ui-one-line "> " (make-string 50 ?x)))
                 columns)))))

(ert-deftest agentel-ui-line-width-stops-at-the-largest-width ()
  (agentel-ui-test-with-narrow-window
    (let ((agentel-ui-max-line-width 10))
      (should (= (agentel-ui-line-width) 10)))))

(ert-deftest agentel-ui-line-width-of-a-hidden-buffer-is-the-largest-width ()
  (with-temp-buffer
    (should (= (agentel-ui-line-width) agentel-ui-max-line-width))))

(provide 'agentel-ui-test)
;;; agentel-ui-test.el ends here
