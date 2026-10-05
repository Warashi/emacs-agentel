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

(ert-deftest agentel-ui-window-width-is-the-narrow-window-beyond-the-largest-width ()
  (agentel-ui-test-with-narrow-window
    (let ((agentel-ui-max-line-width 10))
      (should (= (agentel-ui-window-width) (window-max-chars-per-line))))))

(ert-deftest agentel-ui-window-width-of-a-hidden-buffer-is-unknown ()
  (with-temp-buffer
    (should-not (agentel-ui-window-width))))

(ert-deftest agentel-ui-line-width-keeps-point-away-from-an-unselected-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((shown (generate-new-buffer "shown")))
      (unwind-protect
          (with-current-buffer shown
            (insert (make-string 20 ?x))
            (set-window-buffer (split-window-right) shown)
            (goto-char 5)
            (should-not (= (point) (window-point (get-buffer-window shown))))
            (agentel-ui-line-width)
            (should (= (point) 5)))
        (kill-buffer shown)))))

(ert-deftest agentel-ui-one-line-fits-a-given-width ()
  (agentel-ui-test-with-narrow-window
    (should (equal (agentel-ui-one-line "> " "abcdefghijkl" 6) "> abc…"))))

(ert-deftest agentel-ui-one-line-is-marked-as-fitting-the-width ()
  (should (get-text-property 0 'agentel-ui-fits-width (agentel-ui-one-line "> " "x"))))

(ert-deftest agentel-ui-follow-width-reports-only-a-change ()
  (agentel-ui-test-with-narrow-window
    (let* ((calls 0)
           (agentel-ui--followers nil))
      (agentel-ui-follow-width (lambda () (setq calls (1+ calls))))
      (agentel-ui--check-widths)
      (should (= calls 0))
      (delete-other-windows)
      (agentel-ui--check-widths)
      (should (= calls 1))
      (agentel-ui--check-widths)
      (should (= calls 1)))))

(ert-deftest agentel-ui-follow-width-reports-a-change-beyond-the-largest-width ()
  (agentel-ui-test-with-narrow-window
    (let* ((calls 0)
           (agentel-ui-max-line-width 10)
           (agentel-ui--followers nil))
      (agentel-ui-follow-width (lambda () (setq calls (1+ calls))))
      (delete-other-windows)
      (agentel-ui--check-widths)
      (should (= calls 1)))))

(ert-deftest agentel-ui-follow-width-forgets-a-buffer-that-changed-its-mode ()
  (agentel-ui-test-with-narrow-window
    (let ((agentel-ui--followers nil))
      (agentel-ui-follow-width #'ignore)
      (fundamental-mode)
      (delete-other-windows)
      (agentel-ui--check-widths)
      (should-not agentel-ui--followers))))

(ert-deftest agentel-ui-state-shows-a-known-state-as-its-icon ()
  (should (equal (agentel-ui-state 'waiting) "🙋")))

(ert-deftest agentel-ui-state-shows-loading-as-an-icon ()
  (should (equal (agentel-ui-state 'loading) "📥")))

(ert-deftest agentel-ui-state-names-the-state-on-hover ()
  (should (equal (get-text-property 0 'help-echo (agentel-ui-state 'running))
                 "running")))

(ert-deftest agentel-ui-state-shows-an-unknown-state-by-name ()
  (should (equal (agentel-ui-state 'interrupted) "[interrupted]")))

(ert-deftest agentel-ui-state-follows-the-icons-set-by-the-user ()
  (let ((agentel-ui-state-icons '((idle . "-"))))
    (should (equal (agentel-ui-state 'idle) "-"))))

(ert-deftest agentel-ui-view-shows-a-model-as-its-type-defines ()
  (agentel-ui-define-view 'agentel-ui-test-count
    (lambda (model options)
      (format "%s%s" (or (plist-get options :prefix) "")
              (agentel-store-get model 'count))))
  (let ((model (agentel-store-model--make :type 'agentel-ui-test-count
                                          :data '((count . 4)))))
    (should (equal (agentel-ui-view model) "4"))
    (should (equal (agentel-ui-view model :prefix "n=") "n=4"))))

(ert-deftest agentel-ui-view-property-returns-more-properties-of-a-type ()
  (agentel-ui-define-view 'agentel-ui-test-folded #'ignore :collapsed t)
  (agentel-ui-define-view 'agentel-ui-test-open #'ignore)
  (should (agentel-ui-view-property 'agentel-ui-test-folded :collapsed))
  (should-not (agentel-ui-view-property 'agentel-ui-test-open :collapsed)))

(provide 'agentel-ui-test)
;;; agentel-ui-test.el ends here
