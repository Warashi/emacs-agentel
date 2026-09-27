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

(ert-deftest agentel-ui-state-names-the-state-on-hover ()
  (should (equal (get-text-property 0 'help-echo (agentel-ui-state 'running))
                 "running")))

(ert-deftest agentel-ui-state-shows-an-unknown-state-by-name ()
  (should (equal (agentel-ui-state 'interrupted) "[interrupted]")))

(ert-deftest agentel-ui-state-follows-the-icons-set-by-the-user ()
  (let ((agentel-ui-state-icons '((idle . "-"))))
    (should (equal (agentel-ui-state 'idle) "-"))))

(agentel-ui-define 'agentel-ui-test-counter
  :update (lambda (message data)
            (pcase message
              (`(add ,n) `((count . ,(+ n (or (alist-get 'count data) 0)))))))
  :view (lambda (model options)
          (format "%s%s" (or (plist-get options :prefix) "")
                  (agentel-ui-get model 'count))))

(defun agentel-ui-test-counts (store)
  "Return the counts of the models of STORE, oldest first."
  (mapcar (lambda (model) (agentel-ui-get model 'count)) (agentel-ui-models store)))

(ert-deftest agentel-ui-dispatch-keeps-one-model-per-key ()
  (let ((store (agentel-ui-store-create)))
    (agentel-ui-dispatch store 'c 'agentel-ui-test-counter '(add 1))
    (agentel-ui-dispatch store 'c 'agentel-ui-test-counter '(add 2))
    (should (equal (agentel-ui-test-counts store) '(3)))
    (should (eq (agentel-ui-find store 'c) (agentel-ui-last store)))))

(ert-deftest agentel-ui-dispatch-without-a-key-adds-a-model-each-time ()
  (let ((store (agentel-ui-store-create)))
    (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 1))
    (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 2))
    (should (equal (agentel-ui-test-counts store) '(1 2)))
    (should (equal (agentel-ui-get (agentel-ui-last store) 'count) 2))))

(ert-deftest agentel-ui-update-changes-a-model-without-a-key ()
  (let* ((store (agentel-ui-store-create))
         (model (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 1))))
    (agentel-ui-update store model '(add 2))
    (should (equal (agentel-ui-test-counts store) '(3)))))

(ert-deftest agentel-ui-tells-subscribers-which-model-was-added-or-changed ()
  (let ((store (agentel-ui-store-create))
        told)
    (agentel-ui-subscribe store (lambda (model added)
                                  (push (list (agentel-ui-model-key model) added) told)))
    (agentel-ui-dispatch store 'a 'agentel-ui-test-counter '(add 1))
    (agentel-ui-dispatch store 'a 'agentel-ui-test-counter '(add 1))
    (agentel-ui-dispatch store 'b 'agentel-ui-test-counter '(add 1))
    (should (equal (reverse told) '((a t) (a nil) (b t))))))

(ert-deftest agentel-ui-tells-subscribers-in-the-order-they-subscribed ()
  (let ((store (agentel-ui-store-create))
        told)
    (agentel-ui-subscribe store (lambda (_ _) (push 'first told)))
    (agentel-ui-subscribe store (lambda (_ _) (push 'second told)))
    (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 1))
    (should (equal (reverse told) '(first second)))))

(ert-deftest agentel-ui-stops-telling-an-unsubscribed-function ()
  (let* ((store (agentel-ui-store-create))
         (told 0)
         (subscriber (lambda (_ _) (setq told (1+ told)))))
    (agentel-ui-subscribe store subscriber)
    (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 1))
    (agentel-ui-unsubscribe store subscriber)
    (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 1))
    (should (= told 1))))

(ert-deftest agentel-ui-view-shows-a-model-as-its-type-defines ()
  (let* ((store (agentel-ui-store-create))
         (model (agentel-ui-dispatch store nil 'agentel-ui-test-counter '(add 4))))
    (should (equal (agentel-ui-view model) "4"))
    (should (equal (agentel-ui-view model :prefix "n=") "n=4"))))

(ert-deftest agentel-ui-kind-returns-more-properties-of-a-type ()
  (agentel-ui-define 'agentel-ui-test-folded :update #'ignore :view #'ignore :collapsed t)
  (should (agentel-ui-kind 'agentel-ui-test-folded :collapsed))
  (should-not (agentel-ui-kind 'agentel-ui-test-counter :collapsed)))

(provide 'agentel-ui-test)
;;; agentel-ui-test.el ends here
