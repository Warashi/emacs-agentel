;;; agentel-list-test.el --- Tests for agentel-list  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-list)

(defmacro agentel-list-test-with-sessions (&rest body)
  "Run BODY with a parent `parent', its child `child' and another `other'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions (list #'agentel-list--schedule-refresh)))
     (let* ((parent (agentel-session-create :cwd "/tmp/one/" :project "repo-one"))
            (other (agentel-session-create :cwd "/tmp/two/"))
            (child (agentel-session-create :parent parent :cwd "/tmp/one/")))
       (agentel-session-register parent "p")
       (agentel-session-register other "o")
       (agentel-session-register child "c")
       (setf (agentel-session-title parent) "Fix things"
             (agentel-session-title child) "Explore")
       (agentel-chat-open parent :input t)
       (agentel-chat-open child)
       (agentel-chat-open other :input t)
       (unwind-protect
           (with-current-buffer (agentel-list-noselect)
             ,@body)
         (dolist (s (list parent child other))
           (kill-buffer (agentel-session-buffer s)))
         (kill-buffer agentel-list-buffer-name)))))

(defun agentel-list-test-lines ()
  "Return the lines of the list."
  (split-string (buffer-substring-no-properties (point-min) (point-max))
                "\n" t))

(ert-deftest agentel-list-shows-the-project-then-the-title-of-a-session ()
  (agentel-list-test-with-sessions
    (should (equal (agentel-list-test-lines)
                   '("repo-one [idle]" "  Fix things"
                     "  └ Explore [idle]"
                     "two [idle]")))))

(ert-deftest agentel-list-shows-when-a-subagent-waits-for-the-user ()
  (agentel-list-test-with-sessions
    (agentel-session-add-pending child 'question)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("repo-one [waiting]" "  Fix things"
                     "  └ Explore [waiting]"
                     "two [idle]")))))

(ert-deftest agentel-list-fits-a-narrow-window ()
  (agentel-list-test-with-sessions
    (should (<= (apply #'max (mapcar #'string-width (agentel-list-test-lines)))
                40))))

(ert-deftest agentel-list-keeps-point-on-its-session-across-refreshes ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "two")
    (agentel-session-set-busy parent t)
    (agentel-list--refresh)
    (should (eq (agentel-list--session) other))))

(ert-deftest agentel-list-keeps-the-window-on-its-line-across-refreshes ()
  (agentel-list-test-with-sessions
    (let ((list-window (display-buffer (current-buffer))))
      (with-selected-window list-window
        (goto-char (point-min))
        (search-forward "Fix things"))
      (with-temp-buffer
        (agentel-session-set-busy other t)
        (agentel-list--refresh))
      (with-selected-window list-window
        (should (equal (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
                       "  Fix things")))
      (delete-window list-window))))

(ert-deftest agentel-list-follows-changes ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-list--refresh)
    (goto-char (point-min))
    (should (search-forward "two [running]" nil t))))

(ert-deftest agentel-list-visits-the-session-on-its-title-line ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "Fix things")
    (agentel-list-visit)
    (should (eq (window-buffer (selected-window)) (agentel-session-buffer parent)))))

(ert-deftest agentel-list-visits-the-session-at-point ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "Explore")
    (agentel-list-visit)
    (should (eq (window-buffer (selected-window)) (agentel-session-buffer child)))))

(ert-deftest agentel-list-visits-a-session-whose-buffer-was-killed ()
  (agentel-list-test-with-sessions
    (kill-buffer (agentel-session-buffer child))
    (goto-char (point-min))
    (search-forward "Explore")
    (agentel-list-visit)
    (should (buffer-live-p (agentel-session-buffer child)))))

(ert-deftest agentel-list-does-not-stop-a-subagent-alone ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "Explore")
    (should-error (agentel-list-kill) :type 'user-error)))

(provide 'agentel-list-test)
;;; agentel-list-test.el ends here
