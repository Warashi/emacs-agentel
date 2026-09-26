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
         (agentel-session-changed-functions (list #'agentel-list--track-unread
                                                  #'agentel-list--schedule-refresh)))
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
                   '("  repo-one [idle]" "    Fix things"
                     "    └ Explore [idle]"
                     "  two [idle]")))))

(ert-deftest agentel-list-shows-when-a-subagent-waits-for-the-user ()
  (agentel-list-test-with-sessions
    (agentel-session-add-pending child 'question)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("  repo-one [waiting]" "    Fix things"
                     "    └ Explore [waiting]"
                     "  two [idle]")))))

(ert-deftest agentel-list-puts-sessions-waiting-for-the-user-first ()
  (agentel-list-test-with-sessions
    (agentel-session-add-pending other 'question)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("  two [waiting]"
                     "  repo-one [idle]" "    Fix things"
                     "    └ Explore [idle]")))))

(ert-deftest agentel-list-marks-a-session-whose-turn-ended-out-of-sight ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-session-set-busy other nil)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("● two [idle]"
                     "  repo-one [idle]" "    Fix things"
                     "    └ Explore [idle]")))))

(ert-deftest agentel-list-puts-waiting-sessions-before-unread-ones ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-session-set-busy other nil)
    (agentel-session-add-pending child 'question)
    (agentel-list--refresh)
    (should (equal (car (agentel-list-test-lines)) "  repo-one [waiting]"))))

(ert-deftest agentel-list-does-not-mark-a-session-whose-turn-ended-in-sight ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer other))
      (agentel-session-set-busy other t)
      (agentel-session-set-busy other nil))
    (agentel-list--refresh)
    (should (equal (car (last (agentel-list-test-lines))) "  two [idle]"))))

(ert-deftest agentel-list-does-not-mark-a-subagent ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy child t)
    (agentel-session-set-busy child nil)
    (agentel-list--refresh)
    (should-not (seq-some (lambda (line) (string-prefix-p "●" line))
                          (agentel-list-test-lines)))))

(ert-deftest agentel-list-unmarks-a-session-once-shown ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-session-set-busy other nil)
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer other))
      (agentel-list--forget-shown (selected-frame)))
    (agentel-list--refresh)
    (should (equal (car (last (agentel-list-test-lines))) "  two [idle]"))))

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
                       "    Fix things")))
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

(defun agentel-list-test-window ()
  "Return the window showing the list, or nil."
  (get-buffer-window agentel-list-buffer-name))

(ert-deftest agentel-list-opens-in-a-side-window-and-moves-to-it ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer parent))
      (agentel-list)
      (let ((window (agentel-list-test-window)))
        (should (eq (selected-window) window))
        (should (eq (window-parameter window 'window-side) agentel-list-side))
        (should (window-parameter window 'no-other-window))))))

(ert-deftest agentel-list-moves-to-the-list-already-shown ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer parent))
      (agentel-list)
      (other-window -1 t)
      (let ((windows (length (window-list))))
        (agentel-list)
        (should (eq (selected-window) (agentel-list-test-window)))
        (should (= (length (window-list)) windows))))))

(ert-deftest agentel-list-closes-the-list-from-inside ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer parent))
      (agentel-list)
      (agentel-list)
      (should-not (agentel-list-test-window))
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer parent))))))

(ert-deftest agentel-list-stays-when-a-session-is-visited ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer parent))
      (agentel-list)
      (goto-char (point-min))
      (search-forward "two")
      (agentel-list-visit)
      (should (eq (window-buffer (selected-window)) (agentel-session-buffer other)))
      (should (agentel-list-test-window)))))

(ert-deftest agentel-list-stays-when-other-windows-are-deleted ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer parent))
      (agentel-list)
      (other-window -1 t)
      (delete-other-windows)
      (should (agentel-list-test-window)))))

(ert-deftest agentel-list-does-not-stop-a-subagent-alone ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "Explore")
    (should-error (agentel-list-kill) :type 'user-error)))

(provide 'agentel-list-test)
;;; agentel-list-test.el ends here
