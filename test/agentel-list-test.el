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
         (agentel-list--store (agentel-store-create))
         (agentel-session-changed-functions (list #'agentel-list--on-changed)))
     (let* ((parent (agentel-session-create :cwd "/tmp/one/" :project "repo-one"))
            (other (agentel-session-create :cwd "/tmp/two/"))
            (child (agentel-session-create :parent parent :cwd "/tmp/one/")))
       (agentel-session-register parent "p")
       (agentel-session-register other "o")
       (agentel-session-register child "c")
       (setf (agentel-session-title parent) "Fix things"
             (agentel-session-title child) "Explore")
       (mapc #'agentel-session-changed (list parent child))
       (agentel-chat-open parent :input t)
       (agentel-chat-open child)
       (agentel-chat-open other :input t)
       (unwind-protect
           (with-current-buffer (agentel-list-noselect)
             ,@body)
         (dolist (s (list parent child other))
           (kill-buffer (agentel-session-buffer s)))
         (kill-buffer agentel-list-buffer-name)))))

(defun agentel-list-test-after (&rest messages)
  "Return the data of the list after MESSAGES, starting with no session."
  (let (data)
    (dolist (message messages data)
      (setq data (agentel-list--update message data)))))

(defun agentel-list-test-seen (session &rest properties)
  "Return the message that SESSION was seen as PROPERTIES say."
  `(changed ,(append (list :session session :live t) properties)))

(ert-deftest agentel-list-update-marks-a-turn-that-ended-out-of-sight ()
  (should (equal (alist-get 'unread (agentel-list-test-after
                                     (agentel-list-test-seen 'a :busy t)
                                     (agentel-list-test-seen 'a :busy nil)))
                 '(a))))

(ert-deftest agentel-list-update-does-not-mark-a-turn-that-ended-in-sight ()
  (should-not (alist-get 'unread (agentel-list-test-after
                                  (agentel-list-test-seen 'a :busy t)
                                  (agentel-list-test-seen 'a :busy nil :shown t)))))

(ert-deftest agentel-list-update-does-not-mark-a-subagent ()
  (should-not (alist-get 'unread (agentel-list-test-after
                                  (agentel-list-test-seen 'c :busy t :parent 'p)
                                  (agentel-list-test-seen 'c :busy nil :parent 'p)))))

(ert-deftest agentel-list-update-does-not-mark-a-session-that-was-not-busy ()
  (should-not (alist-get 'unread (agentel-list-test-after
                                  (agentel-list-test-seen 'a :busy nil)
                                  (agentel-list-test-seen 'a :busy nil)))))

(ert-deftest agentel-list-update-unmarks-sessions-once-shown ()
  (should (equal (alist-get 'unread (agentel-list-test-after
                                     (agentel-list-test-seen 'a :busy t)
                                     (agentel-list-test-seen 'b :busy t)
                                     (agentel-list-test-seen 'a :busy nil)
                                     (agentel-list-test-seen 'b :busy nil)
                                     '(shown (a))))
                 '(b))))

(ert-deftest agentel-list-update-keeps-sessions-in-the-order-they-appeared ()
  (let ((data (agentel-list-test-after (agentel-list-test-seen 'a)
                                       (agentel-list-test-seen 'b)
                                       (agentel-list-test-seen 'a :busy t))))
    (should (equal (mapcar #'car (alist-get 'sessions data)) '(a b)))))

(ert-deftest agentel-list-update-forgets-a-removed-session ()
  (let ((data (agentel-list-test-after
               (agentel-list-test-seen 'a :busy t)
               (agentel-list-test-seen 'a :busy nil)
               '(changed (:session a :live nil)))))
    (should-not (alist-get 'sessions data))
    (should-not (alist-get 'unread data))))

(ert-deftest agentel-list-update-leaves-the-data-it-was-given-alone ()
  (let* ((before (agentel-list-test-after (agentel-list-test-seen 'a :busy t)))
         (copy (copy-tree before)))
    (agentel-list--update (agentel-list-test-seen 'a :busy nil) before)
    (agentel-list--update '(shown (a)) before)
    (should (equal before copy))))

(defun agentel-list-test-view (&rest messages)
  "Return the lines of the list after MESSAGES, starting with no session."
  (split-string (substring-no-properties
                 (agentel-ui-view (agentel-store-model--make
                                   :type 'agentel-list
                                   :data (apply #'agentel-list-test-after messages))))
                "\n" t))

(ert-deftest agentel-list-view-shows-subagents-below-their-session ()
  (should (equal (agentel-list-test-view
                  (agentel-list-test-seen 'p :state 'idle :project "repo" :name "Fix"
                                          :titled t)
                  (agentel-list-test-seen 'o :state 'running :project "two" :name "two")
                  (agentel-list-test-seen 'c :state 'idle :name "Explore" :parent 'p))
                 '("  💤 repo" "    Fix" "    └ 💤 Explore" "  🏃 two"))))

(ert-deftest agentel-list-view-puts-waiting-then-unread-sessions-first ()
  (should (equal (agentel-list-test-view
                  (agentel-list-test-seen 'a :state 'idle :project "a")
                  (agentel-list-test-seen 'b :state 'running :project "b" :busy t)
                  (agentel-list-test-seen 'c :state 'waiting :project "c")
                  (agentel-list-test-seen 'b :state 'idle :project "b"))
                 '("  🙋 c" "● 💤 b" "  💤 a"))))

(ert-deftest agentel-list-view-marks-each-line-with-its-session ()
  (let ((text (agentel-ui-view
               (agentel-store-model--make
                :type 'agentel-list
                :data (agentel-list-test-after
                       (agentel-list-test-seen 'p :state 'idle :project "repo"
                                               :name "Fix" :titled t))))))
    (should (eq (get-text-property 0 'agentel-list-session text) 'p))
    (should (eq (get-text-property (1- (length text)) 'agentel-list-session text) 'p))))

(defun agentel-list-test-lines ()
  "Return the lines of the list."
  (split-string (buffer-substring-no-properties (point-min) (point-max))
                "\n" t))

(ert-deftest agentel-list-shows-the-project-then-the-title-of-a-session ()
  (agentel-list-test-with-sessions
    (should (equal (agentel-list-test-lines)
                   '("  💤 repo-one" "    Fix things"
                     "    └ 💤 Explore"
                     "  💤 two")))))

(ert-deftest agentel-list-shows-the-directory-of-a-session-outside-a-project ()
  (agentel-list-test-with-sessions
    (setf (agentel-session-title other) "Something")
    (agentel-session-changed other)
    (agentel-list--refresh)
    (should (equal (last (agentel-list-test-lines) 2)
                   '("  💤 two" "    Something")))))

(ert-deftest agentel-list-shows-when-a-subagent-waits-for-the-user ()
  (agentel-list-test-with-sessions
    (agentel-session-add-pending child 'question)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("  🙋 repo-one" "    Fix things"
                     "    └ 🙋 Explore"
                     "  💤 two")))))

(ert-deftest agentel-list-puts-sessions-waiting-for-the-user-first ()
  (agentel-list-test-with-sessions
    (agentel-session-add-pending other 'question)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("  🙋 two"
                     "  💤 repo-one" "    Fix things"
                     "    └ 💤 Explore")))))

(ert-deftest agentel-list-marks-a-session-whose-turn-ended-out-of-sight ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-session-set-busy other nil)
    (agentel-list--refresh)
    (should (equal (agentel-list-test-lines)
                   '("● 💤 two"
                     "  💤 repo-one" "    Fix things"
                     "    └ 💤 Explore")))))

(ert-deftest agentel-list-puts-waiting-sessions-before-unread-ones ()
  (agentel-list-test-with-sessions
    (agentel-session-set-busy other t)
    (agentel-session-set-busy other nil)
    (agentel-session-add-pending child 'question)
    (agentel-list--refresh)
    (should (equal (car (agentel-list-test-lines)) "  🙋 repo-one"))))

(ert-deftest agentel-list-does-not-mark-a-session-whose-turn-ended-in-sight ()
  (agentel-list-test-with-sessions
    (save-window-excursion
      (switch-to-buffer (agentel-session-buffer other))
      (agentel-session-set-busy other t)
      (agentel-session-set-busy other nil))
    (agentel-list--refresh)
    (should (equal (car (last (agentel-list-test-lines))) "  💤 two"))))

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
    (should (equal (car (last (agentel-list-test-lines))) "  💤 two"))))

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
    (should (search-forward "🏃 two" nil t))))

(ert-deftest agentel-list-stops-following-the-sessions-in-another-mode ()
  (agentel-list-test-with-sessions
    (should (memq #'agentel-list--schedule-refresh
                  (agentel-store-subscribers agentel-list--store)))
    (fundamental-mode)
    (should-not (memq #'agentel-list--schedule-refresh
                      (agentel-store-subscribers agentel-list--store)))))

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

(defun agentel-list-test-line ()
  "Return the line at point."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest agentel-list-moves-to-the-next-session-past-the-title-line ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (agentel-list-next)
    (should (bolp))
    (should (equal (agentel-list-test-line) "    └ 💤 Explore"))
    (agentel-list-next)
    (should (equal (agentel-list-test-line) "  💤 two"))))

(ert-deftest agentel-list-moves-to-the-first-line-of-the-previous-session ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (search-forward "two")
    (agentel-list-previous)
    (should (equal (agentel-list-test-line) "    └ 💤 Explore"))
    (agentel-list-previous)
    (should (bolp))
    (should (equal (agentel-list-test-line) "  💤 repo-one"))))

(ert-deftest agentel-list-moves-by-as-many-sessions-as-the-prefix ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (agentel-list-next 2)
    (should (equal (agentel-list-test-line) "  💤 two"))
    (agentel-list-next -2)
    (should (equal (agentel-list-test-line) "  💤 repo-one"))))

(ert-deftest agentel-list-stays-at-the-ends-of-the-list ()
  (agentel-list-test-with-sessions
    (goto-char (point-min))
    (agentel-list-previous)
    (should (equal (agentel-list-test-line) "  💤 repo-one"))
    (search-forward "two")
    (agentel-list-next)
    (should (equal (agentel-list-test-line) "  💤 two"))))

(ert-deftest agentel-list-binds-n-and-p-to-the-next-and-previous-session ()
  (should (eq (keymap-lookup agentel-list-mode-map "n") #'agentel-list-next))
  (should (eq (keymap-lookup agentel-list-mode-map "p") #'agentel-list-previous)))

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
