;;; agentel-pending-test.el --- Tests for agentel-pending  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-pending)
(require 'agentel-test-helper)

(defmacro agentel-pending-test-with-sessions (&rest body)
  "Run BODY with the sessions `one', `two' and `three', none shown."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-list--store (agentel-store-create))
         (agentel-session-changed-functions (list #'agentel-list--on-changed)))
     (let ((one (agentel-session-create :cwd "/tmp/one/"))
           (two (agentel-session-create :cwd "/tmp/two/"))
           (three (agentel-session-create :cwd "/tmp/three/")))
       (agentel-session-register one "1")
       (agentel-session-register two "2")
       (agentel-session-register three "3")
       (dolist (s (list one two three))
         (agentel-chat-open s :input t))
       (unwind-protect
           (save-window-excursion
             (with-temp-buffer
               (switch-to-buffer (current-buffer))
               ,@body))
         (dolist (s (list one two three))
           (kill-buffer (agentel-session-buffer s)))))))

(ert-deftest agentel-pending-next-visits-the-first-session-that-needs-the-user ()
  (agentel-pending-test-with-sessions
    (agentel-session-send two '(start-turn))
    (agentel-session-send two '(finish-turn))
    (agentel-test-ask three)
    (agentel-pending-next)
    (should (eq (window-buffer) (agentel-session-buffer three)))))

(ert-deftest agentel-pending-next-visits-the-one-after-the-current-session ()
  (agentel-pending-test-with-sessions
    (agentel-test-ask one)
    (agentel-test-ask three)
    (switch-to-buffer (agentel-session-buffer one))
    (agentel-pending-next)
    (should (eq (window-buffer) (agentel-session-buffer three)))))

(ert-deftest agentel-pending-next-goes-back-to-the-first-after-the-last ()
  (agentel-pending-test-with-sessions
    (agentel-test-ask one)
    (agentel-test-ask three)
    (switch-to-buffer (agentel-session-buffer three))
    (agentel-pending-next)
    (should (eq (window-buffer) (agentel-session-buffer one)))))

(ert-deftest agentel-pending-next-shows-the-session-in-the-selected-window ()
  (agentel-pending-test-with-sessions
    (agentel-test-ask two)
    (let ((window (selected-window)))
      (agentel-pending-next)
      (should (eq (selected-window) window))
      (should (eq (window-buffer window) (agentel-session-buffer two))))))

(ert-deftest agentel-pending-next-stays-when-no-session-needs-the-user ()
  (agentel-pending-test-with-sessions
    (let ((buffer (current-buffer)))
      (agentel-pending-next)
      (should (eq (window-buffer) buffer)))))

(ert-deftest agentel-pending-next-stays-when-only-the-current-session-needs-the-user ()
  (agentel-pending-test-with-sessions
    (agentel-test-ask two)
    (switch-to-buffer (agentel-session-buffer two))
    (agentel-pending-next)
    (should (eq (window-buffer) (agentel-session-buffer two)))))

(ert-deftest agentel-pending-next-is-bound-in-the-session-buffer ()
  (should (eq (keymap-lookup agentel-chat-mode-map "C-c C-n")
              #'agentel-pending-next)))

;;; agentel-pending-test.el ends here
