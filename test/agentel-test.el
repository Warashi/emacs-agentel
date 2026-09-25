;;; agentel-test.el --- End to end tests for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel)
(require 'agentel-test-helper)

(ert-deftest agentel-start-opens-a-session-buffer ()
  (agentel-test-with-started session nil
    (should (string-prefix-p "mock-" (agentel-session-id session)))
    (should (eq major-mode 'agentel-chat-mode))))

(ert-deftest agentel-prompt-shows-reply-and-returns-to-idle ()
  (agentel-test-with-started session nil
    (agentel-test-send "hello")
    (should (eq (agentel-session-state session) 'running))
    (agentel-test-wait-for-text "Echo: hello")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (should (equal (agentel-chat-input) ""))))

(ert-deftest agentel-prompt-failure-is-shown ()
  (agentel-test-with-started session nil
    (agentel-test-send "fail")
    (agentel-test-wait-for-text "Prompt failed: Authentication required")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))))

(ert-deftest agentel-cancel-ends-the-turn ()
  (agentel-test-with-started session nil
    (agentel-test-send "slow")
    (agentel-test-wait-for-text "Working slowly")
    (agentel-chat-cancel)
    (agentel-test-wait-for-text "Turn ended: cancelled")
    (should (eq (agentel-chat-entry-type (agentel-chat-entry-at (match-beginning 0)))
                'stop))))

(ert-deftest agentel-killing-the-buffer-stops-the-agent ()
  (agentel-test-with-started session nil
    (let ((process (agentel-connection-process (agentel-session-connection session))))
      (kill-buffer (current-buffer))
      (agentel-test-wait-until (lambda () (not (process-live-p process))))
      (should-not (memq session (agentel-session-list))))))

(ert-deftest agentel-command-prefix-wraps-the-agent ()
  (let ((agentel-command-prefix '("env" "AGENTEL_WRAPPED=1")))
    (agentel-test-with-started session nil
      (let ((command (process-command
                      (agentel-connection-process (agentel-session-connection session)))))
        (should (equal (seq-take command 3)
                       (list "env" "AGENTEL_WRAPPED=1" agentel-command))))
      (agentel-test-send "hello")
      (agentel-test-wait-for-text "Echo: hello"))))

(ert-deftest agentel-command-prefix-function-gets-the-session-directory ()
  (let* ((directories nil)
         (agentel-command-prefix (lambda (cwd)
                                   (push cwd directories)
                                   '("env" "AGENTEL_WRAPPED=1"))))
    (agentel-test-with-started session nil
      (should (equal directories (list (agentel-session-cwd session))))
      (should (equal (seq-take (process-command
                                (agentel-connection-process
                                 (agentel-session-connection session)))
                               2)
                     '("env" "AGENTEL_WRAPPED=1"))))))

(ert-deftest agentel-shows-why-the-agent-exited ()
  (let* ((agentel-session--registry nil)
         (agentel-command "sh")
         (agentel-command-args '("-c" "echo 'Not logged in' >&2; exit 3"))
         (session (agentel-start :cwd temporary-file-directory :display nil)))
    (unwind-protect
        (with-current-buffer (agentel-session-buffer session)
          (agentel-test-wait-until
           (lambda () (memq (agentel-session-state session) '(failed exited))))
          (agentel-test-wait-for-text "Agent exited")
          (agentel-test-wait-for-text "Not logged in"))
      (kill-buffer (agentel-session-buffer session)))))

(ert-deftest agentel-ended-session-refuses-input ()
  (agentel-test-with-started session nil
    (delete-process (agentel-connection-process (agentel-session-connection session)))
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'exited)))
    (goto-char (point-max))
    (insert "hello")
    (should-error (agentel-chat-send) :type 'user-error)
    (should (equal (agentel-chat-input) "hello"))))

(ert-deftest agentel-start-loads-a-previous-session ()
  (agentel-test-with-started session '(:session-id "old-1")
    (should (equal (agentel-session-id session) "old-1"))
    (agentel-test-wait-for-text "The plan was to write tests first.")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (goto-char (point-min))
    (search-forward "The plan was")
    (should (agentel-chat-entry-get (agentel-chat-entry-at) 'finished))
    (should-not (save-excursion (goto-char (point-min))
                                (search-forward "Replayed subagent" nil t)))))

(provide 'agentel-test)
;;; agentel-test.el ends here
