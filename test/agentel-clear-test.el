;;; agentel-clear-test.el --- Tests for agentel-clear  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-clear)
(require 'agentel-test-helper)

(ert-deftest agentel-clear-starts-over-with-the-same-settings ()
  (agentel-test-with-started session '(:model "haiku" :mode "plan")
    (agentel-test-wait-until
     (lambda () (equal (alist-get 'currentValue (agentel-config-option session "mode"))
                       "plan")))
    (agentel-test-send "hello")
    (agentel-test-wait-for-text "Echo: hello")
    (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
    (let ((old-buffer (current-buffer))
          (process (agentel-connection-process (agentel-session-connection session)))
          cleared)
      (save-window-excursion
        (switch-to-buffer old-buffer)
        (agentel-test-send "/clear")
        (setq cleared (car (agentel-session-roots)))
        (unwind-protect
            (progn
              (should-not (eq cleared session))
              (should (eq (window-buffer) (agentel-session-buffer cleared)))
              (should-not (buffer-live-p old-buffer))
              (agentel-test-wait-until (lambda () (not (process-live-p process))))
              (should-not (memq session (agentel-session-list)))
              (agentel-test-wait-until
               (lambda () (and (eq (agentel-session-state cleared) 'idle)
                               (equal (alist-get 'currentValue
                                                 (agentel-config-option cleared "mode"))
                                      "plan"))))
              (should (equal (agentel-session-cwd cleared) (agentel-session-cwd session)))
              (should (equal (alist-get 'currentValue
                                        (agentel-config-option cleared "model"))
                             "haiku"))
              (with-current-buffer (agentel-session-buffer cleared)
                (should-not (save-excursion (goto-char (point-min))
                                            (search-forward "Echo: hello" nil t)))
                (should-not (save-excursion (goto-char (point-min))
                                            (search-forward "not available" nil t)))))
          (kill-buffer (agentel-session-buffer cleared)))))))

(provide 'agentel-clear-test)
;;; agentel-clear-test.el ends here
