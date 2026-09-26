;;; agentel-copilot-subagent-test.el --- Tests for agentel-copilot-subagent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-copilot-subagent)
(require 'agentel-test-helper)

(defmacro agentel-copilot-subagent-test-with-copilot (var &rest body)
  "Start a session on the mock of Copilot, bind it to VAR, run BODY."
  (declare (indent 1))
  `(let ((agentel-agents (list (cons 'copilot (agentel-test-mock-command 'copilot)))))
     (agentel-test-with-started ,var '(:agent copilot) ,@body)))

(defun agentel-copilot-subagent-test-run (session prompt)
  "Send PROMPT and return the transcript of SESSION after the turn."
  (agentel-test-send prompt)
  (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest agentel-copilot-subagent-tool-calls-stay-out-of-the-parent ()
  (agentel-copilot-subagent-test-with-copilot session
    (let ((text (agentel-copilot-subagent-test-run session "subagent please")))
      (should (string-match-p "Explore the repository" text))
      (should-not (string-match-p "Viewing /tmp/README\\.org" text)))))

(ert-deftest agentel-copilot-subagent-background-tool-calls-stay-out-of-the-parent ()
  (agentel-copilot-subagent-test-with-copilot session
    (let ((text (agentel-copilot-subagent-test-run
                 session "background subagent please")))
      (should (string-match-p "Waiting for it\\." text))
      (should-not (string-match-p "Viewing /tmp/README\\.org" text)))))

(provide 'agentel-copilot-subagent-test)
;;; agentel-copilot-subagent-test.el ends here
