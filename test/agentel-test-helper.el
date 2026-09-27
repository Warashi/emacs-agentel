;;; agentel-test-helper.el --- Shared helpers for agentel tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel)

(defconst agentel-test-mock-agent
  (expand-file-name "agentel-mock-agent.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path of the scripted ACP agent used by the tests.")

(defun agentel-test-mock-command (&optional flavor)
  "Return the command line that runs the mock agent.
FLAVOR is the agent it mimics, see `agentel-mock-flavor'."
  (append (list (expand-file-name invocation-name invocation-directory)
                "--batch" "-Q")
          (when flavor
            (list "--eval" (format "(setq agentel-mock-flavor '%s)" flavor)))
          (list "-l" agentel-test-mock-agent)))

(defun agentel-test-wait-until (predicate &optional timeout)
  "Process output until PREDICATE returns non-nil or TIMEOUT seconds pass.
Return the value of PREDICATE, failing the test on timeout."
  (let ((deadline (+ (float-time) (or timeout 10)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (unless value
      (ert-fail "Timed out waiting for condition"))
    value))

(defmacro agentel-test-with-started (var options &rest body)
  "Start a session on the mock agent with OPTIONS, bind it to VAR, run BODY."
  (declare (indent 2))
  `(let* ((agentel-session--registry nil)
          (command (agentel-test-mock-command))
          (agentel-agents (cons (cons 'mock command) agentel-agents))
          (agentel-default-agent 'mock)
          (,var (apply #'agentel-start :cwd temporary-file-directory
                       :display nil ,options)))
     (unwind-protect
         (progn
           (agentel-test-wait-until
            (lambda () (eq (agentel-session-state ,var) 'idle)))
           (with-current-buffer (agentel-session-buffer ,var) ,@body))
       (when (buffer-live-p (agentel-session-buffer ,var))
         (kill-buffer (agentel-session-buffer ,var))))))

(defun agentel-test-send (text)
  "Type TEXT into the input area and send it."
  (goto-char (point-max))
  (insert text)
  (agentel-chat-send))

(defun agentel-test-wait-for-text (regexp)
  "Wait until the current buffer matches REGEXP."
  (agentel-test-wait-until
   (lambda () (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t)))))

(agentel-conversation-define 'agentel-test-question
  (lambda (message _data)
    (pcase message
      ('(ask) '((waiting . t)))
      ('(answer) '((answered . t))))))

(agentel-ui-define-view 'agentel-test-question
  (lambda (_item _options) "Question"))

(defun agentel-test-ask (session)
  "Make SESSION wait for the user to answer a question."
  (agentel-conversation-send session 'agentel-test-question 'agentel-test-question '(ask)))

(defun agentel-test-answer (session)
  "Answer the question SESSION waits for."
  (agentel-conversation-send session 'agentel-test-question 'agentel-test-question '(answer)))

(provide 'agentel-test-helper)
;;; agentel-test-helper.el ends here
