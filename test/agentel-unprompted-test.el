;;; agentel-unprompted-test.el --- Tests for agentel-unprompted  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-unprompted)
(require 'agentel-test-helper)

(defmacro agentel-unprompted-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-running-functions (list #'agentel-unprompted--running-p))
         (agentel-session-update-functions (list #'agentel-conversation--on-update
                                                 #'agentel-unprompted--on-update)))
     (let ((session (agentel-session-create :cwd "/tmp/project/")))
       (agentel-session-register session "s1")
       ,@body)))

(defun agentel-unprompted-test-update (update &optional id)
  "Dispatch UPDATE to the session ID, s1 by default."
  (agentel-session-dispatch `((sessionId . ,(or id "s1")) (update . ,update))))

(defun agentel-unprompted-test-say (text)
  "Dispatch the agent message chunk TEXT to session s1."
  (agentel-unprompted-test-update
   `((sessionUpdate . "agent_message_chunk") (content . ((type . "text") (text . ,text))))))

(defun agentel-unprompted-test-usage ()
  "Dispatch a usage report to session s1."
  (agentel-unprompted-test-update '((sessionUpdate . "usage_update") (used . 1) (size . 10))))

(ert-deftest agentel-unprompted-runs-while-the-agent-works-without-a-prompt ()
  (agentel-unprompted-test-with-session
    (agentel-unprompted-test-say "A message arrived.")
    (should (eq (agentel-session-state session) 'running))))

(ert-deftest agentel-unprompted-runs-for-thoughts-and-new-tool-calls ()
  (dolist (update '(((sessionUpdate . "agent_thought_chunk")
                     (content . ((type . "text") (text . "Hmm"))))
                    ((sessionUpdate . "tool_call") (toolCallId . "t1") (title . "Read"))))
    (agentel-unprompted-test-with-session
      (agentel-unprompted-test-update update)
      (should (eq (agentel-session-state session) 'running)))))

(ert-deftest agentel-unprompted-stays-idle-for-background-news ()
  (agentel-unprompted-test-with-session
    (agentel-unprompted-test-update '((sessionUpdate . "tool_call_update") (toolCallId . "t1")
                                      (status . "completed")))
    (agentel-unprompted-test-update '((sessionUpdate . "subagent_update") (sessionId . "c1")))
    (agentel-unprompted-test-usage)
    (should (eq (agentel-session-state session) 'idle))))

(ert-deftest agentel-unprompted-ends-at-the-usage-of-the-turn ()
  (agentel-unprompted-test-with-session
    (agentel-unprompted-test-say "Woke up.")
    (agentel-unprompted-test-usage)
    (should (eq (agentel-session-state session) 'idle))
    (should (agentel-store-get (car (last (agentel-conversation-items session))) 'finished))))

(ert-deftest agentel-unprompted-leaves-the-message-of-a-prompt-to-its-turn ()
  (agentel-unprompted-test-with-session
    (agentel-session-send session '(start-turn))
    (agentel-unprompted-test-say "Working on it.")
    (agentel-unprompted-test-usage)
    (should (eq (agentel-session-state session) 'running))
    (should-not (agentel-store-get (car (last (agentel-conversation-items session)))
                                   'finished))
    (agentel-session-send session '(finish-turn))
    (should (eq (agentel-session-state session) 'idle))))

(ert-deftest agentel-unprompted-ignores-a-session-that-loads ()
  (agentel-unprompted-test-with-session
    (agentel-session-send session '(start-loading))
    (agentel-unprompted-test-say "Earlier answer.")
    (agentel-session-send session '(finish-loading))
    (should (eq (agentel-session-state session) 'idle))))

(ert-deftest agentel-unprompted-ignores-subagents ()
  (agentel-unprompted-test-with-session
    (let ((child (agentel-session-create :parent session)))
      (agentel-session-register child "c1")
      (agentel-unprompted-test-update
       '((sessionUpdate . "agent_message_chunk") (content . ((type . "text") (text . "Hi"))))
       "c1")
      (should-not (agentel-unprompted--running-p child)))))

(ert-deftest agentel-unprompted-tells-listeners-when-it-starts-and-ends ()
  (agentel-unprompted-test-with-session
    (let* ((told 0)
           (agentel-session-changed-functions (list (lambda (_session) (cl-incf told)))))
      (agentel-unprompted-test-say "One")
      (agentel-unprompted-test-say " two")
      (should (= told 1))
      (agentel-unprompted-test-usage)
      (should (= told 2)))))

(ert-deftest agentel-unprompted-shows-a-turn-of-the-agent-as-its-own-message ()
  (agentel-test-with-started session nil
    (let* (states
           (agentel-session-changed-functions
            (cons (lambda (s) (when (eq s session) (push (agentel-session-state s) states)))
                  agentel-session-changed-functions)))
      (agentel-test-send "wakeup")
      (agentel-test-wait-until
       (lambda () (and (eq (agentel-session-state session) 'idle)
                       (memq 'running (cdr (memq 'idle states))))))
      (let ((last (car (last (agentel-conversation-items session)))))
        (should (string-prefix-p "## Plan" (agentel-store-get last 'text)))
        (should (agentel-store-get last 'finished)))
      (agentel-test-wait-for-text "^I will wait\\.\n\n## Plan$"))))

(provide 'agentel-unprompted-test)
;;; agentel-unprompted-test.el ends here
