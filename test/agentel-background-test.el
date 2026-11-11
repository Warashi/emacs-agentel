;;; agentel-background-test.el --- Tests for agentel-background  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-background)
(require 'agentel-test-helper)

(defmacro agentel-background-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil))
     (let ((session (agentel-session-create)))
       (agentel-session-register session "s1")
       ,@body)))

(defun agentel-background-test-update (session &rest update)
  "Tell SESSION the `session/update' UPDATE, an alist."
  (agentel-session-dispatch `((sessionId . "s1") (update . ,update)))
  session)

(defun agentel-background-test-state (session &rest ids)
  "Tell SESSION that the tool calls IDS run in the background."
  (agentel-background-test-update
   session '(sessionUpdate . "state_update") '(state . "idle")
   `(background . ,(vconcat (mapcar (lambda (id) `((type . "tool_call") (toolCallId . ,id)))
                                    ids)))))

(defun agentel-background-test-titles (session)
  "Return the titles of the background work of SESSION."
  (when-let* ((model (agentel-store-find (agentel-session-store session) 'background)))
    (mapcar (lambda (item) (alist-get 'title (cdr item)))
            (agentel-store-get model 'items))))

(ert-deftest agentel-background-starts-with-the-title-the-tool-call-had ()
  (agentel-background-test-with-session
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1")
     '(name . "Workflow") '(title . "Workflow") '(status . "pending"))
    (agentel-background-test-state session "w1")
    (should (equal (agentel-background-test-titles session) '("Workflow")))))

(ert-deftest agentel-background-follows-the-title-of-its-tool-call ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "w1")
     '(title . "Workflow: research: sleep-0"))
    (should (equal (agentel-background-test-titles session)
                   '("Workflow: research: sleep-0")))))

(ert-deftest agentel-background-ignores-tool-calls-outside-the-background ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "tool_call_update") '(toolCallId . "t2") '(title . "Read"))
    (should (equal (agentel-background-test-titles session) '(nil)))))

(ert-deftest agentel-background-forgets-work-that-left-the-list ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-state session)
    (should-not (agentel-background-test-titles session))))

(ert-deftest agentel-background-keeps-the-list-when-the-agent-does-not-report-it ()
  (agentel-background-test-with-session
    (agentel-background-test-state session "w1")
    (agentel-background-test-update
     session '(sessionUpdate . "state_update") '(state . "running"))
    (should (equal (length (agentel-background-test-titles session)) 1))))

(ert-deftest agentel-background-ignores-work-of-unknown-types ()
  (agentel-background-test-with-session
    (agentel-background-test-update
     session '(sessionUpdate . "state_update") '(state . "idle")
     '(background . [((type . "subagent") (sessionId . "c1"))]))
    (should-not (agentel-background-test-titles session))))

(ert-deftest agentel-background-v2-workflow-is-pinned-until-it-ends ()
  (let ((agentel-acp-v2-agents '(mock)))
    (agentel-test-with-started session nil
      (agentel-test-send "workflow please")
      (agentel-test-wait-until
       (lambda () (string-match-p "Workflow: research: sleep-0"
                                  (or (overlay-get agentel-chat--pin 'before-string) ""))))
      (should (eq (agentel-session-state session) 'idle))
      (agentel-test-send "hello")
      (agentel-test-wait-for-text "Echo: hello")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (overlay-get agentel-chat--pin 'before-string)))))

(provide 'agentel-background-test)
;;; agentel-background-test.el ends here
