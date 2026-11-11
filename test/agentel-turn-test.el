;;; agentel-turn-test.el --- Tests for agentel-turn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-turn)

(defvar agentel-turn-test-version 1
  "ACP version of the connection of the session of a test.")

(defmacro agentel-turn-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'.
Its connection speaks `agentel-turn-test-version'.  Requests to the
agent are pushed to `sent' as (METHOD PARAMS ON-SUCCESS ON-FAILURE)
instead of being sent, newest first."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (sent nil))
     (cl-letf (((symbol-function 'agentel-connection-request)
                (cl-function
                 (lambda (_connection method params &key on-success on-failure)
                   (push (list method params on-success on-failure) sent))))
               ((symbol-function 'agentel-connection-notify)
                (lambda (_connection method params)
                  (push (list method params) sent))))
       (let ((session (agentel-session-create
                       :cwd "/tmp/project/"
                       :connection (agentel-connection--make
                                    :info `((protocolVersion . ,agentel-turn-test-version))))))
         (agentel-session-register session "s1")
         ,@body))))

(defun agentel-turn-test-items (session)
  "Return the items of the conversation of SESSION as (TYPE . DATA)."
  (mapcar (lambda (item)
            (cons (agentel-store-model-type item) (agentel-store-model-data item)))
          (agentel-conversation-items session)))

(defun agentel-turn-test-agent-says (session text)
  "Make the agent of SESSION say TEXT."
  (agentel-conversation--on-update
   session `((sessionUpdate . "agent_message_chunk")
             (content . ((type . "text") (text . ,text))))))

(ert-deftest agentel-turn-prompt-records-the-prompt-and-runs-the-session ()
  (agentel-turn-test-with-session
    (agentel-turn-prompt session "hello")
    (should (equal (agentel-turn-test-items session) '((user (text . "hello")))))
    (should (agentel-session-in-turn session))))

(ert-deftest agentel-turn-prompt-sends-the-text-to-the-agent ()
  (agentel-turn-test-with-session
    (agentel-turn-prompt session "hello")
    (should (equal (seq-take (car sent) 2)
                   '("session/prompt"
                     ((sessionId . "s1")
                      (prompt . [((type . "text") (text . "hello"))])))))))

(ert-deftest agentel-turn-ends-when-the-agent-answers ()
  (agentel-turn-test-with-session
    (agentel-turn-prompt session "hello")
    (agentel-turn-test-agent-says session "Hi")
    (funcall (nth 2 (car sent)) '((stopReason . "end_turn")))
    (should-not (agentel-session-in-turn session))
    (let ((reply (car (last (agentel-conversation-items session)))))
      (should (equal (agentel-store-get reply 'text) "Hi"))
      (should (agentel-store-get reply 'finished)))
    (should (equal (length (agentel-conversation-items session)) 2))))

(ert-deftest agentel-turn-tells-why-it-ended-early ()
  (agentel-turn-test-with-session
    (agentel-turn-prompt session "hello")
    (funcall (nth 2 (car sent)) '((stopReason . "max_tokens")))
    (should-not (agentel-session-in-turn session))
    (should (equal (car (last (agentel-turn-test-items session)))
                   '(stop (text . "Turn ended: max_tokens"))))))

(ert-deftest agentel-turn-tells-why-the-prompt-failed ()
  (agentel-turn-test-with-session
    (agentel-turn-prompt session "hello")
    (funcall (nth 3 (car sent)) '((message . "boom")))
    (should-not (agentel-session-in-turn session))
    (should (equal (car (last (agentel-turn-test-items session)))
                   '(error (text . "Prompt failed: boom"))))))

(defun agentel-turn-test-state (session state &rest fields)
  "Tell SESSION that its agent is in STATE, with the alist FIELDS."
  (agentel-session-dispatch
   `((sessionId . ,(agentel-session-id session))
     (update . ((sessionUpdate . "state_update") (state . ,state) ,@fields)))
   (agentel-session-connection session)))

(ert-deftest agentel-turn-v2-goes-on-after-the-agent-takes-the-prompt ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (agentel-turn-prompt session "hello")
      (funcall (nth 2 (car sent)) '((messageId . "m1")))
      (should (agentel-session-in-turn session)))))

(ert-deftest agentel-turn-v2-ends-when-the-agent-becomes-idle ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (agentel-turn-prompt session "hello")
      (funcall (nth 2 (car sent)) '((messageId . "m1")))
      (agentel-turn-test-agent-says session "Hi")
      (agentel-turn-test-state session "idle" '(stopReason . "end_turn"))
      (should-not (agentel-session-in-turn session))
      (let ((reply (car (last (agentel-conversation-items session)))))
        (should (equal (agentel-store-get reply 'text) "Hi"))
        (should (agentel-store-get reply 'finished)))
      (should (equal (length (agentel-conversation-items session)) 2)))))

(ert-deftest agentel-turn-v2-goes-on-when-only-the-background-work-ends ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (agentel-turn-prompt session "hello")
      (agentel-turn-test-state session "idle" '(background . []))
      (should (agentel-session-in-turn session)))))

(ert-deftest agentel-turn-v2-tells-why-it-ended-early ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (agentel-turn-prompt session "hello")
      (agentel-turn-test-state session "idle" '(stopReason . "max_tokens"))
      (should (equal (car (last (agentel-turn-test-items session)))
                     '(stop (text . "Turn ended: max_tokens")))))))

(ert-deftest agentel-turn-v2-tells-why-the-turn-failed ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (agentel-turn-prompt session "hello")
      (agentel-turn-test-state session "idle" '(stopReason . "error")
                               '(error . ((code . -32000) (message . "boom"))))
      (should-not (agentel-session-in-turn session))
      (should (equal (car (last (agentel-turn-test-items session)))
                     '(error (text . "Prompt failed: boom")))))))

(ert-deftest agentel-turn-v2-runs-a-turn-the-agent-starts-itself ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (agentel-turn-test-state session "running")
      (should (agentel-session-in-turn session))
      (agentel-turn-test-agent-says session "Done")
      (agentel-turn-test-state session "idle" '(stopReason . "end_turn"))
      (should-not (agentel-session-in-turn session))
      (should (agentel-store-get (car (last (agentel-conversation-items session)))
                                 'finished)))))

(ert-deftest agentel-turn-v2-leaves-the-turns-of-subagents-to-their-parent ()
  (let ((agentel-turn-test-version 2))
    (agentel-turn-test-with-session
      (let ((child (agentel-session-create :parent session
                                           :connection (agentel-session-connection session))))
        (agentel-session-register child "c1")
        (agentel-turn-test-state child "running")
        (should-not (agentel-session-in-turn child))))))

(ert-deftest agentel-turn-v1-leaves-the-turn-to-the-answer-of-the-prompt ()
  (agentel-turn-test-with-session
    (agentel-turn-prompt session "hello")
    (agentel-turn-test-state session "idle" '(stopReason . "end_turn"))
    (should (agentel-session-in-turn session))))

(ert-deftest agentel-turn-cancel-asks-the-agent-to-stop ()
  (agentel-turn-test-with-session
    (agentel-turn-cancel session)
    (should (equal sent '(("session/cancel" ((sessionId . "s1"))))))))

(provide 'agentel-turn-test)
;;; agentel-turn-test.el ends here
