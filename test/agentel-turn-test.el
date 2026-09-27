;;; agentel-turn-test.el --- Tests for agentel-turn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-turn)

(defmacro agentel-turn-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'.
Requests to the agent are pushed to `sent' as (METHOD PARAMS
ON-SUCCESS ON-FAILURE) instead of being sent, newest first."
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
       (let ((session (agentel-session-create :cwd "/tmp/project/")))
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

(ert-deftest agentel-turn-cancel-asks-the-agent-to-stop ()
  (agentel-turn-test-with-session
    (agentel-turn-cancel session)
    (should (equal sent '(("session/cancel" ((sessionId . "s1"))))))))

(provide 'agentel-turn-test)
;;; agentel-turn-test.el ends here
