;;; agentel-conversation-test.el --- Tests for conversations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-conversation)

(defmacro agentel-conversation-test-with-session (&rest body)
  "Run BODY with a registered session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-conversation--on-update)))
     (let ((session (agentel-session-create :cwd "/tmp/project/")))
       (agentel-session-register session "s1")
       ,@body)))

(defun agentel-conversation-test-update (update)
  "Dispatch UPDATE to session s1."
  (agentel-session-dispatch `((sessionId . "s1") (update . ,update))))

(defun agentel-conversation-test-chunk (kind text)
  "Dispatch a text chunk of KIND with TEXT to session s1."
  (agentel-conversation-test-update
   `((sessionUpdate . ,kind) (content . ((type . "text") (text . ,text))))))

(defun agentel-conversation-test-items (session)
  "Return the items of the conversation of SESSION as (TYPE . DATA)."
  (mapcar (lambda (item)
            (cons (agentel-store-model-type item) (agentel-store-model-data item)))
          (agentel-conversation-items session)))

(ert-deftest agentel-conversation-joins-chunks-into-one-message ()
  (agentel-conversation-test-with-session
    (agentel-conversation-test-chunk "agent_message_chunk" "Hello")
    (agentel-conversation-test-chunk "agent_message_chunk" ", world")
    (should (equal (agentel-conversation-test-items session)
                   '((agent (text . "Hello, world")))))))

(ert-deftest agentel-conversation-separates-thoughts-from-messages ()
  (agentel-conversation-test-with-session
    (agentel-conversation-test-chunk "agent_thought_chunk" "Hmm")
    (agentel-conversation-test-chunk "agent_message_chunk" "Answer")
    (should (equal (mapcar #'car (agentel-conversation-test-items session))
                   '(thought agent)))))

(ert-deftest agentel-conversation-finishes-a-message-when-something-follows ()
  (agentel-conversation-test-with-session
    (agentel-conversation-test-chunk "agent_message_chunk" "Let me look.")
    (agentel-conversation-test-update '((sessionUpdate . "tool_call") (toolCallId . "t1")
                                        (title . "Read")))
    (should (agentel-store-get (car (agentel-conversation-items session)) 'finished))))

(ert-deftest agentel-conversation-finishes-the-last-message-at-the-end-of-a-turn ()
  (agentel-conversation-test-with-session
    (agentel-conversation-test-chunk "agent_message_chunk" "Done.")
    (agentel-conversation-finish-message session)
    (should (agentel-store-get (car (agentel-conversation-items session)) 'finished))))

(ert-deftest agentel-conversation-keeps-one-item-per-tool-call ()
  (agentel-conversation-test-with-session
    (agentel-conversation-test-update '((sessionUpdate . "tool_call") (toolCallId . "t1")
                                        (title . "Read") (status . "pending")))
    (agentel-conversation-test-update '((sessionUpdate . "tool_call_update")
                                        (toolCallId . "t1")
                                        (status . "completed")))
    (should (equal (agentel-conversation-test-items session)
                   '((tool (status . "completed") (title . "Read")))))))

(ert-deftest agentel-conversation-replaces-the-plan ()
  (agentel-conversation-test-with-session
    (agentel-conversation-test-update '((sessionUpdate . "plan") (entries . [a])))
    (agentel-conversation-test-update '((sessionUpdate . "plan") (entries . [b])))
    (should (equal (agentel-conversation-test-items session)
                   '((plan (entries . [b])))))))

(ert-deftest agentel-conversation-records-prompts-and-notes ()
  (agentel-conversation-test-with-session
    (agentel-conversation-prompt session "hi")
    (agentel-conversation-note session "Turn ended: cancelled" 'stop)
    (agentel-conversation-note session "Agent exited")
    (should (equal (agentel-conversation-test-items session)
                   '((user (text . "hi"))
                     (stop (text . "Turn ended: cancelled"))
                     (notice (text . "Agent exited")))))))

(agentel-conversation-define 'agentel-conversation-test-counter
  (lambda (message data)
    (pcase message
      (`(add ,n) `((count . ,(+ n (or (alist-get 'count data) 0))))))))

(ert-deftest agentel-conversation-send-keeps-one-item-per-key ()
  (agentel-conversation-test-with-session
    (agentel-conversation-send session 'c 'agentel-conversation-test-counter '(add 1))
    (agentel-conversation-send session 'c 'agentel-conversation-test-counter '(add 2))
    (should (equal (agentel-store-get (agentel-conversation-find session 'c) 'count) 3))
    (should (= (length (agentel-conversation-items session)) 1))))

(ert-deftest agentel-conversation-tells-subscribers-of-changes ()
  (agentel-conversation-test-with-session
    (let* (told
           (subscriber (lambda (item added)
                         (push (cons (agentel-store-model-type item) added) told))))
      (agentel-conversation-subscribe session subscriber)
      (agentel-conversation-note session "one")
      (agentel-conversation-unsubscribe session subscriber)
      (agentel-conversation-note session "two")
      (should (equal told '((notice . t)))))))

(provide 'agentel-conversation-test)
;;; agentel-conversation-test.el ends here
