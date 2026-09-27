;;; agentel-focus-test.el --- Tests for agentel-focus  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-focus)

(defmacro agentel-focus-test-with-session (&rest body)
  "Run BODY in a focused chat buffer of a session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-conversation--on-update)))
     (let* ((session (agentel-session-create :cwd "/tmp/project/"))
            (buffer (agentel-chat-open session :input t)))
       (agentel-session-register session "s1")
       (unwind-protect
           (with-current-buffer buffer
             (agentel-focus-mode)
             ,@body)
         (kill-buffer buffer)))))

(defun agentel-focus-test-chunk (kind text)
  "Dispatch a text chunk of KIND with TEXT to session s1."
  (agentel-session-dispatch
   `((sessionId . "s1")
     (update . ((sessionUpdate . ,kind) (content . ((type . "text") (text . ,text))))))))

(defun agentel-focus-test-tool (id title)
  "Dispatch the tool call ID with TITLE to session s1."
  (agentel-session-dispatch
   `((sessionId . "s1")
     (update . ((sessionUpdate . "tool_call") (toolCallId . ,id)
                (title . ,title) (status . "completed"))))))

(defun agentel-focus-test-prompt (session text)
  "Show TEXT as a prompt sent to SESSION, which starts a turn."
  (agentel-conversation-send session nil 'user `(chunk ,text)))

(agentel-conversation-define 'agentel-focus-test-question
  (lambda (message _data)
    (pcase message
      (`(show ,text ,waiting) `((text . ,text) (waiting . ,waiting))))))
(agentel-ui-define-view 'agentel-focus-test-question
  (lambda (entry _options) (agentel-store-get entry 'text)))

(defun agentel-focus-test-question (session key text waiting)
  "Show TEXT as the question KEY of SESSION, WAITING for an answer or not."
  (agentel-conversation-send session key 'agentel-focus-test-question
                             `(show ,text ,waiting)))

(defun agentel-focus-test-visible ()
  "Return the text of the current buffer that is shown."
  (let ((text ""))
    (dotimes (i (- (point-max) (point-min)))
      (let ((pos (+ (point-min) i)))
        (unless (invisible-p pos)
          (setq text (concat text (string (char-after pos)))))))
    text))

(defun agentel-focus-test-a-finished-turn (session)
  "Play a turn of SESSION that ends with an answer."
  (agentel-focus-test-prompt session "first prompt")
  (agentel-focus-test-chunk "agent_message_chunk" "first answer")
  (agentel-focus-test-prompt session "second prompt")
  (agentel-focus-test-chunk "agent_thought_chunk" "pondering")
  (agentel-focus-test-chunk "agent_message_chunk" "Let me look.")
  (agentel-focus-test-tool "t1" "Read README.org")
  (agentel-conversation-note session "a notice")
  (agentel-focus-test-chunk "agent_message_chunk" "second answer"))

(defun agentel-focus-test-item (type &optional data)
  "Return an item of TYPE with DATA."
  (agentel-store-model--make :type type :data data))

(ert-deftest agentel-focus-items-chooses-from-the-items-alone ()
  (let* ((old (agentel-focus-test-item 'user))
         (old-error (agentel-focus-test-item 'error))
         (prompt (agentel-focus-test-item 'user))
         (message (agentel-focus-test-item 'agent))
         (tool (agentel-focus-test-item 'tool))
         (question (agentel-focus-test-item 'permission '((waiting . t))))
         (answered (agentel-focus-test-item 'permission '((waiting))))
         (thought (agentel-focus-test-item 'thought))
         (stop (agentel-focus-test-item 'stop)))
    (should (equal (agentel-focus-items
                    (list old old-error prompt message tool question answered thought stop))
                   (list prompt message question thought stop)))))

(ert-deftest agentel-focus-items-without-a-prompt-chooses-from-all ()
  (let ((notice (agentel-focus-test-item 'notice))
        (message (agentel-focus-test-item 'agent)))
    (should (equal (agentel-focus-items (list notice message)) (list message)))))

(ert-deftest agentel-focus-shows-the-last-prompt-and-answer-when-idle ()
  (agentel-focus-test-with-session
    (agentel-focus-test-a-finished-turn session)
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "second prompt" text))
      (should (string-match-p "second answer" text))
      (dolist (hidden '("first prompt" "first answer" "Thinking" "Let me look"
                        "Read README" "a notice"))
        (should-not (string-match-p hidden text)))
      (should (string-prefix-p "❯ second prompt" text)))))

(ert-deftest agentel-focus-shows-the-latest-tool-call-while-running ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-tool "t1" "Read a.el")
    (agentel-focus-test-tool "t2" "Read b.el")
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "do it" text))
      (should (string-match-p "Read b\\.el" text))
      (should-not (string-match-p "Read a\\.el" text)))))

(ert-deftest agentel-focus-shows-the-latest-thought-while-running ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-chunk "agent_thought_chunk" "Where to start")
    (should (string-match-p "Thinking: Where to start" (agentel-focus-test-visible)))
    (agentel-focus-test-tool "t1" "Read a.el")
    (agentel-focus-test-chunk "agent_thought_chunk" "What next")
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "Thinking: What next" text))
      (should-not (string-match-p "Read a\\.el\\|Where to start" text)))))

(ert-deftest agentel-focus-shows-questions-until-they-are-answered ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-question session 'q1 "Allow this?" t)
    (agentel-focus-test-tool "t1" "Read a.el")
    (should (string-match-p "Allow this\\?" (agentel-focus-test-visible)))
    (agentel-focus-test-question session 'q1 "Allow this?" nil)
    (should-not (string-match-p "Allow this\\?" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-shows-an-item-once-it-waits ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "delegate")
    (agentel-focus-test-question session 'sub "the subagent" nil)
    (agentel-focus-test-tool "t1" "Read a.el")
    (should-not (string-match-p "the subagent" (agentel-focus-test-visible)))
    (agentel-focus-test-question session 'sub "the subagent" t)
    (should (string-match-p "the subagent" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-shows-errors-of-the-last-turn ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-conversation-note session "Prompt failed: boom" 'error)
    (should (string-match-p "Prompt failed: boom" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-shows-why-the-last-turn-ended ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-chunk "agent_message_chunk" "Half an answer")
    (agentel-conversation-note session "Turn ended: max_tokens" 'stop)
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "Half an answer" text))
      (should (string-match-p "Turn ended: max_tokens" text)))))

(ert-deftest agentel-focus-shows-everything-when-turned-off ()
  (agentel-focus-test-with-session
    (agentel-focus-test-a-finished-turn session)
    (agentel-focus-mode -1)
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "first prompt" text))
      (should (string-match-p "Read README" text)))))

(ert-deftest agentel-focus-keeps-the-input-visible ()
  (agentel-focus-test-with-session
    (agentel-focus-test-a-finished-turn session)
    (goto-char (point-max))
    (insert "next question")
    (should (string-suffix-p "❯ next question" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-can-be-turned-on-in-a-running-session ()
  (agentel-focus-test-with-session
    (agentel-focus-mode -1)
    (agentel-focus-test-a-finished-turn session)
    (agentel-focus-mode)
    (let ((text (agentel-focus-test-visible)))
      (should (string-prefix-p "❯ second prompt" text))
      (should (string-match-p "second answer" text))
      (should-not (string-match-p "Read README" text)))))

(ert-deftest agentel-focus-keeps-updated-tool-calls-in-place ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-tool "t1" "Read a.el")
    (agentel-focus-test-tool "t2" "Read b.el")
    (agentel-focus-test-tool "t3" "Read c.el")
    ;; Updating a hidden tool call between hidden and shown ones.
    (agentel-focus-test-tool "t2" "Read b.el again")
    (agentel-focus-test-tool "t1" "Read a.el again")
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "\\`❯ do it\n\n.*Read c\\.el\n\n❯ \\'" text)))
    (agentel-focus-test-tool "t3" "Read c.el again")
    (should (string-match-p "\\`❯ do it\n\n.*Read c\\.el again\n\n❯ \\'"
                            (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-keeps-earlier-turns-hidden-when-they-change ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "first")
    (agentel-focus-test-tool "t1" "Read a.el")
    (agentel-focus-test-prompt session "second")
    (agentel-focus-test-tool "t1" "Read a.el later")
    (should-not (string-match-p "Read a\\.el" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-shows-the-last-message-while-running ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-chunk "agent_message_chunk" "Let me look.")
    (agentel-focus-test-tool "t1" "Read a.el")
    (should (string-match-p "\\`❯ do it\n\nLet me look\\.\n\n.*Read a\\.el\n\n❯ \\'"
                            (agentel-focus-test-visible)))
    (agentel-focus-test-chunk "agent_message_chunk" "Some ")
    (agentel-focus-test-chunk "agent_message_chunk" "text")
    (should (string-match-p "\\`❯ do it\n\nSome text\n\n❯ \\'"
                            (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-shows-a-tool-call-that-ended-the-turn ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-chunk "agent_message_chunk" "Let me look.")
    (agentel-focus-test-tool "t1" "Read a.el")
    (agentel-chat--finish-turn session)
    (should (string-match-p "\\`❯ do it\n\nLet me look\\.\n\n.*Read a\\.el\n\n❯ \\'"
                            (agentel-focus-test-visible)))))

(defun agentel-focus-test-random-step (session state)
  "Change SESSION in one random way.
STATE is a plist of the tool calls and questions made and the
questions open."
  (let ((tools (or (plist-get state :tools) 0))
        (questions (or (plist-get state :questions) 0))
        (items (plist-get state :items)))
    (pcase (random 8)
      (0 (agentel-focus-test-prompt session "p"))
      (1 (agentel-focus-test-chunk "agent_message_chunk" "m"))
      (2 (agentel-focus-test-chunk "agent_thought_chunk" "t"))
      (3 (agentel-focus-test-tool (format "t%d" (cl-incf tools)) "new"))
      (4 (when (> tools 0)
           (agentel-focus-test-tool (format "t%d" (1+ (random tools)))
                                    (make-string (1+ (random 5)) ?u))))
      (5 (let ((key (format "q%d" (cl-incf questions))))
           (agentel-focus-test-question session key "Q?" t)
           (push key items)))
      (6 (when items (agentel-focus-test-question session (pop items) "Q?" nil)))
      (7 (agentel-conversation-note session "n" (seq-random-elt '(error stop notice)))))
    (list :tools tools :questions questions :items items)))

(ert-deftest agentel-focus-follows-changes-like-it-was-turned-on-afresh ()
  (dotimes (seed 200)
    (random (format "agentel-focus-%d" seed))
    (agentel-focus-test-with-session
      (let ((state nil))
        (dotimes (_ (1+ (random 80)))
          (setq state (agentel-focus-test-random-step session state)))
        (let ((followed (agentel-focus-test-visible)))
          (agentel-focus-mode -1)
          (agentel-focus-mode)
          (should (equal (list seed followed)
                         (list seed (agentel-focus-test-visible)))))))))

(ert-deftest agentel-focus-can-be-turned-on-for-every-session-buffer ()
  (let ((agentel-session--registry nil)
        (agentel-chat-mode-hook (list #'agentel-focus-mode)))
    (let ((buffer (agentel-chat-open (agentel-session-create) :input t)))
      (unwind-protect
          (with-current-buffer buffer
            (should agentel-focus-mode))
        (kill-buffer buffer)))))

(ert-deftest agentel-focus-is-toggled-from-the-session-buffer ()
  (should (eq (keymap-lookup agentel-chat-mode-map "C-c C-f") #'agentel-focus-mode)))

(ert-deftest agentel-focus-keeps-what-it-shows-when-the-width-changes ()
  (agentel-focus-test-with-session
    (agentel-focus-test-a-finished-turn session)
    (agentel-focus-test-prompt session "third prompt")
    (agentel-focus-test-tool "t2" "Run the tests")
    (agentel-focus-test-chunk "agent_thought_chunk" "Checking the result")
    (let ((before (agentel-focus-test-visible)))
      (agentel-chat--fit-width)
      (should (equal (agentel-focus-test-visible) before)))))

(provide 'agentel-focus-test)
;;; agentel-focus-test.el ends here
