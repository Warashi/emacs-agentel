;;; agentel-focus-test.el --- Tests for agentel-focus  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-focus)

(defmacro agentel-focus-test-with-session (&rest body)
  "Run BODY in a focused chat buffer of a session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions (list #'agentel-focus--on-changed))
         (agentel-session-update-functions (list #'agentel-chat--on-update)))
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
  (agentel-chat-add nil 'user #'agentel-chat--render-text `((text . ,text)))
  (agentel-session-set-busy session t))

(defun agentel-focus-test-question (session text)
  "Add a pending question of SESSION shown as TEXT and return it."
  (let ((item (list :kind 'test)))
    (agentel-chat-add nil 'test (lambda (_) text) `((item . ,item)))
    (agentel-session-add-pending session item)
    item))

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
  (agentel-session-set-busy session nil)
  (agentel-focus-test-prompt session "second prompt")
  (agentel-focus-test-chunk "agent_thought_chunk" "pondering")
  (agentel-focus-test-chunk "agent_message_chunk" "Let me look.")
  (agentel-focus-test-tool "t1" "Read README.org")
  (agentel-chat-notice session "a notice")
  (agentel-focus-test-chunk "agent_message_chunk" "second answer"))

(ert-deftest agentel-focus-shows-the-last-prompt-and-answer-when-idle ()
  (agentel-focus-test-with-session
    (agentel-focus-test-a-finished-turn session)
    (agentel-session-set-busy session nil)
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
    (agentel-focus-test-chunk "agent_message_chunk" "Reading more.")
    (agentel-focus-test-tool "t2" "Read b.el")
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "do it" text))
      (should (string-match-p "Read b\\.el" text))
      (should-not (string-match-p "Read a\\.el" text))
      (should-not (string-match-p "Reading more" text)))))

(ert-deftest agentel-focus-shows-questions-until-they-are-answered ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (let ((item (agentel-focus-test-question session "Allow this?")))
      (agentel-focus-test-tool "t1" "Read a.el")
      (should (string-match-p "Allow this\\?" (agentel-focus-test-visible)))
      (agentel-session-remove-pending session item)
      (should-not (string-match-p "Allow this\\?" (agentel-focus-test-visible))))))

(ert-deftest agentel-focus-shows-errors-of-the-last-turn ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-session-set-busy session nil)
    (agentel-chat-notice session "Prompt failed: boom" 'error)
    (should (string-match-p "Prompt failed: boom" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-shows-a-subagent-that-waits ()
  (agentel-focus-test-with-session
    (let ((child (agentel-session-create :parent session)))
      (agentel-session-register child "c1")
      (agentel-focus-test-prompt session "delegate")
      (agentel-chat-add nil 'subagent (lambda (_) "the subagent") `((child . ,child)))
      (agentel-focus-test-tool "t1" "Read a.el")
      (should-not (string-match-p "the subagent" (agentel-focus-test-visible)))
      (agentel-session-add-pending child (list :kind 'test))
      (should (string-match-p "the subagent" (agentel-focus-test-visible))))))

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

(provide 'agentel-focus-test)
;;; agentel-focus-test.el ends here
