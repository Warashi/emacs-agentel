;;; agentel-focus-test.el --- Tests for agentel-focus  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'benchmark)
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

(ert-deftest agentel-focus-shows-why-the-last-turn-ended ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-chunk "agent_message_chunk" "Half an answer")
    (agentel-session-set-busy session nil)
    (agentel-chat-notice session "Turn ended: max_tokens" 'stop)
    (let ((text (agentel-focus-test-visible)))
      (should (string-match-p "Half an answer" text))
      (should (string-match-p "Turn ended: max_tokens" text)))))

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

(ert-deftest agentel-focus-can-be-turned-on-in-a-running-session ()
  (agentel-focus-test-with-session
    (agentel-focus-mode -1)
    (agentel-focus-test-a-finished-turn session)
    (agentel-session-set-busy session nil)
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
    (agentel-session-set-busy session nil)
    (agentel-focus-test-prompt session "second")
    (agentel-focus-test-tool "t1" "Read a.el later")
    (should-not (string-match-p "Read a\\.el" (agentel-focus-test-visible)))))

(ert-deftest agentel-focus-hides-a-streamed-message-while-running ()
  (agentel-focus-test-with-session
    (agentel-focus-test-prompt session "do it")
    (agentel-focus-test-tool "t1" "Read a.el")
    (agentel-focus-test-chunk "agent_message_chunk" "Some ")
    (agentel-focus-test-chunk "agent_message_chunk" "text")
    (should-not (string-match-p "Some\\|text" (agentel-focus-test-visible)))
    (agentel-session-set-busy session nil)
    (should (string-match-p "\\`❯ do it\n\nSome text\n\n❯ \\'"
                            (agentel-focus-test-visible)))))

(defun agentel-focus-test-random-step (session state)
  "Change SESSION in one random way.
STATE is a plist of the tool calls made and the questions open."
  (let ((tools (or (plist-get state :tools) 0))
        (items (plist-get state :items)))
    (pcase (random 9)
      (0 (agentel-focus-test-prompt session "p"))
      (1 (agentel-focus-test-chunk "agent_message_chunk" "m"))
      (2 (agentel-focus-test-chunk "agent_thought_chunk" "t"))
      (3 (agentel-focus-test-tool (format "t%d" (cl-incf tools)) "new"))
      (4 (when (> tools 0)
           (agentel-focus-test-tool (format "t%d" (1+ (random tools)))
                                    (make-string (1+ (random 5)) ?u))))
      (5 (agentel-session-set-busy session (zerop (random 2))))
      (6 (push (agentel-focus-test-question session "Q?") items))
      (7 (when items (agentel-session-remove-pending session (pop items))))
      (8 (agentel-chat-notice session "n" (seq-random-elt '(error stop notice)))))
    (list :tools tools :items items)))

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

(defun agentel-focus-test-time (turns tools change)
  "Return the seconds CHANGE takes after TURNS turns and TOOLS tool calls.
CHANGE is called with the session; the fastest of a few runs counts."
  (agentel-focus-test-with-session
    (dotimes (i turns)
      (agentel-focus-test-prompt session (format "prompt %d" i))
      (agentel-focus-test-tool (format "t%d" i) "Read x")
      (agentel-focus-test-chunk "agent_message_chunk" "answer")
      (agentel-session-set-busy session nil))
    (agentel-focus-test-prompt session "now")
    (dotimes (i tools)
      (agentel-focus-test-tool (format "now%d" i) "Read x"))
    (agentel-focus-test-chunk "agent_message_chunk" "x")
    (garbage-collect)
    (apply #'min (mapcar (lambda (_)
                           (car (benchmark-run 100 (funcall change session))))
                         '(1 2 3)))))

(defun agentel-focus-test-scales-flat-p (change)
  "Return non-nil if CHANGE takes as long in a short session as in long ones."
  (let ((short (agentel-focus-test-time 10 10 change)))
    (and (< (agentel-focus-test-time 1000 10 change) (* 5 short))
         (< (agentel-focus-test-time 10 3000 change) (* 5 short)))))

(ert-deftest agentel-focus-follows-a-chunk-regardless-of-the-length ()
  (should (agentel-focus-test-scales-flat-p
           (lambda (_) (agentel-focus-test-chunk "agent_message_chunk" "more text ")))))

(ert-deftest agentel-focus-follows-the-session-regardless-of-the-length ()
  (should (agentel-focus-test-scales-flat-p
           (lambda (session) (setf (agentel-session-data session 'usage) (random))))))

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
