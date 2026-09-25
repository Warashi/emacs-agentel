;;; agentel-chat-test.el --- Tests for agentel-chat  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agentel-chat)

(defmacro agentel-chat-test-with-session (&rest body)
  "Run BODY in the chat buffer of a registered session bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-chat--on-update)))
     (let* ((session (agentel-session-create :cwd "/tmp/project/"))
            (buffer (agentel-chat-open session :input t)))
       (agentel-session-register session "s1")
       (unwind-protect
           (with-current-buffer buffer ,@body)
         (kill-buffer buffer)))))

(defun agentel-chat-test-update (update)
  "Dispatch UPDATE to session s1."
  (agentel-session-dispatch `((sessionId . "s1") (update . ,update))))

(defun agentel-chat-test-chunk (kind text)
  "Dispatch a text chunk of KIND with TEXT to session s1."
  (agentel-chat-test-update
   `((sessionUpdate . ,kind) (content . ((type . "text") (text . ,text))))))

(defun agentel-chat-test-transcript ()
  "Return the transcript of the current chat buffer as plain text."
  (buffer-substring-no-properties (point-min) agentel-chat--transcript-end))

(ert-deftest agentel-chat-open-shows-an-empty-input ()
  (agentel-chat-test-with-session
    (should (equal (agentel-chat-input) ""))
    (should (equal (agentel-chat-test-transcript) ""))))

(ert-deftest agentel-chat-hides-the-gap-before-the-prompt-until-something-is-said ()
  (agentel-chat-test-with-session
    (should (invisible-p (point-min)))
    (agentel-chat-test-chunk "agent_message_chunk" "Hello")
    (should-not (invisible-p (1- agentel-chat--input-start)))
    (should-not (text-property-any (point-min) (point-max) 'invisible t))))

(ert-deftest agentel-chat-streams-agent_message_chunks-into-one-message ()
  (agentel-chat-test-with-session
    (agentel-chat-test-chunk "agent_message_chunk" "Hello")
    (agentel-chat-test-chunk "agent_message_chunk" ", world")
    (should (equal (agentel-chat-test-transcript) "Hello, world"))))

(ert-deftest agentel-chat-keeps-the-input-while-the-agent-writes ()
  (agentel-chat-test-with-session
    (goto-char (point-max))
    (insert "draft\nsecond line")
    (agentel-chat-test-chunk "agent_message_chunk" "Hello")
    (should (equal (agentel-chat-input) "draft\nsecond line"))
    (should (equal (agentel-chat-test-transcript) "Hello"))))

(ert-deftest agentel-chat-separates-thoughts-from-messages ()
  (agentel-chat-test-with-session
    (agentel-chat-test-chunk "agent_thought_chunk" "Hmm")
    (agentel-chat-test-chunk "agent_message_chunk" "Answer")
    (let ((text (agentel-chat-test-transcript)))
      (should (string-match-p "Hmm" text))
      (should (string-match-p "\n\nAnswer\\'" text)))))

(ert-deftest agentel-chat-shows-user-messages-with-a-marker ()
  (agentel-chat-test-with-session
    (agentel-chat-test-chunk "user_message_chunk" "What now?")
    (should (string-match-p "What now\\?" (agentel-chat-test-transcript)))))

(ert-deftest agentel-chat-collapses-tool-output-until-toggled ()
  (agentel-chat-test-with-session
    (agentel-chat-test-update
     '((sessionUpdate . "tool_call") (toolCallId . "t1")
       (title . "Read README.org") (kind . "read") (status . "pending")))
    (agentel-chat-test-update
     '((sessionUpdate . "tool_call_update") (toolCallId . "t1")
       (status . "completed")
       (content . [((type . "content")
                    (content . ((type . "text") (text . "file body"))))])))
    (should (string-match-p "Read README.org" (agentel-chat-test-transcript)))
    (should-not (string-match-p "file body" (agentel-chat-test-transcript)))
    (goto-char (point-min))
    (search-forward "Read README")
    (agentel-chat-toggle)
    (should (string-match-p "file body" (agentel-chat-test-transcript)))))

(ert-deftest agentel-chat-updating-an-old-tool-keeps-later-entries-intact ()
  (agentel-chat-test-with-session
    (agentel-chat-test-update
     '((sessionUpdate . "tool_call") (toolCallId . "t1")
       (title . "Run tests") (kind . "execute") (status . "in_progress")))
    (agentel-chat-test-chunk "agent_message_chunk" "Meanwhile")
    (goto-char (point-min))
    (search-forward "Run")
    (agentel-chat-toggle)
    (agentel-chat-test-update
     '((sessionUpdate . "tool_call_update") (toolCallId . "t1")
       (status . "completed")
       (content . [((type . "content")
                    (content . ((type . "text") (text . "ok 3 tests"))))])))
    (agentel-chat-test-chunk "agent_message_chunk" " done")
    (let ((text (agentel-chat-test-transcript)))
      (should (string-match-p "ok 3 tests\n\nMeanwhile done\\'" text)))))

(ert-deftest agentel-chat-updating-tools-in-any-order-keeps-each-in-place ()
  (agentel-chat-test-with-session
    (dolist (id '("t1" "t2" "t3"))
      (agentel-chat-test-update
       `((sessionUpdate . "tool_call") (toolCallId . ,id) (title . ,id))))
    (dolist (id '("t2" "t3" "t1"))
      (agentel-chat-test-update
       `((sessionUpdate . "tool_call_update") (toolCallId . ,id)
         (title . ,(concat id " again")))))
    (should (string-match-p "\\`.*t1 again\n\n.*t2 again\n\n.*t3 again\\'"
                            (agentel-chat-test-transcript)))))

(ert-deftest agentel-chat-shows-the-latest-plan ()
  (agentel-chat-test-with-session
    (agentel-chat-test-update
     '((sessionUpdate . "plan")
       (entries . [((content . "Write tests") (status . "in_progress"))
                   ((content . "Implement") (status . "pending"))])))
    (agentel-chat-test-update
     '((sessionUpdate . "plan")
       (entries . [((content . "Write tests") (status . "completed"))
                   ((content . "Implement") (status . "in_progress"))])))
    (let ((text (agentel-chat-test-transcript)))
      (should (= 1 (how-many "Write tests" (point-min) (point-max))))
      (should (string-match-p "\\[x\\] Write tests" text)))))

(ert-deftest agentel-chat-transcript-is-read-only ()
  (agentel-chat-test-with-session
    (agentel-chat-test-chunk "agent_message_chunk" "Hello")
    (goto-char (point-min))
    (should-error (insert "x") :type 'text-read-only)))

(ert-deftest agentel-chat-tells-which-entry-changed ()
  (agentel-chat-test-with-session
    (let ((agentel-chat-format-message-function nil)
          changed)
      (add-hook 'agentel-chat-entry-changed-functions (lambda (e) (push e changed)) nil t)
      (agentel-chat-test-chunk "agent_message_chunk" "Hel")
      (let ((message (agentel-chat-entry-at (point-min))))
        (should (equal changed (list message)))
        (agentel-chat-test-chunk "agent_message_chunk" "lo")
        (should (equal changed (list message message))))
      (setq changed nil)
      (agentel-chat-test-update '((sessionUpdate . "tool_call") (toolCallId . "t1")
                                  (title . "Read") (status . "pending")))
      (should changed)
      (should (seq-every-p (lambda (e) (eq e (agentel-chat-find '(tool . "t1")))) changed))
      (setq changed nil)
      (goto-char (point-max))
      (insert "typing")
      (should-not changed))))

(ert-deftest agentel-chat-notice-appears-in-transcript ()
  (agentel-chat-test-with-session
    (agentel-chat-notice session "Agent exited" 'error)
    (should (string-match-p "Agent exited" (agentel-chat-test-transcript)))))

(defun agentel-chat-test-shown ()
  "Return the text of the current buffer as shown, with overlay strings."
  (let ((text "") (pos (point-min)))
    (while (< pos (point-max))
      (dolist (overlay (overlays-at pos))
        (when (and (= (overlay-start overlay) pos) (overlay-get overlay 'before-string))
          (setq text (concat text (overlay-get overlay 'before-string)))))
      (unless (invisible-p pos)
        (setq text (concat text (string (char-after pos)))))
      (setq pos (1+ pos)))
    (substring-no-properties text)))

(ert-deftest agentel-chat-pins-feature-lines-above-the-prompt ()
  (agentel-chat-test-with-session
    (let* ((lines nil)
           (agentel-chat-pin-functions (list (lambda (_) lines) (lambda (_) '("last"))))
           (agentel-session-changed-functions (list #'agentel-chat--on-changed)))
      (agentel-chat-test-chunk "agent_message_chunk" "Hello")
      (agentel-session-changed session)
      (should (equal (agentel-chat-test-shown) "Hello\n\nlast\n❯ "))
      (setq lines '("one" "two"))
      (agentel-session-changed session)
      (should (equal (agentel-chat-test-shown) "Hello\n\none\ntwo\nlast\n❯ "))
      (agentel-chat-test-chunk "agent_message_chunk" " again")
      (agentel-chat-test-update '((sessionUpdate . "tool_call") (toolCallId . "t1")
                                  (title . "Read")))
      (goto-char (point-max))
      (insert "typed")
      (should (string-suffix-p "\n\none\ntwo\nlast\n❯ typed" (agentel-chat-test-shown)))
      (agentel-chat--set-input "")
      (should (string-suffix-p "Read\n\none\ntwo\nlast\n❯ " (agentel-chat-test-shown))))))

(ert-deftest agentel-chat-pins-nothing-without-lines ()
  (agentel-chat-test-with-session
    (let ((agentel-chat-pin-functions (list (lambda (_) nil))))
      (agentel-chat-test-chunk "agent_message_chunk" "Hello")
      (agentel-chat-refresh-pin session)
      (should (equal (agentel-chat-test-shown) "Hello\n\n❯ ")))))

(ert-deftest agentel-chat-header-joins-feature-segments ()
  (agentel-chat-test-with-session
    (let ((agentel-chat-header-functions
           (list (lambda (_s) "model") (lambda (_s) nil) (lambda (_s) "ctx"))))
      (should (string-match-p "model.*ctx" (agentel-chat--header-line))))))

(ert-deftest agentel-chat-header-line-escapes-percent-signs ()
  ;; `format-mode-line' renders nothing in batch mode, so check the
  ;; mode line format the :eval form produces instead.
  (agentel-chat-test-with-session
    (let ((agentel-chat-header-functions (list (lambda (_s) "ctx 12%"))))
      (should (string-match-p "ctx 12%%\\'"
                              (eval (cadr header-line-format) t))))))

(ert-deftest agentel-chat-answer-runs-the-oldest-question ()
  (agentel-chat-test-with-session
    (let (answered)
      (agentel-session-add-pending
       session (list :answer (lambda () (push 'first answered))))
      (agentel-session-add-pending
       session (list :answer (lambda () (push 'second answered))))
      (agentel-chat-answer)
      (should (equal answered '(first))))))

(ert-deftest agentel-chat-answer-without-questions-is-a-user-error ()
  (agentel-chat-test-with-session
    (should-error (agentel-chat-answer) :type 'user-error)))

(ert-deftest agentel-chat-ordered-completion-keeps-the-order ()
  (let ((table (agentel-chat-ordered-completion '("Yes" "No"))))
    (should (eq (completion-metadata-get
                 (completion-metadata "" table nil) 'display-sort-function)
                #'identity))
    (should (equal (all-completions "" table) '("Yes" "No")))))

(provide 'agentel-chat-test)
;;; agentel-chat-test.el ends here
