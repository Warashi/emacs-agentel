;;; agentel-markdown-test.el --- Tests for agentel-markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-markdown)

(defun agentel-markdown-test-face-at (string substring)
  "Return the face of STRING at the start of SUBSTRING."
  (get-text-property (string-search substring string) 'face string))

(defun agentel-markdown-test-has-face (string substring face)
  "Return non-nil if SUBSTRING of STRING has FACE among its faces."
  (let ((faces (ensure-list (agentel-markdown-test-face-at string substring))))
    (memq face faces)))

(ert-deftest agentel-markdown-keeps-the-text ()
  (let ((text "# Title\n\nSome **bold** and `code`.\n\n```sh\nls -l\n```\n\n| a | b |\n|---|---|\n| long | x |"))
    (should (equal (substring-no-properties (agentel-markdown-format text)) text))))

(ert-deftest agentel-markdown-highlights-headings-bold-and-inline-code ()
  (let ((s (agentel-markdown-format "# Title\nSome **bold**, *italic* and `code`.")))
    (should (agentel-markdown-test-has-face s "Title" 'agentel-markdown-heading-face))
    (should (agentel-markdown-test-has-face s "bold" 'bold))
    (should (agentel-markdown-test-has-face s "italic" 'italic))
    (should (agentel-markdown-test-has-face s "code" 'agentel-markdown-code-face))))

(ert-deftest agentel-markdown-does-not-take-list-stars-for-italics ()
  (let ((s (agentel-markdown-format "* one\n* two")))
    (should-not (agentel-markdown-test-has-face s "one" 'italic))))

(ert-deftest agentel-markdown-fontifies-code-blocks-in-their-language ()
  (let ((s (agentel-markdown-format "```emacs-lisp\n(defun f () \"doc\")\n```")))
    (should (agentel-markdown-test-has-face s "defun" 'font-lock-keyword-face))
    (should (agentel-markdown-test-has-face s "defun" 'agentel-markdown-code-block-face))))

(ert-deftest agentel-markdown-dims-only-the-fences-of-a-fontified-block ()
  (let ((s (agentel-markdown-format "and *then* test\n\n```emacs-lisp\n(x)\n```\n")))
    (should-not (agentel-markdown-test-has-face s "test" 'agentel-markdown-markup-face))
    (should-not (agentel-markdown-test-has-face s "(x)" 'agentel-markdown-markup-face))
    (should (agentel-markdown-test-has-face s "```\n" 'agentel-markdown-markup-face))))

(ert-deftest agentel-markdown-leaves-code-blocks-alone-otherwise ()
  (let ((s (agentel-markdown-format "```\n**not bold**\n```")))
    (should-not (agentel-markdown-test-has-face s "not bold" 'bold))
    (should (agentel-markdown-test-has-face s "not bold" 'agentel-markdown-code-block-face))))

(defun agentel-markdown-test-shown (string)
  "Return the lines of STRING as shown, with display strings in place."
  (let ((pos 0) shown)
    (while (< pos (length string))
      (let ((next (next-single-property-change pos 'display string (length string)))
            (display (get-text-property pos 'display string)))
        (push (if (stringp display) display (substring-no-properties string pos next))
              shown)
        (setq pos next)))
    (split-string (apply #'concat (nreverse shown)) "\n")))

(ert-deftest agentel-markdown-aligns-the-columns-of-a-table ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "| a | b |\n|---|---|\n| long | x |"))))
    (should (equal (nth 0 shown) "| a    | b |"))
    (should (equal (nth 2 shown) "| long | x |"))))

(ert-deftest agentel-markdown-aligns-wide-characters-by-their-width ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "| 名前 | b |\n|---|---|\n| abcdef | x |"))))
    (should (equal (nth 0 shown) "| 名前   | b |"))
    (should (equal (nth 2 shown) "| abcdef | x |"))))

(ert-deftest agentel-markdown-extends-the-delimiter-row-of-a-table ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "| a | b |\n|---|---|\n| long | x |"))))
    (should (equal (nth 1 shown) "|------|---|"))))

(ert-deftest agentel-markdown-does-not-split-a-cell-at-an-escaped-pipe ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "| a\\|b | c |\n|---|---|\n| d | e |"))))
    (should (equal (nth 2 shown) "| d    | e |"))))

(ert-deftest agentel-markdown-aligns-a-row-starting-with-inline-code ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "a | b\n---|---\n`long` | x"))))
    (should (equal (nth 0 shown) "a      | b"))))

(ert-deftest agentel-markdown-wraps-the-cells-of-a-table-wider-than-the-window ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format
                 "| a | b |\n|---|---|\n| one two three | x |" 15))))
    (should (equal shown '("| a       | b |"
                           "|---------|---|"
                           "| one two | x |"
                           "| three   |   |")))))

(defun agentel-markdown-test-column (lines column)
  "Return the text of COLUMN in the body of the table shown in LINES."
  (mapconcat (lambda (line) (string-trim (nth column (split-string line "|"))))
             (cdr (seq-drop-while (lambda (line) (not (string-prefix-p "|-" line)))
                                  lines))))

(ert-deftest agentel-markdown-wraps-wide-characters-within-the-window ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format
                 "| 項目 | 説明 |\n|---|---|\n| 幅 | 日本語の長い説明文がここに入ります |" 20))))
    (dolist (line shown)
      (should (<= (string-width line) 20)))
    (should (equal (agentel-markdown-test-column shown 2)
                   "日本語の長い説明文がここに入ります"))))

(ert-deftest agentel-markdown-wraps-wide-characters-in-a-very-narrow-window ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "| 項目 | 説明 |\n|---|---|\n| 幅 | 日本語の説明 |" 4))))
    (should (equal (agentel-markdown-test-column shown 2) "日本語の説明"))))

(ert-deftest agentel-markdown-breaks-a-word-longer-than-its-column ()
  (let ((shown (agentel-markdown-test-shown
                (agentel-markdown-format "| x |\n|---|\n| abcdefghij |" 8))))
    (should (equal (nthcdr 2 shown) '("| abcd |" "| efgh |" "| ij   |")))))

(ert-deftest agentel-markdown-keeps-a-table-fitting-the-window-as-it-is ()
  (let ((text "| a | b |\n|---|---|\n| long | x |"))
    (should (equal (agentel-markdown-test-shown (agentel-markdown-format text 12))
                   (agentel-markdown-test-shown (agentel-markdown-format text))))))

(ert-deftest agentel-markdown-keeps-the-text-of-a-wrapped-table ()
  (let ((text "| a | b |\n|---|---|\n| one two three | x |"))
    (should (equal (substring-no-properties (agentel-markdown-format text 15)) text))))

(ert-deftest agentel-markdown-keeps-the-faces-of-a-wrapped-cell ()
  (let* ((s (agentel-markdown-format "| a | b |\n|---|---|\n| one `two` three | x |" 15))
         (row (get-text-property (string-search "one" s) 'display s)))
    (should (agentel-markdown-test-has-face row "two" 'agentel-markdown-code-face))))

(ert-deftest agentel-markdown-marks-a-table-as-fitting-the-width ()
  (should (text-property-any 0 10 'agentel-ui-fits-width t
                             (agentel-markdown-format "| a | b |\n|---|---|\n| c | d |")))
  (let ((s (agentel-markdown-format "no table")))
    (should-not (text-property-any 0 (length s) 'agentel-ui-fits-width t s))))

(ert-deftest agentel-markdown-leaves-tables-in-code-blocks-alone ()
  (let ((text "```\n| a | b |\n|---|---|\n| long | x |\n```"))
    (should (equal (agentel-markdown-test-shown (agentel-markdown-format text))
                   (split-string text "\n")))))

(defun agentel-markdown-test-copy-at (text substring)
  "Return what copying the code at SUBSTRING of TEXT formatted puts in the kill ring."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert (agentel-markdown-format text))
      (goto-char (point-min))
      (search-forward substring)
      (agentel-markdown-copy-code)
      (car kill-ring))))

(ert-deftest agentel-markdown-copies-the-code-of-the-block-at-point ()
  (let ((text "Run:\n\n```sh\nls -l\necho done\n```\n\nThen:\n\n```\nmake\n```\n"))
    (should (equal (agentel-markdown-test-copy-at text "echo") "ls -l\necho done"))
    (should (equal (agentel-markdown-test-copy-at text "```s") "ls -l\necho done"))
    (should (equal (agentel-markdown-test-copy-at text "mak") "make"))))

(ert-deftest agentel-markdown-copies-the-code-without-its-properties ()
  (should-not (text-properties-at
               0 (agentel-markdown-test-copy-at "```emacs-lisp\n(defun f ())\n```" "def"))))

(defmacro agentel-markdown-test-with-session (&rest body)
  "Run BODY in a chat buffer of a session bound to `session', at its input."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil)
         (agentel-session-update-functions (list #'agentel-conversation--on-update))
         (kill-ring nil))
     (let* ((session (agentel-session-create))
            (buffer (agentel-chat-open session :input t)))
       (agentel-session-register session "s1")
       (unwind-protect
           (with-current-buffer buffer
             ,@body)
         (kill-buffer buffer)))))

(defun agentel-markdown-test-turn (session prompt &rest messages)
  "Show PROMPT sent to SESSION and the finished agent MESSAGES answering it.
A tool call separates the messages."
  (agentel-conversation-send session nil 'user `(chunk ,prompt))
  (dolist (text messages)
    (agentel-session-dispatch
     `((sessionId . "s1")
       (update . ((sessionUpdate . "tool_call") (toolCallId . ,text)
                  (title . "Tool") (status . "completed")))))
    (agentel-session-dispatch
     `((sessionId . "s1")
       (update . ((sessionUpdate . "agent_message_chunk")
                  (content . ((type . "text") (text . ,text)))))))
    (agentel-conversation-finish-message session)))

(ert-deftest agentel-markdown-copies-the-only-code-block-of-the-last-turn ()
  (agentel-markdown-test-with-session
    (agentel-markdown-test-turn session "old" "```\nold\n```")
    (agentel-markdown-test-turn session "new" "Run:\n\n```sh\nls -l\n```")
    (goto-char (point-max))
    (agentel-markdown-copy-code)
    (should (equal (car kill-ring) "ls -l"))))

(ert-deftest agentel-markdown-asks-which-code-block-of-the-last-turn-to-copy ()
  (agentel-markdown-test-with-session
    (agentel-markdown-test-turn session "old" "```\nold\n```")
    (agentel-markdown-test-turn session "new"
                                "```sh\nls -l\nls -a\n```" "```\nmake\n```")
    (goto-char (point-max))
    (let (offered default)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &optional _pred _req _init _hist def)
                   (setq offered (all-completions "" table)
                         default def)
                   (car offered))))
        (agentel-markdown-copy-code))
      (should (equal offered '("sh: ls -l" "make")))
      (should (equal default "make"))
      (should (equal (car kill-ring) "ls -l\nls -a")))))

(ert-deftest agentel-markdown-copies-the-chosen-one-of-blocks-alike ()
  (agentel-markdown-test-with-session
    (agentel-markdown-test-turn session "new"
                                "```\nmake\n# a\n```" "```\nmake\n# b\n```")
    (goto-char (point-max))
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &optional _pred _req _init _hist def)
                   (setq offered (all-completions "" table))
                   def)))
        (agentel-markdown-copy-code))
      (should (equal (length (delete-dups (copy-sequence offered))) 2))
      (should (equal (car kill-ring) "make\n# b")))))

(ert-deftest agentel-markdown-copies-the-block-at-point-in-the-transcript ()
  (agentel-markdown-test-with-session
    (agentel-markdown-test-turn session "new" "```\nfirst\n```" "```\nsecond\n```")
    (goto-char (point-min))
    (search-forward "firs")
    (agentel-markdown-copy-code)
    (should (equal (car kill-ring) "first"))))

(ert-deftest agentel-markdown-tells-when-the-last-turn-has-no-code-block ()
  (agentel-markdown-test-with-session
    (agentel-markdown-test-turn session "old" "```\nold\n```")
    (agentel-markdown-test-turn session "new" "No code.")
    (goto-char (point-max))
    (should-error (agentel-markdown-copy-code) :type 'user-error)))

(ert-deftest agentel-markdown-formats-finished-agent-messages ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil)
        (agentel-session-update-functions (list #'agentel-conversation--on-update)))
    (let* ((session (agentel-session-create))
           (buffer (agentel-chat-open session :input t)))
      (agentel-session-register session "s1")
      (unwind-protect
          (with-current-buffer buffer
            (agentel-session-dispatch
             '((sessionId . "s1")
               (update . ((sessionUpdate . "agent_message_chunk")
                          (content . ((type . "text") (text . "Use `ls`.")))))))
            (agentel-conversation-finish-message session)
            (goto-char (point-min))
            (search-forward "ls")
            (should (memq 'agentel-markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face)))))
        (kill-buffer buffer)))))

(ert-deftest agentel-markdown-formats-a-continued-message-again ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil)
        (agentel-session-update-functions (list #'agentel-conversation--on-update)))
    (let* ((session (agentel-session-create))
           (buffer (agentel-chat-open session :input t)))
      (agentel-session-register session "s1")
      (unwind-protect
          (with-current-buffer buffer
            (dolist (text '("Use " "`ls`"))
              (agentel-session-dispatch
               `((sessionId . "s1")
                 (update . ((sessionUpdate . "agent_message_chunk")
                            (content . ((type . "text") (text . ,text)))))))
              (agentel-conversation-finish-message session))
            (goto-char (point-min))
            (search-forward "ls")
            (should (memq 'agentel-markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face)))))
        (kill-buffer buffer)))))

(provide 'agentel-markdown-test)
;;; agentel-markdown-test.el ends here
