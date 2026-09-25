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
  (let ((text "# Title\n\nSome **bold** and `code`.\n\n```sh\nls -l\n```"))
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

(ert-deftest agentel-markdown-formats-finished-agent-messages ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil)
        (agentel-session-update-functions (list #'agentel-chat--on-update)))
    (let* ((session (agentel-session-create))
           (buffer (agentel-chat-open session :input t)))
      (agentel-session-register session "s1")
      (unwind-protect
          (with-current-buffer buffer
            (agentel-session-dispatch
             '((sessionId . "s1")
               (update . ((sessionUpdate . "agent_message_chunk")
                          (content . ((type . "text") (text . "Use `ls`.")))))))
            (agentel-chat-finish-message)
            (goto-char (point-min))
            (search-forward "ls")
            (should (memq 'agentel-markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face)))))
        (kill-buffer buffer)))))

(ert-deftest agentel-markdown-formats-a-continued-message-again ()
  (let ((agentel-session--registry nil)
        (agentel-session-changed-functions nil)
        (agentel-session-update-functions (list #'agentel-chat--on-update)))
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
              (agentel-chat-finish-message))
            (goto-char (point-min))
            (search-forward "ls")
            (should (memq 'agentel-markdown-code-face
                          (ensure-list (get-text-property (1- (point)) 'face)))))
        (kill-buffer buffer)))))

(provide 'agentel-markdown-test)
;;; agentel-markdown-test.el ends here
