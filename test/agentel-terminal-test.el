;;; agentel-terminal-test.el --- Tests for agentel-terminal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'agentel-terminal)
(require 'agentel-background)
(require 'agentel-test-helper)

(defmacro agentel-terminal-test-with-session (&rest body)
  "Run BODY with a registered session of an ACP v2 agent bound to `session'."
  (declare (indent 0))
  `(let ((agentel-session--registry nil)
         (agentel-session-changed-functions nil))
     (let ((session (agentel-session-create
                     :connection (agentel-connection--make
                                  :info '((protocolVersion . 2))))))
       (agentel-session-register session "s1")
       ,@body)))

(defun agentel-terminal-test-update (&rest update)
  "Tell the session s1 the `session/update' UPDATE, an alist."
  (agentel-session-dispatch `((sessionId . "s1") (update . ,update))))

(defun agentel-terminal-test-base64 (text)
  "Return TEXT encoded as the agent sends terminal output."
  (base64-encode-string (encode-coding-string text 'utf-8) t))

(defun agentel-terminal-test-start (id command)
  "Start the terminal ID of a Bash tool call running COMMAND."
  (agentel-terminal-test-update '(sessionUpdate . "terminal_update") `(terminalId . ,id))
  (agentel-terminal-test-update
   '(sessionUpdate . "tool_call_update") `(toolCallId . ,id) '(name . "Bash")
   '(title . "Terminal") '(status . "pending")
   `(content . [((type . "terminal") (terminalId . ,id))]))
  (agentel-terminal-test-update '(sessionUpdate . "terminal_update") `(terminalId . ,id)
                                `(command . ,command)))

(defun agentel-terminal-test-output (session id)
  "Return the output the tool call ID of SESSION shows."
  (agentel-store-get (agentel-conversation-find session (cons 'tool id)) 'output))

(ert-deftest agentel-terminal-shows-its-output-in-its-tool-call ()
  (agentel-terminal-test-with-session
    (agentel-terminal-test-start "t1" "echo hello")
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_update") '(terminalId . "t1")
     `(output . ((data . ,(agentel-terminal-test-base64 "héllo")))))
    (should (equal (agentel-terminal-test-output session "t1") "héllo"))))

(ert-deftest agentel-terminal-replaces-its-output-and-appends-chunks ()
  (agentel-terminal-test-with-session
    (agentel-terminal-test-start "t1" "make")
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_update") '(terminalId . "t1")
     `(output . ((data . ,(agentel-terminal-test-base64 "old")))))
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_update") '(terminalId . "t1")
     `(output . ((data . ,(agentel-terminal-test-base64 "built\n")))))
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_output_chunk") '(terminalId . "t1")
     `(data . ,(agentel-terminal-test-base64 "[exited with code 0]\n")))
    (should (equal (agentel-terminal-test-output session "t1")
                   "built\n[exited with code 0]\n"))))

(ert-deftest agentel-terminal-shows-output-that-came-before-its-tool-call ()
  (agentel-terminal-test-with-session
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_update") '(terminalId . "t1")
     `(output . ((data . ,(agentel-terminal-test-base64 "early")))))
    (agentel-terminal-test-start "t1" "make")
    (should (equal (agentel-terminal-test-output session "t1") "early"))))

(ert-deftest agentel-terminal-in-the-background-is-pinned-with-its-command ()
  (agentel-terminal-test-with-session
    (agentel-terminal-test-start "t1" "npm run dev")
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_update") '(terminalId . "t1")
     `(output . ((data . ,(agentel-terminal-test-base64 "Listening")))))
    (agentel-terminal-test-update
     '(sessionUpdate . "state_update") '(state . "running")
     '(background . [((type . "terminal") (terminalId . "t1"))]))
    (agentel-terminal-test-update
     '(sessionUpdate . "terminal_output_chunk") '(terminalId . "t1")
     `(data . ,(agentel-terminal-test-base64 " on 3000")))
    (let ((item (cdar (agentel-background--items session))))
      (should (equal (alist-get 'title item) "npm run dev"))
      (should (equal (alist-get 'report item) "Listening on 3000")))))

(ert-deftest agentel-terminal-v2-background-command-is-pinned-until-it-exits ()
  (let ((agentel-acp-v2-agents '(mock)))
    (agentel-test-with-started session nil
      (agentel-test-send "async please")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should (string-match-p "⚙ npm run dev"
                              (or (overlay-get agentel-chat--pin 'before-string) "")))
      (agentel-test-send "hello")
      (agentel-test-wait-for-text "Echo: hello")
      (agentel-test-wait-until (lambda () (eq (agentel-session-state session) 'idle)))
      (should-not (overlay-get agentel-chat--pin 'before-string))
      (should (equal (agentel-terminal-test-output
                      session (car (hash-table-keys (agentel-terminal--table session))))
                     "Listening on port 3000\n[exited with code 0]\n")))))

(provide 'agentel-terminal-test)
;;; agentel-terminal-test.el ends here
