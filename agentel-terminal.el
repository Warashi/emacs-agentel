;;; agentel-terminal.el --- Terminals of ACP v2 agents for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; In ACP version 2 the agent runs commands in terminals of its own and
;; tells them apart from the tool calls: `terminal_update' replaces what
;; is known of a terminal, its command, the whole output so far and its
;; exit, and `terminal_output_chunk' appends to the output.  The output
;; comes base64 encoded.  A tool call shows a terminal by naming it in
;; its content, and the output of the terminal is shown as the output of
;; that tool call.
;;
;; The terminals of a session are kept for other features, such as the
;; pinned background work, through `agentel-terminal-get'.

;;; Code:

(require 'seq)
(require 'agentel-session)
(require 'agentel-conversation)

(defun agentel-terminal--table (session)
  "Return the table of the terminals of SESSION by id."
  (or (agentel-session-data session 'terminals)
      (setf (agentel-session-data session 'terminals) (make-hash-table :test 'equal))))

(defun agentel-terminal-get (session id)
  "Return what is known of the terminal ID of SESSION, or nil.
It is an alist of its `command', its `output' so far and the `tool'
call that shows it."
  (gethash id (agentel-terminal--table session)))

(defun agentel-terminal--decode (data)
  "Return the terminal output DATA decoded from base64."
  (decode-coding-string (base64-decode-string data) 'utf-8))

(defun agentel-terminal--change (session id fields)
  "Merge the alist FIELDS into the terminal ID of SESSION.
The tool call showing the terminal shows its output."
  (let* ((table (agentel-terminal--table session))
         (terminal (copy-alist (gethash id table))))
    (pcase-dolist (`(,key . ,value) fields)
      (setf (alist-get key terminal) value))
    (puthash id terminal table)
    (let-alist terminal
      (when (and .tool .output)
        (agentel-conversation-send session (cons 'tool .tool) 'tool
                                   `(update ((output . ,.output))))))))

(defun agentel-terminal--on-update (session update)
  "Follow the terminals of SESSION through UPDATE."
  (let-alist update
    (pcase .sessionUpdate
      ("terminal_update"
       (agentel-terminal--change
        session .terminalId
        (seq-filter #'cdr `((command . ,.command)
                            (output . ,(and .output.data
                                            (agentel-terminal--decode .output.data)))))))
      ("terminal_output_chunk"
       (let ((output (alist-get 'output (agentel-terminal-get session .terminalId))))
         (agentel-terminal--change
          session .terminalId
          `((output . ,(concat output (agentel-terminal--decode .data)))))))
      ("tool_call_update"
       (seq-doseq (item .content)
         (when (equal (alist-get 'type item) "terminal")
           (agentel-terminal--change session (alist-get 'terminalId item)
                                     `((tool . ,.toolCallId)))))))))

;; Other features read the terminals the update changed.
(add-hook 'agentel-session-update-functions #'agentel-terminal--on-update -50)

(provide 'agentel-terminal)
;;; agentel-terminal.el ends here
