;;; agentel-focus.el --- Show only what the next input needs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-focus-mode' shows in the transcript only what the user
;; answers next: the last prompt, the questions still waiting for an
;; answer, errors and why the turn ended, the last agent message, and
;; the latest thought or tool call when it came after that message.  A
;; subagent is shown while it waits for an answer; while it runs it is
;; pinned above the prompt instead.
;;
;; What is shown is chosen from the items of the conversation alone, by
;; `agentel-focus-items', and the session buffer draws them in place of
;; the whole conversation; see `agentel-chat-transcript-function'.

;;; Code:

(require 'agentel-chat)
(require 'agentel-store)

(defun agentel-focus-items (items)
  "Return the ITEMS of a conversation the next input needs, oldest first.
They are those from the last prompt on, or all without a prompt, that
are prompts, errors, why the turn ended, waiting for an answer, the
last agent message or the latest thought or tool call after it."
  (let ((turn items) last-message last-activity)
    (let ((tail items))
      (while tail
        (when (eq (agentel-store-model-type (car tail)) 'user)
          (setq turn tail))
        (setq tail (cdr tail))))
    (dolist (item turn)
      (pcase (agentel-store-model-type item)
        ('agent (setq last-message item
                      last-activity nil))
        ((or 'thought 'tool) (setq last-activity item))))
    (seq-filter (lambda (item)
                  (or (eq item last-message)
                      (eq item last-activity)
                      (memq (agentel-store-model-type item) '(user error stop))
                      (agentel-store-get item 'waiting)))
                turn)))

;;;###autoload
(define-minor-mode agentel-focus-mode
  "Show only what the next input to the session needs.
The last prompt stays visible, with the questions waiting for an
answer, errors and why the turn ended, the last agent message, and
the latest thought or tool call when it came after that message."
  :lighter " Focus"
  (if agentel-focus-mode
      (setq-local agentel-chat-transcript-function #'agentel-focus-items)
    (kill-local-variable 'agentel-chat-transcript-function))
  (agentel-chat-render))

(keymap-set agentel-chat-mode-map "C-c C-f" #'agentel-focus-mode)

(provide 'agentel-focus)
;;; agentel-focus.el ends here
