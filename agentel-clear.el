;;; agentel-clear.el --- Start the conversation over for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The /clear command replaces the session with a new one in the same
;; directory and with the same settings, and closes the old buffer
;; and its agent.  The old conversation stays with the agent, so
;; /resume can open it again.

;;; Code:

(require 'agentel-session)
(require 'agentel-commands)

(declare-function agentel-start "agentel")
(declare-function agentel-session-restart-options "agentel")

(defun agentel-clear (session _args)
  "Replace SESSION with a new session started as SESSION is now."
  (let* ((old-buffer (agentel-session-buffer session))
         (cleared (apply #'agentel-start :display nil
                         (agentel-session-restart-options session)))
         (new-buffer (agentel-session-buffer cleared)))
    (dolist (window (get-buffer-window-list old-buffer nil t))
      (set-window-buffer window new-buffer))
    (kill-buffer old-buffer)
    cleared))

(agentel-commands-define "clear" "Start a new conversation with the same settings"
                         #'agentel-clear)

(provide 'agentel-clear)
;;; agentel-clear.el ends here
