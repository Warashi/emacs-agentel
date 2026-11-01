;;; agentel-unprompted.el --- Turns the agent starts without a prompt  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Claude Code starts turns of its own, such as for a message from
;; another session or a scheduled wakeup.  ACP v1 tells nothing of the
;; state of a session, so such a turn shows only as output of the agent
;; outside the turn of a prompt.  It ends at its result, whose usage the
;; agent reports.  The session runs meanwhile, and its last message is
;; complete at the end.

;;; Code:

(require 'agentel-session)
(require 'agentel-conversation)

(defconst agentel-unprompted--work
  '("agent_message_chunk" "agent_thought_chunk" "tool_call")
  "Updates that tell the agent works.
Updates of tool calls also tell of background commands, which run
without a turn.")

(defun agentel-unprompted--running-p (session)
  "Return non-nil while SESSION is in a turn the agent started itself."
  (agentel-session-data session 'unprompted))

(defun agentel-unprompted--set (session running)
  "Record whether SESSION is in a turn the agent started itself as RUNNING."
  (unless (eq (agentel-unprompted--running-p session) running)
    (setf (agentel-session-data session 'unprompted) running)))

(defun agentel-unprompted--on-update (session update)
  "Follow the turns of SESSION the agent starts itself through UPDATE."
  (let ((kind (alist-get 'sessionUpdate update)))
    (cond ((equal kind "usage_update")
           (when (agentel-unprompted--running-p session)
             (agentel-unprompted--set session nil)
             (unless (agentel-session-in-turn session)
               (agentel-conversation-finish-message session))))
          ((and (member kind agentel-unprompted--work)
                (not (agentel-session-parent session))
                (not (agentel-session-in-turn session))
                (not (agentel-session-loading session)))
           (agentel-unprompted--set session t)))))

(add-hook 'agentel-session-update-functions #'agentel-unprompted--on-update)
(add-hook 'agentel-session-running-functions #'agentel-unprompted--running-p)

(provide 'agentel-unprompted)
;;; agentel-unprompted.el ends here
