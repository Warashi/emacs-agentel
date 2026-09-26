;;; agentel-copilot-subagent.el --- Keep Copilot subagents out of the parent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Copilot CLI runs a subagent as a task tool call of the session and
;; streams what the subagent does into the same session.  Its tool
;; calls carry `_meta."github.com/copilot".agentId', which the calls of
;; the session itself lack.  This file withholds them, so the parent's
;; transcript shows the task as one tool call whose content is the
;; reply of the subagent.

;;; Code:

(require 'agentel-session)

(defun agentel-copilot-subagent--agent-id (update)
  "Return the Copilot subagent that sent UPDATE, or nil."
  (alist-get 'agentId (alist-get 'github.com/copilot (alist-get '_meta update))))

(defun agentel-copilot-subagent--withhold-p (_session update)
  "Return non-nil if UPDATE comes from a Copilot subagent."
  (and (member (alist-get 'sessionUpdate update) '("tool_call" "tool_call_update"))
       (agentel-copilot-subagent--agent-id update)))

(add-hook 'agentel-session-withhold-functions #'agentel-copilot-subagent--withhold-p)

(provide 'agentel-copilot-subagent)
;;; agentel-copilot-subagent.el ends here
