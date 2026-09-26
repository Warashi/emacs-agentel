;;; agentel-copilot-subagent.el --- Keep Copilot subagents out of the parent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Copilot CLI runs a subagent as a task tool call of the session and
;; streams what the subagent does into the same session.  This file
;; withholds that output, so the parent's transcript shows the task as
;; one tool call whose content is the reply of the subagent.
;;
;; The tool calls of a subagent carry `_meta."github.com/copilot".agentId',
;; which the calls of the session itself lack.  Its reply and thoughts
;; come as message and thought chunks without any mark, but while a
;; task runs in the foreground the session waits for it, so every chunk
;; until the task ends is the subagent's.  A task started in the
;; background ends at once and the session goes on speaking, so the
;; chunks of such a subagent still show up in the parent.

;;; Code:

(require 'agentel-session)

(defun agentel-copilot-subagent--agent-id (update)
  "Return the Copilot subagent that sent UPDATE, or nil."
  (alist-get 'agentId (alist-get 'github.com/copilot (alist-get '_meta update))))

(defun agentel-copilot-subagent--task-p (update)
  "Return non-nil if UPDATE starts a subagent that the session waits for."
  (let ((input (alist-get 'rawInput update)))
    (and (equal (alist-get 'sessionUpdate update) "tool_call")
         (alist-get 'agent_type input)
         (not (equal (alist-get 'mode input) "background")))))

(defun agentel-copilot-subagent--track (session update)
  "Keep the tasks of SESSION that run in the foreground in step with UPDATE."
  (let ((id (alist-get 'toolCallId update))
        (tasks (agentel-session-data session 'copilot-tasks)))
    (cond ((agentel-copilot-subagent--task-p update)
           (setf (agentel-session-data session 'copilot-tasks) (cons id tasks)))
          ((and (member id tasks)
                (member (alist-get 'status update) '("completed" "failed")))
           (setf (agentel-session-data session 'copilot-tasks) (delete id tasks))))))

(defun agentel-copilot-subagent--withhold-p (session update)
  "Return non-nil if UPDATE of SESSION comes from a Copilot subagent."
  (pcase (alist-get 'sessionUpdate update)
    ((or "tool_call" "tool_call_update")
     (or (agentel-copilot-subagent--agent-id update)
         (progn (agentel-copilot-subagent--track session update) nil)))
    ((or "agent_message_chunk" "agent_thought_chunk")
     (agentel-session-data session 'copilot-tasks))))

(defun agentel-copilot-subagent--on-changed (session)
  "Forget the tasks of SESSION once its turn is over."
  (when (and (not (agentel-session-busy session))
             (agentel-session-data session 'copilot-tasks))
    (setf (agentel-session-data session 'copilot-tasks) nil)))

(add-hook 'agentel-session-withhold-functions #'agentel-copilot-subagent--withhold-p)
(add-hook 'agentel-session-changed-functions #'agentel-copilot-subagent--on-changed)

(provide 'agentel-copilot-subagent)
;;; agentel-copilot-subagent.el ends here
