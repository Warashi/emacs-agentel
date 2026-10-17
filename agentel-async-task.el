;;; agentel-async-task.el --- Background tasks of the agent for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; With `_meta.terminal_output', claude-agent-acp attaches terminals to
;; Bash tool calls.  A background command's tool call completes before
;; its terminal exits.  Keep the terminal output in the tool call and
;; pin background commands until `terminal_exit', even across turns.
;;
;; The tasks still running are pinned above the prompt of the session.

;;; Code:

(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-ui)

(defface agentel-async-task-face
  '((t :inherit font-lock-builtin-face))
  "Face of background task names."
  :group 'agentel)

(defun agentel-async-task--capabilities ()
  "Return the client capability of receiving background tasks."
  '((_meta . ((terminal_output . t)))))

(defun agentel-async-task--merge (tasks id fields)
  "Return TASKS with the alist FIELDS merged into the terminal ID."
  (let ((tasks (copy-alist tasks))
        (task (copy-alist (alist-get id tasks nil nil #'equal))))
    (pcase-dolist (`(,key . ,value) fields)
      (setf (alist-get key task) value))
    (setf (alist-get id tasks nil nil #'equal) task)
    tasks))

(defun agentel-async-task--update (message data)
  "Return the background tasks DATA of a session changed by MESSAGE.
DATA has the `tasks' whose terminals have not exited, newest first,
as (ID . TASK).  Each has its tool id, name, progress and whether it
runs in the background.  MESSAGE is (open ID TOOL), (update ID FIELDS)
or (close ID); news of an unknown terminal is ignored."
  (let ((tasks (alist-get 'tasks data)))
    `((tasks
       . ,(pcase message
            (`(open ,id ,tool)
             (if (assoc id tasks) tasks
               (agentel-async-task--merge tasks id `((tool . ,tool) (state . running)))))
            (`(update ,id ,fields)
             (if (assoc id tasks)
                 (agentel-async-task--merge tasks id fields)
               tasks))
            (`(close ,id)
             (seq-remove (lambda (task) (equal (car task) id)) tasks))
            (_ (agentel-store-reject message)))))))

(agentel-store-define 'agentel-async-tasks #'agentel-async-task--update)

(defun agentel-async-task--on-update (session update)
  "Record terminal output and background work reported in UPDATE of SESSION."
  (when (member (alist-get 'sessionUpdate update) '("tool_call" "tool_call_update"))
    (let* ((store (agentel-session-store session))
           (meta (alist-get '_meta update))
           (info (alist-get 'terminal_info meta))
           (exit (alist-get 'terminal_exit meta))
           (output (alist-get 'terminal_output meta))
           (tool (alist-get 'toolCallId update))
           (model (agentel-store-find store 'async-tasks))
           (tasks (and model (agentel-store-get model 'tasks)))
           (id (or (alist-get 'terminal_id info)
                   (alist-get 'terminal_id exit)
                   (alist-get 'terminal_id output)
                   (car (seq-find (lambda (task)
                                    (equal (alist-get 'tool (cdr task)) tool))
                                  tasks)))))
      (when info
        (agentel-store-dispatch store 'async-tasks 'agentel-async-tasks `(open ,id ,tool))
        (setq model (agentel-store-find store 'async-tasks)))
      (when-let* ((task (and model (assoc id (agentel-store-get model 'tasks)))))
        (let* ((input (alist-get 'rawInput update))
               (name (or (alist-get 'command input) (alist-get 'title update)))
               (text (alist-get 'data output))
               (fields (append
                        (when name `((name . ,name)))
                        (when (or (eq (alist-get 'run_in_background input) t)
                                  (and (equal (alist-get 'status update) "completed")
                                       (not exit)))
                          '((background . t)))
                        (when text `((progress . ,text))))))
          (when fields
            (agentel-store-dispatch store 'async-tasks 'agentel-async-tasks
                                    `(update ,id ,fields)))
          (when text
            (agentel-conversation-send session (cons 'tool (alist-get 'tool (cdr task)))
                                       'tool `(update ((output . ,text)))))
          (when exit
            (agentel-store-dispatch store 'async-tasks 'agentel-async-tasks
                                    `(close ,id))))))))

(defun agentel-async-task--view (model options)
  "Return the pinned lines of the background tasks of MODEL, oldest first.
Each line fits in the :width of OPTIONS."
  (mapconcat (lambda (item)
               (let-alist (cdr item)
                 (agentel-ui-one-line
                  (concat "⚙ " (propertize (or .name "Background task")
                                           'face 'agentel-async-task-face)
                          " " (agentel-ui-state .state)
                          (if .progress " ↳ " ""))
                  .progress
                  (plist-get options :width))))
             (reverse (seq-filter (lambda (task) (alist-get 'background (cdr task)))
                                  (agentel-store-get model 'tasks)))
             "\n"))

(agentel-ui-define-view 'agentel-async-tasks #'agentel-async-task--view :pin 10)

(add-hook 'agentel-connection-capability-functions #'agentel-async-task--capabilities)
(add-hook 'agentel-session-update-functions #'agentel-async-task--on-update)

(provide 'agentel-async-task)
;;; agentel-async-task.el ends here
