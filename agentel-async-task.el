;;; agentel-async-task.el --- Background tasks of the agent for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; claude-agent-acp reports background work other than subagents, such
;; as a shell command left running, through the `asyncTasks' capability
;; of the JetBrains AIR extension.  The client advertises it under
;; `_meta.jetbrains.air' of its capabilities; the agent then announces
;; each task with `async_task_spawned', reports progress with
;; `async_task_progress' and its state with `async_task_state_update'.
;;
;; The tasks still running are pinned above the prompt of the session.

;;; Code:

(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-ui)

(defface agentel-async-task-face
  '((t :inherit font-lock-builtin-face))
  "Face of background task names."
  :group 'agentel)

(defun agentel-async-task--capabilities ()
  "Return the client capability of receiving background tasks."
  '((_meta . ((jetbrains . ((air . ((version . 1)
                                    (capabilities . ["asyncTasks"])))))))))

(defun agentel-async-task--merge (tasks id fields)
  "Return TASKS with the alist FIELDS merged into the task ID.
A task that is neither running nor paused is left out."
  (let ((tasks (copy-alist tasks))
        (task (copy-alist (alist-get id tasks nil nil #'equal))))
    (pcase-dolist (`(,key . ,value) fields)
      (setf (alist-get key task) value))
    (setf (alist-get id tasks nil t #'equal)
          (and (memq (alist-get 'state task) '(running paused)) task))
    tasks))

(defun agentel-async-task--update (message data)
  "Return the background tasks DATA of a session changed by MESSAGE.
DATA has the `tasks' still running or paused, newest first, as (ID
. TASK) where TASK is an alist of its `name', its `state' and its
latest `progress'.  MESSAGE is one of (spawn ID NAME), (progress ID
TEXT) and (change-state ID STATE); news of an unknown task is
ignored."
  (let ((tasks (alist-get 'tasks data)))
    `((tasks
       . ,(pcase message
            (`(spawn ,id ,name)
             (agentel-async-task--merge tasks id `((state . running) (name . ,name))))
            ((and `(,(or 'progress 'change-state) ,id . ,_)
                  (guard (not (assoc id tasks))))
             tasks)
            (`(progress ,id ,text)
             (agentel-async-task--merge tasks id `((progress . ,text))))
            (`(change-state ,id ,state)
             (agentel-async-task--merge tasks id `((state . ,state))))
            (_ (agentel-store-reject message)))))))

(agentel-store-define 'agentel-async-tasks #'agentel-async-task--update)

(defun agentel-async-task--on-update (session update)
  "Tell SESSION of the background task reported in UPDATE."
  (when-let* ((message
               (let-alist update
                 (pcase .sessionUpdate
                   ("async_task_spawned" `(spawn ,.asyncTaskId ,.name))
                   ("async_task_progress"
                    (when-let* ((progress (or .summary .lastToolName .description)))
                      `(progress ,.asyncTaskId ,progress)))
                   ("async_task_state_update"
                    `(change-state ,.asyncTaskId ,(and .state (intern .state))))))))
    (agentel-store-dispatch (agentel-session-store session) 'async-tasks
                            'agentel-async-tasks message)))

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
             (reverse (agentel-store-get model 'tasks))
             "\n"))

(agentel-ui-define-view 'agentel-async-tasks #'agentel-async-task--view :pin 10)

(add-hook 'agentel-connection-capability-functions #'agentel-async-task--capabilities)
(add-hook 'agentel-session-update-functions #'agentel-async-task--on-update)

(provide 'agentel-async-task)
;;; agentel-async-task.el ends here
