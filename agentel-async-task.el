;;; agentel-async-task.el --- Background tasks of the agent for agentel  -*- lexical-binding: t; -*-

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
(require 'agentel-chat)

(defface agentel-async-task-face
  '((t :inherit font-lock-builtin-face))
  "Face of background task names."
  :group 'agentel)

(defun agentel-async-task--capabilities ()
  "Return the client capability of receiving background tasks."
  '((_meta . ((jetbrains . ((air . ((version . 1)
                                    (capabilities . ["asyncTasks"])))))))))

(defun agentel-async-task--update (session id fields)
  "Merge the alist FIELDS into the task ID of SESSION.
A task that is neither running nor paused is forgotten."
  (let* ((tasks (copy-alist (agentel-session-data session 'async-tasks)))
         (task (append fields (alist-get id tasks nil nil #'equal))))
    (setf (alist-get id tasks nil t #'equal)
          (and (member (alist-get 'state task) '("running" "paused")) task))
    (setf (agentel-session-data session 'async-tasks) tasks)))

(defun agentel-async-task--on-update (session update)
  "Record the background task reported in UPDATE for SESSION."
  (let-alist update
    (let ((known (assoc .asyncTaskId (agentel-session-data session 'async-tasks))))
      (pcase .sessionUpdate
        ("async_task_spawned"
         (agentel-async-task--update session .asyncTaskId
                                     `((state . "running") (name . ,.name))))
        ("async_task_progress"
         (when-let* ((progress (or .summary .lastToolName .description))
                     (known))
           (agentel-async-task--update session .asyncTaskId
                                       `((progress . ,progress)))))
        ("async_task_state_update"
         (when known
           (agentel-async-task--update session .asyncTaskId
                                       `((state . ,.state)))))))))

(defun agentel-async-task--pin (session)
  "Return the pinned lines of the running background tasks of SESSION."
  (mapcar (lambda (item)
            (let-alist (cdr item)
              (concat "⚙ " (propertize (or .name "Background task")
                                       'face 'agentel-async-task-face)
                      (format " [%s]" .state)
                      (if .progress (concat " ↳ " .progress) ""))))
          (reverse (agentel-session-data session 'async-tasks))))

(add-hook 'agentel-connection-capability-functions #'agentel-async-task--capabilities)
(add-hook 'agentel-session-update-functions #'agentel-async-task--on-update)
(add-hook 'agentel-chat-pin-functions #'agentel-async-task--pin)

(provide 'agentel-async-task)
;;; agentel-async-task.el ends here
