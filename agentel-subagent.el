;;; agentel-subagent.el --- Subagents in their own buffers for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; With the `subagents' client capability, claude-agent-acp runs each
;; subagent in its own ACP session: it announces the child with
;; `subagent_spawned' in the parent, sends the child's messages and tool
;; calls with the child's session id, and reports the end with
;; `subagent_state_update'.  The Agent/Task tool call itself is not sent
;; to the parent.
;;
;; Each child gets a read-only buffer of its own, so its output never
;; mixes with the parent's.  The parent shows one item per child with
;; its state and latest tool call, from which the child buffer opens.

;;; Code:

(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)

(defface agentel-subagent-face
  '((t :inherit font-lock-type-face))
  "Face of subagent names in the parent transcript."
  :group 'agentel)

(defun agentel-subagent--capabilities ()
  "Return the client capability of hosting subagent sessions."
  '((subagents . nil)))

(defun agentel-subagent--first-line (text)
  "Return the first line of TEXT, shortened."
  (truncate-string-to-width (car (split-string (string-trim (or text "")) "\n"))
                            80 nil nil "…"))

(defvar-keymap agentel-subagent-item-map
  :doc "Keymap on subagent items of a parent transcript."
  "RET" #'agentel-subagent-open
  "<mouse-1>" #'agentel-subagent-open)

(defun agentel-subagent--render (entry)
  "Render the subagent item ENTRY of a parent transcript."
  (let* ((child (agentel-chat-entry-get entry 'child))
         (activity (agentel-session-data child 'subagent-activity)))
    (propertize
     (concat "⎇ "
             (propertize (agentel-session-name child) 'face 'agentel-subagent-face)
             (format " [%s]" (agentel-session-state child))
             "\n    "
             (agentel-subagent--first-line (agentel-session-data child 'subagent-task))
             (if activity (concat "\n    ↳ " activity) ""))
     'keymap agentel-subagent-item-map
     'agentel-subagent child)))

(defun agentel-subagent-open (&optional child)
  "Show the buffer of the subagent CHILD, by default the one at point."
  (interactive)
  (let ((child (or child (get-text-property (point) 'agentel-subagent)
                   (user-error "No subagent at point"))))
    (pop-to-buffer (agentel-session-buffer child))))

(defun agentel-subagent--refresh (child)
  "Render the item of CHILD in its parent's buffer again."
  (when-let* ((parent (agentel-session-parent child))
              (buffer (agentel-session-buffer parent))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when-let* ((entry (agentel-chat-find (cons 'subagent (agentel-session-id child)))))
        (agentel-chat-refresh entry)))))

(defun agentel-subagent--spawn (parent update)
  "Create the subagent announced by UPDATE under PARENT."
  (let-alist update
    (unless (agentel-session-get .subagentSessionId)
      (let ((child (agentel-session-create
                    :connection (agentel-session-connection parent)
                    :parent parent
                    :cwd (agentel-session-cwd parent))))
        (setf (agentel-session-title child) .name)
        (agentel-session-register child .subagentSessionId)
        (setf (agentel-session-data child 'subagent-task) .task)
        (agentel-session-set-busy child t)
        (with-current-buffer (agentel-chat-open child)
          (setq default-directory (or (agentel-session-cwd parent) default-directory))
          (agentel-chat-notice child (concat "Task: " (or .task ""))))
        (when-let* ((buffer (agentel-session-buffer parent))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer
            (agentel-chat-add (cons 'subagent .subagentSessionId) 'subagent
                              #'agentel-subagent--render `((child . ,child)))))))))

(defun agentel-subagent--finish (update)
  "Record the end of the subagent reported by UPDATE."
  (let-alist update
    (when-let* ((child (agentel-session-get .subagentSessionId)))
      (agentel-session-set-ended child (intern .state)))))

(defun agentel-subagent--note-activity (child update)
  "Remember the tool call in UPDATE as the latest activity of CHILD."
  (when-let* ((title (alist-get 'title update)))
    (setf (agentel-session-data child 'subagent-activity) title)))

(defun agentel-subagent--on-update (session update)
  "Handle subagent related UPDATE of SESSION."
  (pcase (alist-get 'sessionUpdate update)
    ("subagent_spawned" (agentel-subagent--spawn session update))
    ("subagent_state_update" (agentel-subagent--finish update))
    ((or "tool_call" "tool_call_update")
     (when (agentel-session-parent session)
       (agentel-subagent--note-activity session update)))))

(defun agentel-subagent--on-changed (session)
  "Keep the parent's item of SESSION in step with it."
  (when (agentel-session-parent session)
    (agentel-subagent--refresh session)))

(add-hook 'agentel-connection-capability-functions #'agentel-subagent--capabilities)
(add-hook 'agentel-session-update-functions #'agentel-subagent--on-update)
(add-hook 'agentel-session-changed-functions #'agentel-subagent--on-changed)

(provide 'agentel-subagent)
;;; agentel-subagent.el ends here
