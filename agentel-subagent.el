;;; agentel-subagent.el --- Subagents in their own buffers for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; Children that have not ended are also pinned above the parent's
;; prompt, since background subagents outlive the turn that spawned
;; them.

;;; Code:

(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-chat)
(require 'agentel-ui)

(defface agentel-subagent-face
  '((t :inherit font-lock-type-face))
  "Face of subagent names in the parent transcript."
  :group 'agentel)

(defun agentel-subagent--capabilities ()
  "Return the client capability of hosting subagent sessions."
  '((subagents . nil)
    (_meta . ((jetbrains . ((air . ((version . 1)
                                     (capabilities . ["nativeSubagentSessions"])))))))))

(defvar-keymap agentel-subagent-item-map
  :doc "Keymap on subagent items of a parent transcript."
  "RET" #'agentel-subagent-open
  "<mouse-1>" #'agentel-subagent-open)

(defun agentel-subagent--render (entry options)
  "Render the subagent item ENTRY of a parent transcript.
Its lines fit in the :width of OPTIONS."
  (let-alist (agentel-store-model-data entry)
    (let ((width (plist-get options :width)))
      (propertize
       (concat "⎇ "
               (propertize .name 'face 'agentel-subagent-face)
               " " (agentel-ui-state .state)
               "\n"
               (agentel-ui-one-line "    " .task width)
               (if .activity
                   (concat "\n" (agentel-ui-one-line "    ↳ " .activity width))
                 ""))
       'keymap agentel-subagent-item-map
       'agentel-subagent .child))))

(defun agentel-subagent--running (session)
  "Return the subagents of SESSION that have not ended, oldest first."
  (seq-remove #'agentel-session-ended (agentel-session-children session)))

(defun agentel-subagent--read-running ()
  "Ask for a running subagent of this buffer's session and return it."
  (let ((children (mapcar (lambda (child) (cons (agentel-session-name child) child))
                          (and agentel-chat--session
                               (agentel-subagent--running agentel-chat--session)))))
    (unless children
      (user-error "No subagent at point or running"))
    (cdr (assoc (completing-read "Subagent: "
                                 (agentel-chat-ordered-completion children) nil t)
                children))))

(defun agentel-subagent-open (&optional child)
  "Show the buffer of the subagent CHILD.
Interactively it is the subagent at point, or one of the running
subagents of the session read from the minibuffer."
  (interactive)
  (let ((child (or child (get-text-property (point) 'agentel-subagent)
                   (agentel-subagent--read-running))))
    (pop-to-buffer (agentel-chat-buffer child))))

(defun agentel-subagent--pin-line (child)
  "Return the pinned line of the running subagent CHILD."
  (let ((activity (agentel-session-data child 'subagent-activity))
        (map (make-sparse-keymap)))
    (keymap-set map "<mouse-1>" (lambda () (interactive) (agentel-subagent-open child)))
    (propertize
     (agentel-ui-one-line
      (concat "⎇ "
              (propertize (agentel-session-name child) 'face 'agentel-subagent-face)
              " " (agentel-ui-state (agentel-session-state child))
              (if activity " ↳ " ""))
      activity)
     'keymap map
     'mouse-face 'highlight
     'help-echo "mouse-1: visit the subagent")))

(defun agentel-subagent--pin (session)
  "Return the pinned lines of the running subagents of SESSION."
  (mapcar #'agentel-subagent--pin-line (agentel-subagent--running session)))

(defun agentel-subagent--update (message _data)
  "Return the data of a subagent item changed by MESSAGE.
MESSAGE is (show CHILD SEEN), where SEEN is an alist of what is shown
of CHILD.  CHILD is kept only to open its buffer."
  (pcase message
    (`(show ,child ,seen) `((child . ,child) ,@seen))))

(agentel-conversation-define 'subagent #'agentel-subagent--update)
(agentel-ui-define-view 'subagent #'agentel-subagent--render)

(defun agentel-subagent--show (child)
  "Show the state of CHILD as an item of its parent's transcript.
The item is keyed by the id of CHILD, so nothing is shown before it
has one."
  (when-let* ((id (agentel-session-id child)))
    (agentel-conversation-send
     (agentel-session-parent child) (cons 'subagent id)
     'subagent
     `(show ,child
            ((name . ,(agentel-session-name child))
             (state . ,(agentel-session-state child))
             (task . ,(agentel-session-data child 'subagent-task))
             (activity . ,(agentel-session-data child 'subagent-activity))
             (waiting . ,(and (agentel-session-waiting-p child) t)))))))

(defun agentel-subagent--spawn (parent update)
  "Create the subagent announced by UPDATE under PARENT."
  (let-alist update
    (unless (agentel-session-get .subagentSessionId
                                 (agentel-session-connection parent))
      (let ((child (agentel-session-create
                    :connection (agentel-session-connection parent)
                    :parent parent
                    :cwd (agentel-session-cwd parent))))
        (agentel-session-send child `(retitle ,.name))
        (agentel-session-register child .subagentSessionId)
        (setf (agentel-session-data child 'subagent-task) .task)
        (setf (agentel-session-data child 'subagent-running) t)
        (with-current-buffer (agentel-chat-open child)
          (setq default-directory (or (agentel-session-cwd parent) default-directory))
          (agentel-conversation-note child (concat "Task: " (or .task ""))))
        (agentel-subagent--show child)))))

(defun agentel-subagent--finish (parent update)
  "Record the end of the subagent of PARENT reported by UPDATE."
  (let-alist update
    (when-let* ((child (agentel-session-get .subagentSessionId
                                            (agentel-session-connection parent))))
      (agentel-session-send child `(end ,(intern .state))))))

(defun agentel-subagent--running-p (child)
  "Return non-nil if CHILD is a subagent, which runs until it ends."
  (agentel-session-data child 'subagent-running))

(defun agentel-subagent--note-activity (child update)
  "Remember the tool call in UPDATE as the latest activity of CHILD."
  (when-let* ((title (alist-get 'title update)))
    (setf (agentel-session-data child 'subagent-activity) title)))

(defun agentel-subagent--withhold-p (session update)
  "Withhold an orphan Agent/Task control UPDATE from the parent SESSION."
  (and (not (agentel-session-parent session))
       (equal (alist-get 'sessionUpdate update) "tool_call_update")
       (member (alist-get 'toolName (alist-get 'claudeCode (alist-get '_meta update)))
               '("Agent" "Task"))
       (not (agentel-conversation-find
             session (cons 'tool (alist-get 'toolCallId update))))))

(defun agentel-subagent--on-update (session update)
  "Handle subagent related UPDATE of SESSION."
  (pcase (alist-get 'sessionUpdate update)
    ("subagent_spawned" (agentel-subagent--spawn session update))
    ("subagent_state_update" (agentel-subagent--finish session update))
    ((or "tool_call" "tool_call_update")
     (when (agentel-session-parent session)
       (agentel-subagent--note-activity session update)))))

(defun agentel-subagent--on-changed (session)
  "Keep the parent's item and pin of SESSION in step with it."
  (when-let* ((parent (agentel-session-parent session)))
    (agentel-subagent--show session)
    (agentel-chat-refresh-pin parent)))

(add-hook 'agentel-connection-capability-functions #'agentel-subagent--capabilities)
(add-hook 'agentel-session-withhold-functions #'agentel-subagent--withhold-p)
(add-hook 'agentel-session-running-functions #'agentel-subagent--running-p)
(add-hook 'agentel-session-update-functions #'agentel-subagent--on-update)
(add-hook 'agentel-session-changed-functions #'agentel-subagent--on-changed)
(add-hook 'agentel-chat-pin-functions #'agentel-subagent--pin)
(keymap-set agentel-chat-mode-map "C-c C-j" #'agentel-subagent-open)

(provide 'agentel-subagent)
;;; agentel-subagent.el ends here
