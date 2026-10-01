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

(defun agentel-subagent--update-task (message data)
  "Return the DATA of what a subagent works on changed by MESSAGE.
DATA has the `task' it was given and its latest `activity'.  MESSAGE
is (assign TASK) or (act ACTIVITY)."
  (pcase-let ((`(,field . ,value)
               (pcase message
                 (`(assign ,task) `(task . ,task))
                 (`(act ,activity) `(activity . ,activity))
                 (_ (agentel-store-reject message)))))
    (cons (cons field value) (assq-delete-all field (copy-alist data)))))

(agentel-store-define 'agentel-subagent-task #'agentel-subagent--update-task)

(defun agentel-subagent--send (child message)
  "Change what the subagent CHILD works on by MESSAGE.
See `agentel-subagent--update-task'."
  (agentel-store-dispatch (agentel-session-store child) 'subagent
                          'agentel-subagent-task message))

(defun agentel-subagent--get (child field)
  "Return the value FIELD of what the subagent CHILD works on has now."
  (when-let* ((model (agentel-store-find (agentel-session-store child) 'subagent)))
    (agentel-store-get model field)))

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
                          (when-let* ((session (agentel-session-current)))
                            (agentel-subagent--running session)))))
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

(defun agentel-subagent--update-running (message data)
  "Return the DATA of the running subagents of a session changed by MESSAGE.
DATA has the `children' that have not ended, oldest first, each as
\(CHILD . SEEN) where SEEN is an alist of what is shown of CHILD.
MESSAGE is (show CHILD SEEN), or (forget CHILD) once CHILD ended.
CHILD is kept only to open its buffer."
  (let ((children (alist-get 'children data)))
    `((children
       . ,(pcase message
            (`(show ,child ,seen)
             (if (assq child children)
                 (mapcar (lambda (entry)
                           (if (eq (car entry) child) (cons child seen) entry))
                         children)
               (append children (list (cons child seen)))))
            (`(forget ,child)
             (seq-remove (lambda (entry) (eq (car entry) child)) children))
            (_ (agentel-store-reject message)))))))

(agentel-store-define 'agentel-subagent-running #'agentel-subagent--update-running)

(defun agentel-subagent--pin-line (child seen width)
  "Return the pinned line of the running subagent CHILD, of which SEEN is seen.
It fits in WIDTH."
  (let-alist seen
    (let ((map (make-sparse-keymap)))
      (keymap-set map "<mouse-1>" (lambda () (interactive) (agentel-subagent-open child)))
      (propertize
       (agentel-ui-one-line
        (concat "⎇ "
                (propertize .name 'face 'agentel-subagent-face)
                " " (agentel-ui-state .state)
                (if .activity " ↳ " ""))
        .activity
        width)
       'keymap map
       'mouse-face 'highlight
       'help-echo "mouse-1: visit the subagent"))))

(defun agentel-subagent--pin (model options)
  "Return the pinned lines of the running subagents of MODEL.
Each line fits in the :width of OPTIONS."
  (mapconcat (pcase-lambda (`(,child . ,seen))
               (agentel-subagent--pin-line child seen (plist-get options :width)))
             (agentel-store-get model 'children)
             "\n"))

(agentel-ui-define-view 'agentel-subagent-running #'agentel-subagent--pin :pin 20)

(defun agentel-subagent--update (message _data)
  "Return the data of a subagent item changed by MESSAGE.
MESSAGE is (show CHILD SEEN), where SEEN is an alist of what is shown
of CHILD.  CHILD is kept only to open its buffer."
  (pcase message
    (`(show ,child ,seen) `((child . ,child) ,@seen))
    (_ (agentel-store-reject message))))

(agentel-conversation-define 'subagent #'agentel-subagent--update)
(agentel-ui-define-view 'subagent #'agentel-subagent--render)

(defun agentel-subagent--show (child)
  "Show the state of CHILD in its parent, as an item and in the pin.
The item is keyed by the id of CHILD, so it is not shown before CHILD
has one.  The pin shows CHILD until it ends."
  (let ((parent (agentel-session-parent child))
        (seen `((name . ,(agentel-session-name child))
                (state . ,(agentel-session-state child))
                (task . ,(agentel-subagent--get child 'task))
                (activity . ,(agentel-subagent--get child 'activity))
                (waiting . ,(and (agentel-session-waiting-p child) t)))))
    (when-let* ((id (agentel-session-id child)))
      (agentel-conversation-send parent (cons 'subagent id) 'subagent
                                 `(show ,child ,seen)))
    (agentel-store-dispatch (agentel-session-store parent) 'subagents
                            'agentel-subagent-running
                            (if (agentel-session-ended child)
                                `(forget ,child)
                              `(show ,child ,seen)))))

(defun agentel-subagent--follow (child)
  "Keep the item and the pin of CHILD in its parent in step with CHILD.
They show its name and state and what it works on, so they follow
those models of CHILD and no other."
  (let ((store (agentel-session-store child))
        (show (lambda (_model _added) (agentel-subagent--show child))))
    (agentel-store-subscribe store show 'state)
    (agentel-store-subscribe store show 'subagent)))

(defun agentel-subagent--follow-waiting (session)
  "Show in the parent's item whether the subagent SESSION waits.
Nothing keeps whether a session waits, so the item is sent again when
the session changes and the item tells otherwise."
  (when-let* ((parent (agentel-session-parent session))
              (id (agentel-session-id session))
              (item (agentel-conversation-find parent (cons 'subagent id))))
    (unless (eq (and (agentel-session-waiting-p session) t)
                (agentel-store-get item 'waiting))
      (agentel-subagent--show session))))

(defun agentel-subagent--resolve (id connection)
  "Return a subagent active under generation ID on CONNECTION."
  (seq-find
   (lambda (child)
     (and (agentel-session-parent child)
          (or (not connection)
              (eq (agentel-session-connection child) connection))
          (equal id (agentel-session-data child 'subagent-current-id))))
   (agentel-session-list)))

(defun agentel-subagent--resumed-child (parent id)
  "Return the finished child of PARENT resumed under generation ID."
  (when (and (stringp id)
             (string-match "\\`\\(.*\\):generation:\\([0-9]+\\)\\'" id)
             (> (string-to-number (match-string 2 id)) 1))
    (let ((child (agentel-session-get (match-string 1 id)
                                      (agentel-session-connection parent))))
      (when (and child (eq (agentel-session-parent child) parent)
                 (agentel-session-ended child))
        child))))

(defun agentel-subagent--spawn (parent update)
  "Create or resume the subagent announced by UPDATE under PARENT."
  (let-alist update
    (unless (agentel-session-get .subagentSessionId
                                 (agentel-session-connection parent))
      (if-let* ((child (agentel-subagent--resumed-child parent .subagentSessionId)))
          (progn
            (setf (alist-get 'subagent-current-id (agentel-session-alist child))
                  .subagentSessionId)
            (agentel-subagent--send child '(act nil))
            (agentel-session-send child '(resume))
            (agentel-conversation-note child (concat "Task: " (or .task ""))))
        (let ((child (agentel-session-create
                      :connection (agentel-session-connection parent)
                      :parent parent
                      :cwd (agentel-session-cwd parent))))
          (agentel-subagent--follow child)
          (agentel-session-send child `(retitle ,.name))
          (agentel-session-register child .subagentSessionId)
          (agentel-subagent--send child `(assign ,.task))
          (with-current-buffer (agentel-chat-open child)
            (setq default-directory (or (agentel-session-cwd parent) default-directory))
            (agentel-conversation-note child (concat "Task: " (or .task ""))))
          (agentel-subagent--show child))))))

(defun agentel-subagent--finish (parent update)
  "Record the end of the subagent of PARENT reported by UPDATE."
  (let-alist update
    (when-let* ((child (agentel-session-get .subagentSessionId
                                            (agentel-session-connection parent))))
      (when (and (eq (agentel-session-parent child) parent)
                 (equal .subagentSessionId
                        (or (agentel-session-data child 'subagent-current-id)
                            (agentel-session-id child))))
        (agentel-session-send child `(end ,(intern .state)))))))

(defun agentel-subagent--running-p (child)
  "Return non-nil if CHILD was given a task, which it works on until it ends."
  (agentel-store-find (agentel-session-store child) 'subagent))

(defun agentel-subagent--note-activity (child update)
  "Remember the tool call in UPDATE as the latest activity of CHILD."
  (when-let* ((title (alist-get 'title update)))
    (agentel-subagent--send child `(act ,title))))

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

(add-hook 'agentel-connection-capability-functions #'agentel-subagent--capabilities)
(add-hook 'agentel-session-resolve-functions #'agentel-subagent--resolve)
(add-hook 'agentel-session-withhold-functions #'agentel-subagent--withhold-p)
(add-hook 'agentel-session-running-functions #'agentel-subagent--running-p)
(add-hook 'agentel-session-update-functions #'agentel-subagent--on-update)
(add-hook 'agentel-session-changed-functions #'agentel-subagent--follow-waiting)
(keymap-set agentel-chat-mode-map "C-c C-j" #'agentel-subagent-open)

(provide 'agentel-subagent)
;;; agentel-subagent.el ends here
