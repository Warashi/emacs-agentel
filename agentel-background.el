;;; agentel-background.el --- Background work of ACP v2 agents for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; In ACP version 2 the agent lists the work that runs independently of
;; its turns, such as a dynamic workflow, in the `background' of each
;; `state_update'.  The tool calls listed are pinned above the prompt
;; with their titles, which they keep updating while they run, and the
;; terminals with their commands, until the agent drops them from the
;; list.  Opening one, by clicking its pinned
;; line or with `agentel-background-open', shows its latest report in a
;; buffer of its own, which keeps the last report once the work ended.
;; The report of a terminal is its output.
;; The work also shows below its session in `agentel-list'.
;;
;; A list left out of a `state_update' is not reported, so the pinned
;; work stays.  Work of other types is left out: claude-agent-acp lists
;; background subagents, which have sessions of their own.

;;; Code:

(require 'seq)
(require 'agentel-session)
(require 'agentel-conversation)
(require 'agentel-chat)
(require 'agentel-list)
(require 'agentel-store)
(require 'agentel-ui)
(require 'agentel-terminal)

(defface agentel-background-face
  '((t :inherit font-lock-builtin-face))
  "Face of the titles of background work."
  :group 'agentel)

(defun agentel-background--update (message data)
  "Return the background work DATA of a session changed by MESSAGE.
DATA has the `session' they run in, kept to open them, and the `items'
in the order the agent lists them, each (ID . FIELDS) with the `title'
and the latest `report' of the tool call or the terminal ID.  MESSAGE is
\(list SESSION ITEMS), which replaces the items, or (update ID FIELDS),
which merges the alist FIELDS into the item ID; news of an unknown item
is ignored."
  (let ((items (alist-get 'items data)))
    `((session . ,(if (eq (car message) 'list) (cadr message) (alist-get 'session data)))
      (items
       . ,(pcase message
            (`(list ,_session ,items) items)
            (`(update ,id ,fields)
             (mapcar (lambda (item)
                       (if (equal (car item) id)
                           (cons id (append fields
                                            (seq-remove (lambda (field)
                                                          (assq (car field) fields))
                                                        (cdr item))))
                         item))
                     items))
            (_ (agentel-store-reject message)))))))

(agentel-store-define 'agentel-background #'agentel-background--update)

(defun agentel-background--title (item)
  "Return the title of the background work ITEM."
  (or (alist-get 'title (cdr item)) "Background work"))

(defun agentel-background--send (session message)
  "Change the background work of SESSION by MESSAGE.
The session is told it changed when the titles of its work did, so the
list shows them."
  (let ((titles (mapcar #'agentel-background--title (agentel-background--items session))))
    (agentel-store-dispatch (agentel-session-store session) 'background 'agentel-background
                            message)
    (unless (equal titles (mapcar #'agentel-background--title
                                  (agentel-background--items session)))
      (agentel-session-changed session))))

(defun agentel-background--items (session)
  "Return the background work items of SESSION."
  (when-let* ((model (agentel-store-find (agentel-session-store session) 'background)))
    (agentel-store-get model 'items)))

(defun agentel-background--seen (session id)
  "Return the fields of the tool call ID as SESSION has seen it so far."
  (if-let* ((item (assoc id (agentel-background--items session))))
      (cdr item)
    (when-let* ((tool (agentel-conversation-find session (cons 'tool id))))
      `((title . ,(agentel-store-get tool 'title))
        (report . ,(agentel-store-get tool 'output))))))

(defun agentel-background--terminal (session id)
  "Return the fields of the terminal ID of SESSION."
  (let-alist (agentel-terminal-get session id)
    (seq-filter #'cdr `((title . ,.command) (report . ,.output)))))

(defun agentel-background--report (content)
  "Return the text of the tool call CONTENT, or nil without any."
  (when-let* ((texts (delq nil (mapcar (lambda (item)
                                         (when (equal (alist-get 'type item) "content")
                                           (alist-get 'text (alist-get 'content item))))
                                       content))))
    (string-join texts "\n")))

(defun agentel-background--list (session work)
  "Pin the tool calls and the terminals among the background WORK of SESSION."
  (agentel-background--send
   session
   `(list ,session
          ,(delq nil (mapcar (lambda (entry)
                               (let-alist entry
                                 (pcase .type
                                   ("tool_call"
                                    (cons .toolCallId
                                          (agentel-background--seen session .toolCallId)))
                                   ("terminal"
                                    (cons .terminalId
                                          (agentel-background--terminal session .terminalId))))))
                             work)))))

(defun agentel-background--on-update (session update)
  "Follow the background work of SESSION through UPDATE."
  (let-alist update
    (pcase .sessionUpdate
      ("state_update"
       (when .background
         (agentel-background--list session .background)))
      ("tool_call_update"
       (when-let* (((assoc .toolCallId (agentel-background--items session)))
                   (fields (seq-filter #'cdr
                                       `((title . ,.title)
                                         (report . ,(agentel-background--report .content))))))
         (agentel-background--send session `(update ,.toolCallId ,fields))))
      ((or "terminal_update" "terminal_output_chunk")
       (when (assoc .terminalId (agentel-background--items session))
         (agentel-background--send
          session `(update ,.terminalId
                           ,(agentel-background--terminal session .terminalId))))))))

(defun agentel-background--pin-line (session item width)
  "Return the pinned line of the background work ITEM of SESSION.
It fits in WIDTH, and clicking it opens the work."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mouse-1>"
                (lambda () (interactive)
                  (pop-to-buffer (agentel-background-open session (car item)))))
    (propertize
     (agentel-ui-one-line "⚙ " (propertize (agentel-background--title item)
                                           'face 'agentel-background-face)
                          width)
     'keymap map
     'mouse-face 'highlight
     'help-echo "mouse-1: show the report of the work")))

(defun agentel-background--view (model options)
  "Return the pinned lines of the background work of MODEL.
Each line fits in the :width of OPTIONS."
  (let ((session (agentel-store-get model 'session)))
    (mapconcat (lambda (item)
                 (agentel-background--pin-line session item (plist-get options :width)))
               (agentel-store-get model 'items)
               "\n")))

(agentel-ui-define-view 'agentel-background #'agentel-background--view :pin 11)

;;;; The buffer of a work

(defvar agentel-background--buffers (make-hash-table :test 'equal)
  "Buffers showing background work, by (SESSION . ID).")

(defvar-local agentel-background--shown nil
  "The report shown in this buffer.")

(define-derived-mode agentel-background-mode special-mode "agentel-background"
  "Major mode of the buffer showing the report of a background work.")

(defun agentel-background--show (report)
  "Show REPORT in this buffer unless it shows it already."
  (when (and report (not (equal report agentel-background--shown)))
    (setq agentel-background--shown report)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (if agentel-chat-format-message-function
                  (funcall agentel-chat-format-message-function report
                           (when-let* ((window (get-buffer-window)))
                             (window-width window)))
                report))
      (goto-char (point-min)))))

(defun agentel-background--buffer (session id)
  "Return a new buffer that follows the report of the work ID of SESSION."
  (let* ((buffer (generate-new-buffer
                  (format "*agentel background: %s*"
                          (agentel-background--title
                           (assoc id (agentel-background--items session))))))
         (store (agentel-session-store session))
         (follow (lambda (_model _added)
                   (when-let* ((item (assoc id (agentel-background--items session))))
                     (with-current-buffer buffer
                       (agentel-background--show (alist-get 'report (cdr item))))))))
    (with-current-buffer buffer
      (agentel-background-mode)
      (agentel-store-subscribe store follow 'background)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (agentel-store-unsubscribe store follow)
                  (remhash (cons session id) agentel-background--buffers))
                nil t))
    (funcall follow nil nil)
    (puthash (cons session id) buffer agentel-background--buffers)
    buffer))

(defun agentel-background--read (session)
  "Ask for a background work of SESSION and return its id."
  (let ((items (mapcar (lambda (item) (cons (agentel-background--title item) (car item)))
                       (agentel-background--items session))))
    (unless items
      (user-error "No work running in the background"))
    (cdr (assoc (completing-read "Background work: "
                                 (agentel-chat-ordered-completion items) nil t)
                items))))

(defun agentel-background-open (session id)
  "Show the report of the background work ID of SESSION in its buffer.
Interactively it is a work of the session of this buffer read from the
minibuffer.  Return the buffer."
  (interactive
   (let ((session (agentel-session-current)))
     (list session (agentel-background--read session))))
  (let ((buffer (or (let ((buffer (gethash (cons session id) agentel-background--buffers)))
                      (and (buffer-live-p buffer) buffer))
                    (agentel-background--buffer session id))))
    (when (called-interactively-p 'any)
      (pop-to-buffer buffer))
    buffer))

(defun agentel-background--list-work (session)
  "Return the background work of SESSION as `agentel-list' shows it."
  (mapcar (lambda (item)
            (list :title (agentel-background--title item)
                  :open (lambda ()
                          (pop-to-buffer (agentel-background-open session (car item))))))
          (agentel-background--items session)))

(keymap-set agentel-chat-mode-map "C-c C-b" #'agentel-background-open)
(add-hook 'agentel-list-work-functions #'agentel-background--list-work)

(add-hook 'agentel-session-update-functions #'agentel-background--on-update)

(provide 'agentel-background)
;;; agentel-background.el ends here
