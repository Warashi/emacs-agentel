;;; agentel-background.el --- Background work of ACP v2 agents for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; In ACP version 2 the agent lists the work that runs independently of
;; its turns, such as a dynamic workflow, in the `background' of each
;; `state_update'.  The tool calls listed are pinned above the prompt
;; with their titles, which they keep updating while they run, until the
;; agent drops them from the list.
;;
;; A list left out of a `state_update' is not reported, so the pinned
;; work stays.  Work of other types is left out: claude-agent-acp lists
;; background subagents, which have sessions of their own.

;;; Code:

(require 'seq)
(require 'agentel-session)
(require 'agentel-conversation)
(require 'agentel-store)
(require 'agentel-ui)

(defface agentel-background-face
  '((t :inherit font-lock-builtin-face))
  "Face of the titles of background work."
  :group 'agentel)

(defun agentel-background--update (message data)
  "Return the background work DATA of a session changed by MESSAGE.
DATA has the `items' in the order the agent lists them, each
\(ID . FIELDS) with the `title' of the tool call ID.  MESSAGE is
\(list ITEMS), which replaces the items, or (update ID FIELDS), which
merges the alist FIELDS into the item ID; news of an unknown item is
ignored."
  (let ((items (alist-get 'items data)))
    `((items
       . ,(pcase message
            (`(list ,items) items)
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

(defun agentel-background--send (session message)
  "Change the background work of SESSION by MESSAGE."
  (agentel-store-dispatch (agentel-session-store session) 'background 'agentel-background
                          message))

(defun agentel-background--items (session)
  "Return the background work items of SESSION."
  (when-let* ((model (agentel-store-find (agentel-session-store session) 'background)))
    (agentel-store-get model 'items)))

(defun agentel-background--seen (session id)
  "Return the fields of the tool call ID as SESSION has seen it so far."
  (if-let* ((item (assoc id (agentel-background--items session))))
      (cdr item)
    (when-let* ((tool (agentel-conversation-find session (cons 'tool id))))
      `((title . ,(agentel-store-get tool 'title))))))

(defun agentel-background--list (session work)
  "Pin the tool calls among the background WORK of SESSION."
  (agentel-background--send
   session
   `(list ,(delq nil (mapcar (lambda (entry)
                               (let-alist entry
                                 (when (equal .type "tool_call")
                                   (cons .toolCallId
                                         (agentel-background--seen session .toolCallId)))))
                             work)))))

(defun agentel-background--on-update (session update)
  "Follow the background work of SESSION through UPDATE."
  (let-alist update
    (pcase .sessionUpdate
      ("state_update"
       (when .background
         (agentel-background--list session .background)))
      ("tool_call_update"
       (when (and .title (assoc .toolCallId (agentel-background--items session)))
         (agentel-background--send session `(update ,.toolCallId ((title . ,.title)))))))))

(defun agentel-background--view (model options)
  "Return the pinned lines of the background work of MODEL.
Each line fits in the :width of OPTIONS."
  (mapconcat (lambda (item)
               (agentel-ui-one-line
                "⚙ " (propertize (or (alist-get 'title (cdr item)) "Background work")
                                 'face 'agentel-background-face)
                (plist-get options :width)))
             (agentel-store-get model 'items)
             "\n"))

(agentel-ui-define-view 'agentel-background #'agentel-background--view :pin 11)

(add-hook 'agentel-session-update-functions #'agentel-background--on-update)

(provide 'agentel-background)
;;; agentel-background.el ends here
