;;; agentel-conversation.el --- Conversations of agentel sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The conversation of a session is what was said and done in it, in
;; order: prompts, messages and thoughts of the agent, tool calls, the
;; plan and notes such as why a turn ended.  It is built here from the
;; `session/update' notifications and knows nothing of how it is shown;
;; buffers subscribe to it and draw it their own way.
;;
;; Each item is a model of `agentel-store'.  Features add items of their
;; own types, such as permission requests, with
;; `agentel-conversation-define' and `agentel-conversation-send'.

;;; Code:

(require 'subr-x)
(require 'agentel-session)
(require 'agentel-store)

(defun agentel-conversation-define (type update)
  "Let the conversation hold items of TYPE, changed by UPDATE.
UPDATE is a function taking a message and the data alist of an item,
nil for a new one, and returning its new data."
  (declare (indent 1))
  (agentel-store-define type update))

(defun agentel-conversation--finish-previous (store item)
  "Mark the agent message before ITEM, the last one of STORE, as complete."
  (let ((previous (agentel-store-last store 1)))
    (when (and previous (eq (agentel-store-model-type previous) 'agent)
               (not (agentel-store-get previous 'finished)))
      (agentel-store-update store previous '(finish)))
    item))

(defun agentel-conversation--store (session)
  "Return the store of the conversation of SESSION."
  (or (agentel-session-data session 'conversation)
      (let ((store (agentel-store-create)))
        ;; A message ends when anything else is said after it.
        (agentel-store-subscribe
         store (lambda (item added)
                 (when added (agentel-conversation--finish-previous store item))))
        ;; Making the store changes nothing shown, so listeners are not told.
        (setf (alist-get 'conversation (agentel-session-alist session)) store))))

(defun agentel-conversation-send (session key type message)
  "Send MESSAGE to the item of TYPE under KEY in the conversation of SESSION.
Without such an item, one is added to the end, so a nil KEY adds one
every time.  Return the item."
  (agentel-store-dispatch (agentel-conversation--store session) key type message))

(defun agentel-conversation-items (session)
  "Return the items of the conversation of SESSION, oldest first."
  (agentel-store-models (agentel-conversation--store session)))

(defun agentel-conversation-find (session key)
  "Return the item under KEY in the conversation of SESSION."
  (agentel-store-find (agentel-conversation--store session) key))

(defun agentel-conversation-subscribe (session function)
  "Call FUNCTION whenever an item of the conversation of SESSION changes.
It is called with the item and non-nil when the item was added."
  (agentel-store-subscribe (agentel-conversation--store session) function))

(defun agentel-conversation-unsubscribe (session function)
  "Stop calling FUNCTION for the changes of the conversation of SESSION."
  (agentel-store-unsubscribe (agentel-conversation--store session) function))

;;;; Items

(defun agentel-conversation--update-text (message data)
  "Return the DATA of a text message changed by MESSAGE.
More text makes a finished message unfinished until it ends again."
  (pcase message
    (`(chunk ,text) `((text . ,(concat (alist-get 'text data) text))))
    ('(finish) `((text . ,(alist-get 'text data)) (finished . t)))))

(dolist (type '(user agent thought))
  (agentel-conversation-define type #'agentel-conversation--update-text))

(dolist (type '(notice error stop))
  (agentel-conversation-define type
    (lambda (message _data)
      (pcase message
        (`(show ,text) `((text . ,text)))))))

(defun agentel-conversation--update-tool (message data)
  "Return the DATA of a tool call changed by MESSAGE."
  (pcase message
    (`(update ,update)
     (dolist (field '(title kind status content rawInput locations) data)
       (when-let* ((value (alist-get field update)))
         (setf (alist-get field data) value))))))

(agentel-conversation-define 'tool #'agentel-conversation--update-tool)

(agentel-conversation-define 'plan
  (lambda (message _data)
    (pcase message
      (`(show ,entries) `((entries . ,entries))))))

(defun agentel-conversation-prompt (session text)
  "Record TEXT sent by the user to SESSION."
  (agentel-conversation-send session nil 'user `(chunk ,text)))

(defun agentel-conversation-note (session text &optional type)
  "Record the note TEXT of agentel in the conversation of SESSION.
TYPE is `error' for errors and `stop' for why a turn ended early."
  (agentel-conversation-send session nil (or type 'notice) `(show ,text)))

(defun agentel-conversation-finish-message (session)
  "Mark the agent message at the end of the conversation of SESSION as complete."
  (let* ((store (agentel-conversation--store session))
         (item (agentel-store-last store)))
    (when (and item (eq (agentel-store-model-type item) 'agent)
               (not (agentel-store-get item 'finished)))
      (agentel-store-update store item '(finish)))))

;;;; Updates from the agent

(defun agentel-conversation--text-chunk (session type update)
  "Add the text chunk UPDATE of SESSION to a message of TYPE."
  (let ((text (alist-get 'text (alist-get 'content update)))
        (store (agentel-conversation--store session)))
    (when (and text (not (string-empty-p text)))
      (if-let* ((last (agentel-store-last store))
                ((eq (agentel-store-model-type last) type)))
          (agentel-store-update store last `(chunk ,text))
        (agentel-store-dispatch store nil type `(chunk ,text))))))

(defun agentel-conversation--on-update (session update)
  "Record UPDATE of SESSION in its conversation."
  (pcase (alist-get 'sessionUpdate update)
    ("agent_message_chunk" (agentel-conversation--text-chunk session 'agent update))
    ("agent_thought_chunk" (agentel-conversation--text-chunk session 'thought update))
    ("user_message_chunk" (agentel-conversation--text-chunk session 'user update))
    ((or "tool_call" "tool_call_update")
     (agentel-conversation-send session (cons 'tool (alist-get 'toolCallId update))
                                'tool `(update ,update)))
    ;; A plan replaces the previous one.
    ("plan" (agentel-conversation-send session 'plan 'plan
                                       `(show ,(alist-get 'entries update))))))

(add-hook 'agentel-session-update-functions #'agentel-conversation--on-update)

(provide 'agentel-conversation)
;;; agentel-conversation.el ends here
