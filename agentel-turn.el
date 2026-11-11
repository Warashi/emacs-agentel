;;; agentel-turn.el --- Turns of agentel sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A turn is the agent working on one prompt of the user.
;; `agentel-turn-prompt' records the prompt in the conversation and
;; sends it with `session/prompt', and `agentel-turn-cancel' asks the
;; agent to stop with `session/cancel'.  The reason a turn ended is
;; noted in the conversation unless it is its end.
;;
;; In ACP version 1 the session is in its turn until the agent answers
;; the prompt.  In version 2 the agent answers once it took the prompt,
;; and tells the state of the session with `state_update': a turn runs
;; from `running', also one the agent started itself, until an `idle'
;; with a stop reason.  An `idle' without one only tells that the
;; background work ended.

;;; Code:

(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)

(defun agentel-turn--v2-p (session)
  "Return non-nil if the agent of SESSION speaks ACP version 2."
  (eql (agentel-connection-version (agentel-session-connection session)) 2))

(defun agentel-turn--finish (session)
  "Record that the turn of SESSION ended."
  (agentel-session-send session '(finish-turn))
  (agentel-conversation-finish-message session))

(defun agentel-turn--end (session reason)
  "End the turn of SESSION that stopped for REASON."
  (agentel-turn--finish session)
  (unless (member reason '("end_turn" nil))
    (agentel-conversation-note session (format "Turn ended: %s" reason) 'stop)))

(defun agentel-turn--fail (session error)
  "End the turn of SESSION that failed with ERROR."
  (agentel-turn--finish session)
  (agentel-conversation-note session
                             (format "Prompt failed: %s" (alist-get 'message error))
                             'error))

(defun agentel-turn-prompt (session text)
  "Send TEXT to the agent as a prompt of SESSION."
  (agentel-conversation-prompt session text)
  (agentel-session-send session '(start-turn))
  (agentel-connection-request
   (agentel-session-connection session) "session/prompt"
   `((sessionId . ,(agentel-session-id session))
     (prompt . [((type . "text") (text . ,text))]))
   :on-success
   (lambda (result)
     (unless (agentel-turn--v2-p session)
       (agentel-turn--end session (alist-get 'stopReason result))))
   :on-failure (lambda (error) (agentel-turn--fail session error))))

(defun agentel-turn--on-update (session update)
  "Follow the turns of SESSION through the `state_update' UPDATE."
  (when (and (equal (alist-get 'sessionUpdate update) "state_update")
             (not (agentel-session-parent session)))
    (let-alist update
      (pcase .state
        ("running"
         (unless (agentel-session-in-turn session)
           (agentel-session-send session '(start-turn))))
        ((and "idle" (guard .stopReason))
         (if (equal .stopReason "error")
             (agentel-turn--fail session .error)
           (agentel-turn--end session .stopReason)))))))

(add-hook 'agentel-session-update-functions #'agentel-turn--on-update)

(defun agentel-turn-cancel (session)
  "Ask the agent to stop the current turn of SESSION."
  (agentel-connection-notify (agentel-session-connection session)
                             "session/cancel"
                             `((sessionId . ,(agentel-session-id session)))))

(provide 'agentel-turn)
;;; agentel-turn.el ends here
