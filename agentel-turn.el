;;; agentel-turn.el --- Turns of agentel sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A turn is the agent working on one prompt of the user.
;; `agentel-turn-prompt' records the prompt in the conversation and
;; sends it with `session/prompt', and `agentel-turn-cancel' asks the
;; agent to stop with `session/cancel'.  The session is in its turn
;; until the agent answers the prompt, and the answer is noted in the
;; conversation when the turn ended for another reason than its end.

;;; Code:

(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)

(defun agentel-turn--finish (session)
  "Record that the turn of SESSION ended."
  (agentel-session-send session '(finish-turn))
  (agentel-conversation-finish-message session))

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
     (agentel-turn--finish session)
     (let ((reason (alist-get 'stopReason result)))
       (unless (member reason '("end_turn" nil))
         (agentel-conversation-note session (format "Turn ended: %s" reason) 'stop))))
   :on-failure
   (lambda (error)
     (agentel-turn--finish session)
     (agentel-conversation-note session
                                (format "Prompt failed: %s" (alist-get 'message error))
                                'error))))

(defun agentel-turn-cancel (session)
  "Ask the agent to stop the current turn of SESSION."
  (agentel-connection-notify (agentel-session-connection session)
                             "session/cancel"
                             `((sessionId . ,(agentel-session-id session)))))

(provide 'agentel-turn)
;;; agentel-turn.el ends here
