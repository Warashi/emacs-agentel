;;; agentel-resume.el --- Resume earlier sessions for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The /resume command lists the earlier sessions of the working
;; directory with `session/list' and opens the chosen one in a new
;; buffer, replaying its history with `session/load'.  The agent does
;; not offer /resume itself because its own picker is terminal only.

;;; Code:

(require 'seq)
(require 'iso8601)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-commands)

(declare-function agentel-start "agentel")

(defun agentel-resume--describe (session)
  "Return the completion candidate of the listed SESSION."
  (let-alist session
    (concat (if .updatedAt
                (format-time-string "%Y-%m-%d %H:%M"
                                    (encode-time (iso8601-parse .updatedAt)))
              "")
            "  "
            (or .title .sessionId))))

(defun agentel-resume--choose (sessions cwd)
  "Let the user choose one of SESSIONS and open it in CWD."
  (let* ((candidates (mapcar (lambda (s) (cons (agentel-resume--describe s) s))
                             sessions))
         (choice (completing-read "Resume session: "
                                  (agentel-chat-ordered-completion
                                   (mapcar #'car candidates))
                                  nil t))
         (session (cdr (assoc choice candidates))))
    (let ((resumed (agentel-start :cwd (or (alist-get 'cwd session) cwd)
                                  :session-id (alist-get 'sessionId session))))
      (setf (agentel-session-title resumed) (alist-get 'title session))
      (agentel-session-changed resumed)
      resumed)))

(defun agentel-resume (session _args)
  "Choose an earlier session in the directory of SESSION and open it."
  (let ((cwd (agentel-session-cwd session)))
    (agentel-connection-request
     (agentel-session-connection session) "session/list" `((cwd . ,cwd))
     :on-success
     (lambda (result)
       (let ((sessions (seq-remove
                        (lambda (s) (agentel-session-get (alist-get 'sessionId s)))
                        (alist-get 'sessions result))))
         (if sessions
             ;; Leave the process filter before reading from the minibuffer.
             (run-at-time 0 nil #'agentel-resume--choose sessions cwd)
           (message "No other session to resume in %s" cwd))))
     :on-failure
     (lambda (error)
       (message "Could not list sessions: %s" (alist-get 'message error))))))

(agentel-commands-define "resume" "Resume an earlier session of this directory"
                         #'agentel-resume)

(provide 'agentel-resume)
;;; agentel-resume.el ends here
