;;; agentel-commands.el --- Slash commands for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Completes slash commands at the start of the input.  The candidates
;; are the commands the agent announced with `available_commands_update'
;; and the commands agentel runs itself, which features add with
;; `agentel-commands-define'.  Agent commands are sent as a prompt like
;; any other text; agentel's own commands run in Emacs instead.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-chat)

(defvar agentel-commands-client-commands nil
  "Commands run by agentel, as (NAME DESCRIPTION FUNCTION).
FUNCTION is called with the session and the argument text.")

(defun agentel-commands-define (name description function)
  "Define the client command /NAME described by DESCRIPTION.
FUNCTION is called with the session and the text after the name."
  (setf (alist-get name agentel-commands-client-commands nil nil #'equal)
        (list description function)))

(defun agentel-commands--on-update (session update)
  "Remember the commands the agent announced in UPDATE for SESSION."
  (when (equal (alist-get 'sessionUpdate update) "available_commands_update")
    (setf (agentel-session-data session 'commands)
          (append (alist-get 'availableCommands update) nil))))

(defun agentel-commands--annotation (session name)
  "Return the annotation of the command NAME in SESSION."
  (if-let* ((client (assoc name agentel-commands-client-commands)))
      (concat "  " (nth 1 client))
    (when-let* ((command (seq-find (lambda (c) (equal (alist-get 'name c) name))
                                   (agentel-session-data session 'commands))))
      (concat (if-let* ((hint (alist-get 'hint (alist-get 'input command))))
                  (concat " " hint)
                "")
              "  "
              (truncate-string-to-width
               (or (alist-get 'description command) "") 70 nil nil "…")))))

(defun agentel-commands-completion-at-point ()
  "Complete a slash command at the start of the input."
  (when-let* ((session agentel-chat--session)
              (start agentel-chat--input-start)
              ((>= (point) start))
              ((save-excursion
                 (goto-char start)
                 (looking-at "/\\([^ \t\n]*\\)")))
              ((<= (point) (match-end 0))))
    (let ((names (append (mapcar #'car agentel-commands-client-commands)
                         (mapcar (lambda (c) (alist-get 'name c))
                                 (agentel-session-data session 'commands)))))
      (list (match-beginning 1) (match-end 1) names
            :exclusive 'no
            :annotation-function
            (lambda (name) (agentel-commands--annotation session name))))))

(defun agentel-commands--run (session text)
  "Run TEXT in SESSION if it invokes a client command.
Return non-nil when it did."
  (when (string-match "\\`/\\([^ \t\n]+\\)\\(?:[ \t\n]+\\(.*\\)\\)?\\'" text)
    (when-let* ((command (assoc (match-string 1 text)
                                agentel-commands-client-commands)))
      (funcall (nth 2 command) session (string-trim (or (match-string 2 text) "")))
      t)))

(defun agentel-commands--setup ()
  "Enable command completion in the current chat buffer."
  (add-hook 'completion-at-point-functions
            #'agentel-commands-completion-at-point nil t))

(add-hook 'agentel-session-update-functions #'agentel-commands--on-update)
(add-hook 'agentel-chat-send-functions #'agentel-commands--run)
(add-hook 'agentel-chat-mode-hook #'agentel-commands--setup)

(provide 'agentel-commands)
;;; agentel-commands.el ends here
