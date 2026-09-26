;;; agentel-list.el --- List of running sessions for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-list' shows every running session with its subagents below
;; it and whether it works or waits for the user, like the session
;; sidebar of GUI agent apps.  A session takes two lines, its project
;; with its state and then its title, so the list stays readable in a
;; narrow window.

;;; Code:

(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)

(defconst agentel-list-buffer-name "*agentel sessions*"
  "Name of the session list buffer.")

(defface agentel-list-waiting-face
  '((t :inherit warning :weight bold))
  "Face of sessions waiting for the user."
  :group 'agentel)

(defface agentel-list-running-face
  '((t :inherit success))
  "Face of sessions working on a turn."
  :group 'agentel)

(defface agentel-list-ended-face
  '((t :inherit shadow))
  "Face of sessions that ended."
  :group 'agentel)

(defface agentel-list-title-face
  '((t :inherit shadow))
  "Face of the title line of a session."
  :group 'agentel)

(defvar agentel-list--refresh-timer nil
  "Timer that refreshes the list after sessions changed.")

(defun agentel-list--state (session)
  "Return the state of SESSION in brackets."
  (let ((state (agentel-session-state session)))
    (propertize (format "[%s]" state)
                'face (pcase state
                        ('waiting 'agentel-list-waiting-face)
                        ((or 'running 'starting) 'agentel-list-running-face)
                        ('idle 'default)
                        (_ 'agentel-list-ended-face)))))

(defun agentel-list--insert-session (session depth)
  "Insert the lines of SESSION and its subagents indented by DEPTH.
A top-level session shows its project and then its title; a subagent
has no project of its own and shows only its title."
  (let ((start (point)))
    (if (> depth 0)
        (insert (make-string (* 2 depth) ?\s) "└ "
                (agentel-session-name session) " "
                (agentel-list--state session) "\n")
      (insert (or (agentel-session-project session)
                  (agentel-session-name session))
              " " (agentel-list--state session) "\n")
      (when (and (agentel-session-project session)
                 (agentel-session-title session))
        (insert "  " (propertize (agentel-session-name session)
                                 'face 'agentel-list-title-face)
                "\n")))
    (put-text-property start (point) 'agentel-list-session session))
  (dolist (child (agentel-session-children session))
    (agentel-list--insert-session child (1+ depth))))

(defun agentel-list--position (pos)
  "Return where POS is as (SESSION LINE COLUMN).
LINE counts the lines from the first one of SESSION."
  (save-excursion
    (goto-char pos)
    (let ((session (get-text-property (point) 'agentel-list-session)))
      (list session
            (if session
                (count-lines (text-property-any (point-min) (point-max)
                                                'agentel-list-session session)
                             (line-beginning-position))
              0)
            (current-column)))))

(defun agentel-list--goto (position)
  "Move point to POSITION returned by `agentel-list--position'."
  (pcase-let ((`(,session ,line ,column) position))
    (goto-char (or (and session
                        (text-property-any (point-min) (point-max)
                                           'agentel-list-session session))
                   (point-min)))
    (when (and session (zerop (forward-line line)))
      (unless (eq (get-text-property (point) 'agentel-list-session) session)
        (forward-line -1)))
    (move-to-column column)
    (point)))

(defun agentel-list--render ()
  "Redraw the list in the current buffer.
Point and the point of every window showing the list stay on the line
of the same session, since the lines are drawn again from scratch."
  (let ((point (agentel-list--position (point)))
        (windows (mapcar (lambda (w) (cons w (agentel-list--position (window-point w))))
                         (get-buffer-window-list nil nil t)))
        (inhibit-read-only t))
    (erase-buffer)
    (dolist (root (agentel-session-roots))
      (agentel-list--insert-session root 0))
    (pcase-dolist (`(,window . ,position) windows)
      (set-window-point window (agentel-list--goto position)))
    (agentel-list--goto point)))

(defun agentel-list--refresh ()
  "Redraw the session list if it is open."
  (setq agentel-list--refresh-timer nil)
  (when-let* ((buffer (get-buffer agentel-list-buffer-name)))
    (with-current-buffer buffer
      (agentel-list--render))))

(defun agentel-list--schedule-refresh (&rest _)
  "Redraw the session list soon, once for many changes."
  (when (and (get-buffer agentel-list-buffer-name)
             (not agentel-list--refresh-timer))
    (setq agentel-list--refresh-timer
          (run-with-idle-timer 0.1 nil #'agentel-list--refresh))))

(defun agentel-list--session ()
  "Return the session of the row at point."
  (or (get-text-property (point) 'agentel-list-session)
      (user-error "No session on this line")))

(defun agentel-list-visit ()
  "Show the buffer of the session at point."
  (interactive)
  (pop-to-buffer (agentel-chat-buffer (agentel-list--session))))

(defun agentel-list-answer ()
  "Answer the oldest question of the session at point."
  (interactive)
  (with-current-buffer (agentel-chat-buffer (agentel-list--session))
    (agentel-chat-answer)))

(defun agentel-list-cancel ()
  "Stop the current turn of the session at point."
  (interactive)
  (with-current-buffer (agentel-chat-buffer (agentel-list--session))
    (agentel-chat-cancel)))

(defun agentel-list-kill ()
  "Stop the session at point and kill its buffer."
  (interactive)
  (let ((session (agentel-list--session)))
    (when (agentel-session-parent session)
      (user-error "A subagent stops with the session that started it"))
    (when (yes-or-no-p (format "Stop %s? " (agentel-session-name session)))
      (kill-buffer (agentel-session-buffer session))
      (agentel-list--refresh))))

(defvar-keymap agentel-list-mode-map
  :doc "Keymap of `agentel-list-mode'."
  :parent special-mode-map
  "RET" #'agentel-list-visit
  "a" #'agentel-list-answer
  "c" #'agentel-list-cancel
  "k" #'agentel-list-kill)

(define-derived-mode agentel-list-mode special-mode "agentel sessions"
  "Major mode listing agentel sessions."
  (setq truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (agentel-list--render))))

(defun agentel-list-noselect ()
  "Return the session list buffer, creating it if needed."
  (let ((buffer (get-buffer-create agentel-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agentel-list-mode)
        (agentel-list-mode))
      (agentel-list--render))
    buffer))

;;;###autoload
(defun agentel-list ()
  "Show the running agent sessions."
  (interactive)
  (pop-to-buffer (agentel-list-noselect)))

(keymap-set agentel-chat-mode-map "C-c C-l" #'agentel-list)
(add-hook 'agentel-session-changed-functions #'agentel-list--schedule-refresh)

(provide 'agentel-list)
;;; agentel-list.el ends here
