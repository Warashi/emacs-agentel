;;; agentel-list.el --- List of running sessions for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-list' shows every running session with its subagents below
;; it and whether it works or waits for the user, like the session
;; sidebar of GUI agent apps.  A session takes two lines, its project
;; with its state and then its title, so the list stays readable in a
;; narrow window.  Sessions waiting for the user come first, then the
;; unread ones, marked with a dot: those whose turn ended while their
;; buffer was out of sight, until it is shown again.  The list stays
;; open in a side window until closed with the same command, and
;; `other-window' passes over it.

;;; Code:

(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)

(defconst agentel-list-buffer-name "*agentel sessions*"
  "Name of the session list buffer.")

(defcustom agentel-list-side 'left
  "Side of the frame where `agentel-list' shows the list."
  :type '(choice (const left) (const right))
  :group 'agentel)

(defcustom agentel-list-width 40
  "Width of the window of `agentel-list'."
  :type 'natnum
  :group 'agentel)

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

(defface agentel-list-unread-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face of the mark of unread sessions."
  :group 'agentel)

(defface agentel-list-title-face
  '((t :inherit shadow))
  "Face of the title line of a session."
  :group 'agentel)

(defvar agentel-list--refresh-timer nil
  "Timer that refreshes the list after sessions changed.")

(defvar agentel-list--busy (make-hash-table :test 'eq :weakness 'key)
  "Whether each session was working on a turn when it last changed.")

(defvar agentel-list--unread (make-hash-table :test 'eq :weakness 'key)
  "Sessions whose turn ended while their buffer was out of sight.")

(defun agentel-list--shown-p (session)
  "Return non-nil when the buffer of SESSION is in a visible window."
  (when-let* ((buffer (agentel-session-buffer session)))
    (get-buffer-window buffer 'visible)))

(defun agentel-list--track-unread (session)
  "Mark SESSION unread when its turn ended out of sight.
The previous busy value is kept here rather than with
`agentel-session-data', whose setter would run this hook again."
  (let ((busy (agentel-session-busy session)))
    (when (and (gethash session agentel-list--busy)
               (not busy)
               (not (agentel-session-parent session))
               (not (agentel-list--shown-p session)))
      (puthash session t agentel-list--unread))
    (puthash session busy agentel-list--busy)))

(defun agentel-list--forget-shown (_frame)
  "Unmark the unread sessions whose buffer is now shown."
  (let (seen)
    (maphash (lambda (session _)
               (when (agentel-list--shown-p session)
                 (push session seen)))
             agentel-list--unread)
    (when seen
      (dolist (session seen)
        (remhash session agentel-list--unread))
      (agentel-list--schedule-refresh))))

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
A top-level session shows its project, or its directory outside of
one, and then its title; a subagent
has no project of its own and shows only its title."
  (let ((start (point)))
    (insert (if (gethash session agentel-list--unread)
                (propertize "●" 'face 'agentel-list-unread-face)
              " ")
            " ")
    (if (> depth 0)
        (insert (make-string (* 2 depth) ?\s) "└ "
                (agentel-session-name session) " "
                (agentel-list--state session) "\n")
      (insert (agentel-session-project-name session) " "
              (agentel-list--state session) "\n")
      (when (agentel-session-title session)
        (insert "    " (propertize (agentel-session-name session)
                                   'face 'agentel-list-title-face)
                "\n")))
    (put-text-property start (point) 'agentel-list-session session))
  (dolist (child (agentel-session-children session))
    (agentel-list--insert-session child (1+ depth))))

(defun agentel-list--rank (session)
  "Return where SESSION goes in the list, lower first."
  (cond ((eq (agentel-session-state session) 'waiting) 0)
        ((gethash session agentel-list--unread) 1)
        (t 2)))

(defun agentel-list--roots ()
  "Return the top-level sessions in the order the list shows them.
Sessions waiting for the user come first, then the unread ones,
oldest first within a rank."
  (seq-sort-by #'agentel-list--rank #'< (agentel-session-roots)))

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
    (dolist (root (agentel-list--roots))
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

(defun agentel-list-next (&optional n)
  "Move to the first line of the Nth next session.
With a negative N, move to the previous ones.  Point stays at the
ends of the list."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (agentel-list-previous (- n))
    (dotimes (_ n)
      (let ((next (next-single-property-change (point) 'agentel-list-session)))
        (when (and next (get-text-property next 'agentel-list-session))
          (goto-char next))))))

(defun agentel-list-previous (&optional n)
  "Move to the first line of the Nth previous session.
With a negative N, move to the next ones.  Point stays at the ends
of the list."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (agentel-list-next (- n))
    (dotimes (_ n)
      (let ((start (previous-single-property-change
                    (min (1+ (point)) (point-max)) 'agentel-list-session
                    nil (point-min))))
        (goto-char (if (> start (point-min))
                       (previous-single-property-change
                        start 'agentel-list-session nil (point-min))
                     start))))))

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
  "n" #'agentel-list-next
  "p" #'agentel-list-previous
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
  "Show the running agent sessions in a side window and move to it.
When point is already in the list, close it instead.  The window
stays through `delete-other-windows' and `other-window' skips it, so
this command is the way in and out of it."
  (interactive)
  (let ((window (get-buffer-window agentel-list-buffer-name)))
    (cond ((and window (eq window (selected-window)))
           (delete-window window))
          (window (select-window window))
          (t (select-window
              (display-buffer-in-side-window
               (agentel-list-noselect)
               `((side . ,agentel-list-side)
                 (window-width . ,agentel-list-width)
                 (window-parameters (no-other-window . t)
                                    (no-delete-other-windows . t)))))))))

(keymap-set agentel-chat-mode-map "C-c C-l" #'agentel-list)
(add-hook 'agentel-session-changed-functions #'agentel-list--track-unread)
(add-hook 'agentel-session-changed-functions #'agentel-list--schedule-refresh)
(add-hook 'window-buffer-change-functions #'agentel-list--forget-shown)

(provide 'agentel-list)
;;; agentel-list.el ends here
