;;; agentel-list.el --- List of running sessions for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-list' shows every running session with its subagents below
;; it and whether it works or waits for the user, like the session
;; sidebar of GUI agent apps.  A session takes two lines, its state
;; with its project and then its title, so the list stays readable in a
;; narrow window.  Sessions waiting for the user come first, then the
;; unread ones, marked with a dot: those whose turn ended while their
;; buffer was out of sight, until it is shown again.  The list stays
;; open in a side window until closed with the same command, and
;; `other-window' passes over it.
;;
;; The list is one model of `agentel-ui'.  Hooks send it what they see
;; of a session whenever the session changes, so its view depends on
;; nothing else, and the buffer draws the view again soon after the
;; model changes.

;;; Code:

(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)
(require 'agentel-ui)

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

(defun agentel-list--shown-p (session)
  "Return non-nil when the buffer of SESSION is in a visible window."
  (when-let* ((buffer (agentel-session-buffer session)))
    (get-buffer-window buffer 'visible)))

;;;; Model

(defun agentel-list--see (sessions seen)
  "Return SESSIONS with SEEN in place of what was seen of its session.
A session no longer live is left out, and a new one goes last."
  (let ((session (plist-get seen :session)))
    (cond ((not (plist-get seen :live))
           (seq-remove (lambda (entry) (eq (car entry) session)) sessions))
          ((assq session sessions)
           (mapcar (lambda (entry)
                     (if (eq (car entry) session) (cons session seen) entry))
                   sessions))
          (t (append sessions (list (cons session seen)))))))

(defun agentel-list--ended-out-of-sight-p (before seen)
  "Return non-nil if the turn of a top-level session ended out of sight.
BEFORE and SEEN are what was seen of it before and now."
  (and (plist-get seen :live)
       (plist-get before :busy)
       (not (plist-get seen :busy))
       (not (plist-get seen :parent))
       (not (plist-get seen :shown))))

(defun agentel-list--update (message data)
  "Return the DATA of the list changed by MESSAGE.
DATA has the sessions, each with what was seen of it last, oldest
first, and the unread sessions: top-level ones whose turn ended while
their buffer was out of sight, until they are shown again."
  (let-alist data
    (pcase message
      (`(changed ,seen)
       (let ((session (plist-get seen :session)))
         `((sessions . ,(agentel-list--see .sessions seen))
           (unread . ,(cond ((not (plist-get seen :live)) (remq session .unread))
                            ((and (agentel-list--ended-out-of-sight-p
                                   (alist-get session .sessions) seen)
                                  (not (memq session .unread)))
                             (cons session .unread))
                            (t .unread))))))
      (`(shown ,sessions)
       `((sessions . ,.sessions)
         (unread . ,(seq-remove (lambda (session) (memq session sessions))
                                .unread)))))))

(defun agentel-list--session-lines (session seen sessions unread depth)
  "Return the lines of SESSION and its subagents indented by DEPTH.
SEEN is what was seen of SESSION, SESSIONS and UNREAD are those of the
data of the list.  Each session leads with its state, which is short
and would be cut off at the end of a long line in a narrow window.  A
top-level session shows its project, or its directory outside of one,
and then its title; a subagent has no project of its own and shows
only its title."
  (concat
   (propertize
    (concat (if (memq session unread)
                (propertize "●" 'face 'agentel-list-unread-face)
              " ")
            " "
            (if (> depth 0)
                (concat (make-string (* 2 depth) ?\s) "└ "
                        (agentel-ui-state (plist-get seen :state)) " "
                        (plist-get seen :name) "\n")
              (concat (agentel-ui-state (plist-get seen :state)) " "
                      (plist-get seen :project) "\n"
                      (if (plist-get seen :titled)
                          (concat "    " (propertize (plist-get seen :name)
                                                     'face 'agentel-list-title-face)
                                  "\n")
                        ""))))
    'agentel-list-session session)
   (mapconcat (pcase-lambda (`(,child . ,seen))
                (agentel-list--session-lines child seen sessions unread (1+ depth)))
              (seq-filter (lambda (entry) (eq (plist-get (cdr entry) :parent) session))
                          sessions)
              "")))

(defun agentel-list--rank (session seen unread)
  "Return where SESSION goes in the list, lower first.
SEEN is what was seen of it and UNREAD the unread sessions."
  (cond ((eq (plist-get seen :state) 'waiting) 0)
        ((memq session unread) 1)
        (t 2)))

(defun agentel-list--view (model _options)
  "Return the text of the list MODEL.
Sessions waiting for the user come first, then the unread ones,
oldest first within a rank."
  (let-alist (agentel-ui-model-data model)
    (mapconcat (pcase-lambda (`(,session . ,seen))
                 (agentel-list--session-lines session seen .sessions .unread 0))
               (seq-sort-by (pcase-lambda (`(,session . ,seen))
                              (agentel-list--rank session seen .unread))
                            #'<
                            (seq-remove (lambda (entry) (plist-get (cdr entry) :parent))
                                        .sessions))
               "")))

(agentel-ui-define 'agentel-list
  :update #'agentel-list--update
  :view #'agentel-list--view)

(defvar agentel-list--store (agentel-ui-store-create)
  "Store of the list, whose one model is under the key `sessions'.")

(defun agentel-list--data (key)
  "Return the value the data of the list has under KEY."
  (when-let* ((model (agentel-ui-find agentel-list--store 'sessions)))
    (agentel-ui-get model key)))

(defun agentel-list--send (message)
  "Send MESSAGE to the model of the list."
  (agentel-ui-dispatch agentel-list--store 'sessions 'agentel-list message))

(defun agentel-list--on-changed (session)
  "Tell the list what is seen of SESSION now that it changed."
  (agentel-list--send
   `(changed (:session ,session
                       :live ,(and (memq session (agentel-session-list)) t)
                       :parent ,(agentel-session-parent session)
                       :busy ,(agentel-session-busy session)
                       :shown ,(and (agentel-list--shown-p session) t)
                       :state ,(agentel-session-state session)
                       :project ,(agentel-session-project-name session)
                       :name ,(agentel-session-name session)
                       :titled ,(and (agentel-session-title session) t)))))

(defun agentel-list--forget-shown (_frame)
  "Tell the list which of the unread sessions are now shown."
  (when-let* ((seen (seq-filter #'agentel-list--shown-p
                                (agentel-list--data 'unread))))
    (agentel-list--send `(shown ,seen))))

;;;; Buffer

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
    (when-let* ((model (agentel-ui-find agentel-list--store 'sessions)))
      (insert (agentel-ui-view model)))
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
  (let* ((store agentel-list--store)
         (unsubscribe (lambda ()
                        (agentel-ui-unsubscribe store #'agentel-list--schedule-refresh))))
    (funcall unsubscribe)
    (agentel-ui-subscribe store #'agentel-list--schedule-refresh)
    (add-hook 'change-major-mode-hook unsubscribe nil t)
    (add-hook 'kill-buffer-hook unsubscribe nil t))
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
(add-hook 'agentel-session-changed-functions #'agentel-list--on-changed)
(add-hook 'window-buffer-change-functions #'agentel-list--forget-shown)

(provide 'agentel-list)
;;; agentel-list.el ends here
