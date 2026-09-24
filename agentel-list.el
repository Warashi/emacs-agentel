;;; agentel-list.el --- List of running sessions for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; `agentel-list' shows every running session with its subagents below
;; it, what it is doing and whether it waits for the user, like the
;; session sidebar of GUI agent apps.  The details column reuses the
;; header line segments of the session buffers, so it shows whatever
;; the loaded features show there without depending on them.

;;; Code:

(require 'tabulated-list)
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

(defvar agentel-list--refresh-timer nil
  "Timer that refreshes the list after sessions changed.")

(defun agentel-list--state (session)
  "Return the state column of SESSION."
  (let ((state (agentel-session-state session)))
    (propertize (symbol-name state)
                'face (pcase state
                        ('waiting 'agentel-list-waiting-face)
                        ((or 'running 'starting) 'agentel-list-running-face)
                        ('idle 'default)
                        (_ 'agentel-list-ended-face)))))

(defun agentel-list--details (session)
  "Return the details column of SESSION."
  (string-join (delq nil (mapcar (lambda (f) (funcall f session))
                                 agentel-chat-header-functions))
               "  "))

(defun agentel-list--rows (session depth)
  "Return the rows of SESSION and its subagents indented by DEPTH."
  (cons (list session
              (vector (agentel-list--state session)
                      (concat (make-string (* 2 depth) ?\s)
                              (if (> depth 0) "└ " "")
                              (agentel-session-name session))
                      (agentel-list--details session)
                      (abbreviate-file-name (or (agentel-session-cwd session) ""))))
        (mapcan (lambda (child) (agentel-list--rows child (1+ depth)))
                (agentel-session-children session))))

(defun agentel-list--entries ()
  "Return the rows of all sessions."
  (mapcan (lambda (session) (agentel-list--rows session 0))
          (agentel-session-roots)))

(defun agentel-list--refresh ()
  "Redraw the session list if it is open."
  (setq agentel-list--refresh-timer nil)
  (when-let* ((buffer (get-buffer agentel-list-buffer-name)))
    (with-current-buffer buffer
      (tabulated-list-print t))))

(defun agentel-list--schedule-refresh (&rest _)
  "Redraw the session list soon, once for many changes."
  (when (and (get-buffer agentel-list-buffer-name)
             (not agentel-list--refresh-timer))
    (setq agentel-list--refresh-timer
          (run-with-idle-timer 0.1 nil #'agentel-list--refresh))))

(defun agentel-list--session ()
  "Return the session of the row at point."
  (or (tabulated-list-get-id) (user-error "No session on this line")))

(defun agentel-list-visit ()
  "Show the buffer of the session at point."
  (interactive)
  (pop-to-buffer (agentel-session-buffer (agentel-list--session))))

(defun agentel-list-answer ()
  "Answer the oldest question of the session at point."
  (interactive)
  (with-current-buffer (agentel-session-buffer (agentel-list--session))
    (agentel-chat-answer)))

(defun agentel-list-cancel ()
  "Stop the current turn of the session at point."
  (interactive)
  (with-current-buffer (agentel-session-buffer (agentel-list--session))
    (agentel-chat-cancel)))

(defun agentel-list-kill ()
  "Stop the session at point and kill its buffer."
  (interactive)
  (let ((session (agentel-list--session)))
    (when (yes-or-no-p (format "Stop %s? " (agentel-session-name session)))
      (kill-buffer (agentel-session-buffer session))
      (agentel-list--refresh))))

(defvar-keymap agentel-list-mode-map
  :doc "Keymap of `agentel-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'agentel-list-visit
  "a" #'agentel-list-answer
  "c" #'agentel-list-cancel
  "k" #'agentel-list-kill)

(define-derived-mode agentel-list-mode tabulated-list-mode "agentel sessions"
  "Major mode listing agentel sessions."
  (setq tabulated-list-format [("State" 9 nil)
                               ("Session" 36 nil)
                               ("Details" 64 nil)
                               ("Directory" 0 nil)])
  (setq tabulated-list-entries #'agentel-list--entries)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun agentel-list-noselect ()
  "Return the session list buffer, creating it if needed."
  (let ((buffer (get-buffer-create agentel-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agentel-list-mode)
        (agentel-list-mode))
      (tabulated-list-print t))
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
