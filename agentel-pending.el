;;; agentel-pending.el --- Move to the next session that needs the user  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-pending-next' shows the next session that needs the user,
;; from any buffer, so the sessions are answered one after another
;; without going through the session list.  The sessions and their
;; order are those the list puts first: waiting for an answer, then
;; unread.

;;; Code:

(require 'agentel-session)
(require 'agentel-chat)
(require 'agentel-list)

;;;###autoload
(defun agentel-pending-next ()
  "Show the next session that needs the user in the selected window.
It is the one after the session of the current buffer in the order of
`agentel-list-pending', or the first one from elsewhere."
  (interactive)
  (let* ((pending (agentel-list-pending))
         (current (agentel-session-current))
         (next (or (cadr (memq current pending))
                   (car pending))))
    (if (or (not next) (eq next current))
        (message "No other session needs you")
      (pop-to-buffer-same-window (agentel-chat-buffer next)))))

(keymap-set agentel-chat-mode-map "C-c C-n" #'agentel-pending-next)

(provide 'agentel-pending)
;;; agentel-pending.el ends here
