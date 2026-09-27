;;; agentel-title.el --- Titles the agent gives sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The agent names a session once it knows what the session is about.
;; This file gives the session the title the agent tells.

;;; Code:

(require 'agentel-session)

(defun agentel-title--on-update (session update)
  "Retitle SESSION as UPDATE tells."
  (when (equal (alist-get 'sessionUpdate update) "session_info_update")
    (when-let* ((title (alist-get 'title update)))
      (agentel-session-send session `(retitle ,title)))))

(add-hook 'agentel-session-update-functions #'agentel-title--on-update)

(provide 'agentel-title)
;;; agentel-title.el ends here
