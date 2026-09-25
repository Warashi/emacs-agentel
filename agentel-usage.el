;;; agentel-usage.el --- Context and cost display for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Follows `usage_update' and shows how full the context window is and
;; what the session has cost so far.  claude-agent-acp reports the cost
;; only at the end of a turn and the context size also in between, so a
;; report without cost keeps the last known cost.

;;; Code:

(require 'agentel-session)
(require 'agentel-chat)

(defcustom agentel-usage-warning-ratio 0.8
  "Share of the context window above which the usage is highlighted."
  :type 'number
  :group 'agentel)

(defface agentel-usage-full-face
  '((t :inherit warning))
  "Face of the context usage when the window is almost full."
  :group 'agentel)

(defun agentel-usage--on-update (session update)
  "Record the usage reported in UPDATE for SESSION."
  (when (equal (alist-get 'sessionUpdate update) "usage_update")
    (let ((previous (agentel-session-data session 'usage)))
      (setf (agentel-session-data session 'usage)
            (list :used (alist-get 'used update)
                  :size (alist-get 'size update)
                  :cost (or (alist-get 'cost update)
                            (plist-get previous :cost)))))))

(defun agentel-usage--tokens (count)
  "Return the token COUNT in a short form."
  (cond ((>= count 1000000) (agentel-usage--trim (/ count 1000000.0) "M"))
        ((>= count 1000) (agentel-usage--trim (/ count 1000.0) "k"))
        (t (number-to-string count))))

(defun agentel-usage--trim (number unit)
  "Return NUMBER with at most one decimal followed by UNIT."
  (concat (replace-regexp-in-string "\\.0\\'" "" (format "%.1f" number)) unit))

(defun agentel-usage--cost (cost)
  "Return the COST alist as text."
  (let ((amount (alist-get 'amount cost))
        (currency (alist-get 'currency cost)))
    (if (equal currency "USD")
        (format "$%.2f" amount)
      (format "%.2f %s" amount currency))))

(defun agentel-usage-summary (session)
  "Return the context usage and cost of SESSION as text, or nil."
  (when-let* ((usage (agentel-session-data session 'usage)))
    (let* ((used (or (plist-get usage :used) 0))
           (size (plist-get usage :size))
           (ratio (if (and size (> size 0)) (/ (float used) size) 0))
           (context (format "ctx %d%% (%s/%s)" (round (* 100 ratio))
                            (agentel-usage--tokens used)
                            (if size (agentel-usage--tokens size) "?")))
           (cost (plist-get usage :cost)))
      (concat (if (>= ratio agentel-usage-warning-ratio)
                  (propertize context 'face 'agentel-usage-full-face)
                context)
              (if cost (concat " · " (agentel-usage--cost cost)) "")))))

(add-hook 'agentel-session-update-functions #'agentel-usage--on-update)
(add-hook 'agentel-chat-header-functions #'agentel-usage-summary 90)

(provide 'agentel-usage)
;;; agentel-usage.el ends here
