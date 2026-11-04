;;; agentel-sleep.el --- Keep macOS awake while a session runs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; While any session is running, the idle sleep of the system is
;; blocked through `system-sleep', letting the display sleep.  A
;; session waiting for the user does not block it.  Only the NS build
;; on macOS is supported.

;;; Code:

(require 'agentel-session)

;; logind refuses the block over D-Bus to an Emacs without a session of
;; its own, so other back ends of `system-sleep' are left alone.  Loading
;; `system-sleep' starts its back end, so it is not even loaded there.
(defvar agentel-sleep--supported (featurep 'ns)
  "Non-nil if the idle sleep of the system is blocked.")

(when agentel-sleep--supported
  (require 'system-sleep))

(declare-function system-sleep-block-sleep "system-sleep")
(declare-function system-sleep-unblock-sleep "system-sleep")

(defvar agentel-sleep--token nil
  "Token of the sleep block held, or nil.")

(defun agentel-sleep--on-changed (_session)
  "Block idle sleep while a session runs and release it otherwise."
  (when agentel-sleep--supported
    (let ((running (seq-some (lambda (s) (eq (agentel-session-state s) 'running))
                             (agentel-session-list))))
      (cond ((and running (not agentel-sleep--token))
             (setq agentel-sleep--token
                   (system-sleep-block-sleep "agentel (agent running)" t)))
            ((and (not running) agentel-sleep--token)
             (system-sleep-unblock-sleep agentel-sleep--token)
             (setq agentel-sleep--token nil))))))

(add-hook 'agentel-session-changed-functions #'agentel-sleep--on-changed)

(provide 'agentel-sleep)
;;; agentel-sleep.el ends here
