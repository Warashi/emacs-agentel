;;; agentel-notify.el --- Notify the user when a session needs them  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; When a session enters a state of `agentel-notify-states', the user
;; is notified through the terminals Emacs shows frames on.  A text
;; terminal gets an OSC 777 notification, which the terminal emulator
;; turns into one of the desktop, so a notification reaches the desktop
;; from an Emacs over SSH too.  Graphic frames of macOS get one of macOS
;; through osascript.  Subagents are left to their root
;; session, which waits while they do.

;;; Code:

(require 'agentel-session)

(defcustom agentel-notify-states '(waiting idle failed exited)
  "States of `agentel-session-state' a session notifies the user in."
  :type '(set (const waiting) (const idle) (const failed) (const exited))
  :group 'agentel)

(defvar agentel-notify-function #'agentel-notify-send
  "Function called with a title and a body to notify the user.")

(defconst agentel-notify--descriptions
  '((waiting . "Waiting for you")
    (idle . "Ready")
    (failed . "Failed")
    (exited . "Exited"))
  "Body of the notification of each state.")

;; Kept apart from `agentel-session-data', whose setter tells listeners
;; of a change and so would run the hook this is set from again.
(defvar agentel-notify--states (make-hash-table :test 'eq :weakness 'key)
  "Last state seen of each root session.")

(defun agentel-notify--on-changed (session)
  "Notify the user when SESSION enters a state of `agentel-notify-states'."
  (cond ((agentel-session-parent session))
        ((not (memq session (agentel-session-list)))
         (remhash session agentel-notify--states))
        (t
         (let ((state (agentel-session-state session)))
           (unless (eq state (gethash session agentel-notify--states))
             (puthash session state agentel-notify--states)
             (when (memq state agentel-notify-states)
               (with-demoted-errors "agentel-notify: %S"
                 (funcall agentel-notify-function
                          (agentel-session-name session)
                          (or (alist-get state agentel-notify--descriptions)
                              (symbol-name state))))))))))

(defun agentel-notify--clean (text)
  "Return TEXT with each control character made a space."
  (replace-regexp-in-string "[[:cntrl:]]" " " text))

(defun agentel-notify--osc (title body)
  "Return the OSC 777 sequence that notifies with TITLE and BODY.
The title ends at the first semicolon, so its own are made spaces."
  (format "\e]777;notify;%s;%s\e\\"
          (string-replace ";" " " (agentel-notify--clean title))
          (agentel-notify--clean body)))

(defun agentel-notify--macos (title body)
  "Show a notification of macOS with TITLE and BODY."
  ;; Passed as arguments, TITLE and BODY need no quoting in AppleScript.
  (start-process "agentel-notify" nil "osascript"
                 "-e" "on run argv"
                 "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
                 "-e" "end run"
                 title body))

(defun agentel-notify-send (title body)
  "Notify the user with TITLE and BODY on each terminal showing frames."
  (let (macos)
    (dolist (terminal (terminal-list))
      (unless (frame-initial-p terminal)
        (pcase (terminal-live-p terminal)
          ('t (send-string-to-terminal (agentel-notify--osc title body) terminal))
          ('ns (setq macos t)))))
    (when macos
      (agentel-notify--macos title body))))

(add-hook 'agentel-session-changed-functions #'agentel-notify--on-changed)

(provide 'agentel-notify)
;;; agentel-notify.el ends here
