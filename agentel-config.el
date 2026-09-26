;;; agentel-config.el --- Model, effort and mode for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Keeps the session config options the agent reports (model, effort,
;; mode, ...), shows them in the header line and changes them with
;; `session/set_config_option'.
;;
;; `agentel-start' takes :model, :effort and :mode.  They are applied
;; in that order after the session starts, because the model decides
;; which effort levels and modes exist: claude-agent-acp drops the effort
;; option for Haiku and replaces the auto mode on models without it.
;; A session started again in place of another gets its current values
;; through the same options.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)

(defvar agentel-session-started-functions)
(defvar agentel-session-restart-options-functions)

(defconst agentel-config--start-options
  '((:model . "model") (:effort . "effort") (:mode . "mode"))
  "Start options of `agentel-start' and the config option each sets.")

(defconst agentel-config--header-order '("model" "effort" "mode")
  "Config option ids shown first in the header line, in this order.")

(defun agentel-config-option (session id)
  "Return the config option ID of SESSION."
  (seq-find (lambda (o) (equal (alist-get 'id o) id))
            (agentel-session-data session 'config-options)))

(defun agentel-config--store (session options)
  "Record the config OPTIONS the agent reported for SESSION."
  (when options
    (setf (agentel-session-data session 'config-options) (append options nil))))

(defun agentel-config--value-name (option)
  "Return the display name of the current value of OPTION."
  (let ((value (alist-get 'currentValue option)))
    (or (alist-get 'name (seq-find (lambda (v) (equal (alist-get 'value v) value))
                                   (alist-get 'options option)))
        (format "%s" value))))

(defun agentel-config--header (session)
  "Return the header line segment of SESSION's settings."
  (when-let* ((options (agentel-session-data session 'config-options)))
    (string-join
     (mapcar (lambda (option)
               (if (member (alist-get 'id option) agentel-config--header-order)
                   (agentel-config--value-name option)
                 (format "%s: %s" (alist-get 'name option)
                         (agentel-config--value-name option))))
             (seq-sort-by (lambda (o)
                            (or (seq-position agentel-config--header-order
                                              (alist-get 'id o))
                                (length agentel-config--header-order)))
                          #'< options))
     " · ")))

;;;; Changing options

(defun agentel-config-set (session id value &optional callback)
  "Set the config option ID of SESSION to VALUE.
CALLBACK is called without arguments once the agent answered."
  (agentel-connection-request
   (agentel-session-connection session) "session/set_config_option"
   `((sessionId . ,(agentel-session-id session)) (configId . ,id) (value . ,value))
   :on-success (lambda (result)
                 (agentel-config--store session (alist-get 'configOptions result))
                 (when callback (funcall callback)))
   :on-failure (lambda (error)
                 (agentel-chat-notice session
                                      (format "Could not set %s: %s" id
                                              (alist-get 'message error))
                                      'error)
                 (when callback (funcall callback)))))

(defun agentel-config--apply (session requests)
  "Apply REQUESTS, a list of (ID . VALUE), to SESSION one after another.
Values are not checked here: the agent accepts aliases such as \"opus\"
that are not among the values it lists."
  (if-let* ((request (car requests)))
      (let ((option (agentel-config-option session (car request)))
            (next (lambda () (agentel-config--apply session (cdr requests)))))
        (cond
         ((not option)
          (agentel-chat-notice session (format "%s is not available for this session"
                                               (car request))
                               'error)
          (funcall next))
         (t (agentel-config-set session (car request) (cdr request) next))))
    (agentel-session-set-busy session nil)))

(defun agentel-config--on-started (session result options)
  "Record the config options in RESULT and apply the start OPTIONS to SESSION."
  (agentel-config--store session (alist-get 'configOptions result))
  (let ((requests (delq nil (mapcar (lambda (entry)
                                      (when-let* ((value (plist-get options (car entry))))
                                        (cons (cdr entry) value)))
                                    agentel-config--start-options))))
    (when requests
      (agentel-session-set-busy session t)
      (agentel-config--apply session requests))))

(defun agentel-config--restart-options (session)
  "Return the start options that give a new session the settings of SESSION.
Options the current model lacks are left out, so the new session does
not report them as unavailable."
  (mapcan (lambda (entry)
            (when-let* ((value (alist-get 'currentValue
                                          (agentel-config-option session (cdr entry)))))
              (list (car entry) value)))
          agentel-config--start-options))

(defun agentel-config--on-update (session update)
  "Follow config changes the agent reports in UPDATE for SESSION."
  (pcase (alist-get 'sessionUpdate update)
    ("config_option_update"
     (agentel-config--store session (alist-get 'configOptions update)))
    ("current_mode_update"
     (when-let* ((option (agentel-config-option session "mode")))
       (setf (alist-get 'currentValue option) (alist-get 'currentModeId update))
       (agentel-session-changed session)))))

;;;; Commands

(defun agentel-config--read (session id)
  "Read a new value of the option ID of SESSION."
  (let* ((option (or (agentel-config-option session id)
                     (user-error "This agent has no %s option" id)))
         (values (mapcar (lambda (v) (cons (alist-get 'name v) (alist-get 'value v)))
                         (alist-get 'options option)))
         (choice (completing-read (format "%s (now %s): " (alist-get 'name option)
                                          (agentel-config--value-name option))
                                  (agentel-chat-ordered-completion values) nil t)))
    (cdr (assoc choice values))))

(defun agentel-config-set-option (id)
  "Change the config option ID of the session of this buffer."
  (interactive
   (list (let ((options (mapcar (lambda (o) (cons (alist-get 'name o) (alist-get 'id o)))
                                (agentel-session-data agentel-chat--session
                                                      'config-options))))
           (cdr (assoc (completing-read "Option: " options nil t) options)))))
  (let ((session agentel-chat--session))
    (agentel-config-set session id (agentel-config--read session id))))

(defun agentel-config-set-model ()
  "Change the model of the session of this buffer."
  (interactive)
  (agentel-config-set-option "model"))

(defun agentel-config-set-effort ()
  "Change the effort of the session of this buffer."
  (interactive)
  (agentel-config-set-option "effort"))

(defun agentel-config-set-mode ()
  "Change the mode of the session of this buffer."
  (interactive)
  (agentel-config-set-option "mode"))

(keymap-set agentel-chat-mode-map "C-c C-o" #'agentel-config-set-option)
(keymap-set agentel-chat-mode-map "C-c C-m" #'agentel-config-set-mode)
(add-hook 'agentel-session-started-functions #'agentel-config--on-started)
(add-hook 'agentel-session-restart-options-functions #'agentel-config--restart-options)
(add-hook 'agentel-session-update-functions #'agentel-config--on-update)
(add-hook 'agentel-chat-header-functions #'agentel-config--header)

(provide 'agentel-config)
;;; agentel-config.el ends here
