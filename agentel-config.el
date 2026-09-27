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
;;
;; Agents name these options differently (Copilot CLI calls the effort
;; reasoning_effort), so they are found by their category.  A value may
;; also be given by its name, since Copilot CLI takes only URLs as mode
;; values.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-chat)

(defvar agentel-session-started-functions)
(defvar agentel-session-restart-options-functions)

(defconst agentel-config--start-options
  '((:model . "model") (:effort . "thought_level") (:mode . "mode"))
  "Start options of `agentel-start' and the category of the option each sets.")

(defconst agentel-config--header-order '("model" "thought_level" "mode")
  "Config option categories shown first in the header line, in this order.")

(defun agentel-config-option (session category)
  "Return the config option of CATEGORY in SESSION."
  (seq-find (lambda (o) (equal (alist-get 'category o) category))
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
               (if (member (alist-get 'category option) agentel-config--header-order)
                   (agentel-config--value-name option)
                 (format "%s: %s" (alist-get 'name option)
                         (agentel-config--value-name option))))
             (seq-sort-by (lambda (o)
                            (or (seq-position agentel-config--header-order
                                              (alist-get 'category o))
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
                 (agentel-conversation-note session
                                            (format "Could not set %s: %s" id
                                                    (alist-get 'message error))
                                            'error)
                 (when callback (funcall callback)))))

(defun agentel-config--value (option value)
  "Return the value of OPTION that VALUE gives, directly or by its name.
Other values are left to the agent, which accepts aliases such as
\"opus\" that are not among the values it lists."
  (let ((values (alist-get 'options option)))
    (if (seq-find (lambda (v) (equal (alist-get 'value v) value)) values)
        value
      (or (alist-get 'value (seq-find (lambda (v)
                                        (string-equal-ignore-case
                                         (alist-get 'name v) value))
                                      values))
          value))))

(defun agentel-config--apply (session requests)
  "Apply REQUESTS to SESSION one after another.
Each request is (KEYWORD CATEGORY . VALUE), a start option with the
category of the option it sets."
  (if-let* ((request (car requests)))
      (let ((option (agentel-config-option session (cadr request)))
            (next (lambda () (agentel-config--apply session (cdr requests)))))
        (cond
         ((not option)
          (agentel-conversation-note
           session (format "%s is not available for this session"
                           (substring (symbol-name (car request)) 1))
           'error)
          (funcall next))
         (t (agentel-config-set session (alist-get 'id option)
                                (agentel-config--value option (cddr request))
                                next))))
    (setf (agentel-session-data session 'config-applying) nil)))

(defun agentel-config--on-started (session result options)
  "Record the config options in RESULT and apply the start OPTIONS to SESSION."
  (agentel-config--store session (alist-get 'configOptions result))
  (let ((requests (delq nil (mapcar (lambda (entry)
                                      (when-let* ((value (plist-get options (car entry))))
                                        `(,(car entry) ,(cdr entry) . ,value)))
                                    agentel-config--start-options))))
    (when requests
      (setf (agentel-session-data session 'config-applying) t)
      (agentel-config--apply session requests))))

(defun agentel-config--applying-p (session)
  "Return non-nil while SESSION applies its start options."
  (agentel-session-data session 'config-applying))

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
  (let* ((option (seq-find (lambda (o) (equal (alist-get 'id o) id))
                           (agentel-session-data session 'config-options)))
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

(defun agentel-config--set-category (category)
  "Change the config option of CATEGORY in the session of this buffer."
  (agentel-config-set-option
   (alist-get 'id (or (agentel-config-option agentel-chat--session category)
                      (user-error "This agent has no %s option" category)))))

(defun agentel-config-set-model ()
  "Change the model of the session of this buffer."
  (interactive)
  (agentel-config--set-category "model"))

(defun agentel-config-set-effort ()
  "Change the effort of the session of this buffer."
  (interactive)
  (agentel-config--set-category "thought_level"))

(defun agentel-config-set-mode ()
  "Change the mode of the session of this buffer."
  (interactive)
  (agentel-config--set-category "mode"))

(keymap-set agentel-chat-mode-map "C-c C-o" #'agentel-config-set-option)
(keymap-set agentel-chat-mode-map "C-c C-m" #'agentel-config-set-mode)
(add-hook 'agentel-session-started-functions #'agentel-config--on-started)
(add-hook 'agentel-session-running-functions #'agentel-config--applying-p)
(add-hook 'agentel-session-restart-options-functions #'agentel-config--restart-options)
(add-hook 'agentel-session-update-functions #'agentel-config--on-update)
(add-hook 'agentel-chat-header-functions #'agentel-config--header)

(provide 'agentel-config)
;;; agentel-config.el ends here
