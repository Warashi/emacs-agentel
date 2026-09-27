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
(require 'agentel-ui)

(defvar agentel-session-started-functions)
(defvar agentel-session-restart-options-functions)

(defconst agentel-config--start-options
  '((:model . "model") (:effort . "thought_level") (:mode . "mode"))
  "Start options of `agentel-start' and the category of the option each sets.")

(defconst agentel-config--header-order '("model" "thought_level" "mode")
  "Config option categories shown first in the header line, in this order.")

(defun agentel-config--update (message data)
  "Return the config DATA of a session changed by MESSAGE.
DATA has the `options', each an alist of its `id', `name', `category',
current `value' and the `choices' of values, each an alist of its
`value' and `name'; and `applying' while the start options are
applied.  MESSAGE is one of (report OPTIONS), which replaces the
options, (select CATEGORY VALUE), (start-applying) and
\\=(finish-applying)."
  (pcase-let ((`(,field . ,value)
               (pcase message
                 (`(report ,options) `(options . ,options))
                 (`(select ,category ,value)
                  `(options
                    . ,(mapcar (lambda (option)
                                 (if (equal (alist-get 'category option) category)
                                     (let ((option (copy-alist option)))
                                       (setf (alist-get 'value option) value)
                                       option)
                                   option))
                               (alist-get 'options data))))
                 ('(start-applying) '(applying . t))
                 ('(finish-applying) '(applying)))))
    (cons (cons field value) (assq-delete-all field (copy-alist data)))))

(agentel-store-define 'agentel-config #'agentel-config--update)

(defun agentel-config--send (session message)
  "Change the config of SESSION by MESSAGE, see `agentel-config--update'."
  (agentel-store-dispatch (agentel-session-store session) 'config 'agentel-config
                          message))

(defun agentel-config--get (session field)
  "Return the value FIELD of the config of SESSION has now."
  (when-let* ((model (agentel-store-find (agentel-session-store session) 'config)))
    (agentel-store-get model field)))

(defun agentel-config--options (session)
  "Return the config options of SESSION."
  (agentel-config--get session 'options))

(defun agentel-config-option (session category)
  "Return the config option of CATEGORY in SESSION."
  (seq-find (lambda (o) (equal (alist-get 'category o) category))
            (agentel-config--options session)))

(defun agentel-config-value (session category)
  "Return the current value of the config option of CATEGORY in SESSION."
  (alist-get 'value (agentel-config-option session category)))

(defun agentel-config--report (session options)
  "Report to SESSION the config OPTIONS the agent gave, if any."
  (when options
    (agentel-config--send
     session
     `(report
       ,(mapcar (lambda (option)
                  (let-alist option
                    `((id . ,.id) (name . ,.name) (category . ,.category)
                      (value . ,.currentValue)
                      (choices . ,(mapcar (lambda (choice)
                                            (let-alist choice
                                              `((value . ,.value) (name . ,.name))))
                                          .options)))))
                options)))))

(defun agentel-config--value-name (option)
  "Return the display name of the current value of OPTION."
  (let ((value (alist-get 'value option)))
    (or (alist-get 'name (seq-find (lambda (v) (equal (alist-get 'value v) value))
                                   (alist-get 'choices option)))
        (format "%s" value))))

(defun agentel-config--view (model _options)
  "Return the settings of the config MODEL of a session as text, or nil."
  (when-let* ((options (agentel-store-get model 'options)))
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

(agentel-ui-define-view 'agentel-config #'agentel-config--view :header 10)

;;;; Changing options

(defun agentel-config-set (session id value &optional callback)
  "Set the config option ID of SESSION to VALUE.
CALLBACK is called without arguments once the agent answered."
  (agentel-connection-request
   (agentel-session-connection session) "session/set_config_option"
   `((sessionId . ,(agentel-session-id session)) (configId . ,id) (value . ,value))
   :on-success (lambda (result)
                 (agentel-config--report session (alist-get 'configOptions result))
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
  (let ((values (alist-get 'choices option)))
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
    (agentel-config--send session '(finish-applying))))

(defun agentel-config--on-started (session result options)
  "Record the config options in RESULT and apply the start OPTIONS to SESSION."
  (agentel-config--report session (alist-get 'configOptions result))
  (let ((requests (delq nil (mapcar (lambda (entry)
                                      (when-let* ((value (plist-get options (car entry))))
                                        `(,(car entry) ,(cdr entry) . ,value)))
                                    agentel-config--start-options))))
    (when requests
      (agentel-config--send session '(start-applying))
      (agentel-config--apply session requests))))

(defun agentel-config--applying-p (session)
  "Return non-nil while SESSION applies its start options."
  (agentel-config--get session 'applying))

(defun agentel-config--restart-options (session)
  "Return the start options that give a new session the settings of SESSION.
Options the current model lacks are left out, so the new session does
not report them as unavailable."
  (mapcan (lambda (entry)
            (when-let* ((value (agentel-config-value session (cdr entry))))
              (list (car entry) value)))
          agentel-config--start-options))

(defun agentel-config--on-update (session update)
  "Follow config changes the agent reports in UPDATE for SESSION."
  (pcase (alist-get 'sessionUpdate update)
    ("config_option_update"
     (agentel-config--report session (alist-get 'configOptions update)))
    ("current_mode_update"
     (agentel-config--send session `(select "mode" ,(alist-get 'currentModeId update))))))

;;;; Commands

(defun agentel-config--read (session id)
  "Read a new value of the option ID of SESSION."
  (let* ((option (seq-find (lambda (o) (equal (alist-get 'id o) id))
                           (agentel-config--options session)))
         (values (mapcar (lambda (v) (cons (alist-get 'name v) (alist-get 'value v)))
                         (alist-get 'choices option)))
         (choice (completing-read (format "%s (now %s): " (alist-get 'name option)
                                          (agentel-config--value-name option))
                                  (agentel-chat-ordered-completion values) nil t)))
    (cdr (assoc choice values))))

(defun agentel-config-set-option (id)
  "Change the config option ID of the session of this buffer."
  (interactive
   (list (let ((options (mapcar (lambda (o) (cons (alist-get 'name o) (alist-get 'id o)))
                                (agentel-config--options agentel-chat--session))))
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

(provide 'agentel-config)
;;; agentel-config.el ends here
