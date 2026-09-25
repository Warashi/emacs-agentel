;;; agentel.el --- ACP client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Shinnosuke Sawada-Dazai <3600530+Warashi@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (acp "0.15"))
;; Keywords: tools, convenience

;;; Commentary:

;; An Agent Client Protocol (ACP) client built on acp.el.
;;
;; `agentel-start' starts an agent and opens a session buffer.  Its
;; keyword arguments are also handed to the features through
;; `agentel-session-started-functions', so a feature can take its own
;; start options without this file knowing them.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-chat)
(require 'agentel-permission)
(require 'agentel-elicitation)
(require 'agentel-commands)
(require 'agentel-resume)
(require 'agentel-config)
(require 'agentel-usage)
(require 'agentel-subagent)
(require 'agentel-async-task)
(require 'agentel-list)
(require 'agentel-markdown)
(require 'agentel-focus)

(defcustom agentel-command "claude-agent-acp"
  "Program that speaks ACP on stdio."
  :type 'string
  :group 'agentel)

(defcustom agentel-command-args nil
  "Arguments of `agentel-command'."
  :type '(repeat string)
  :group 'agentel)

(defcustom agentel-command-prefix nil
  "Command line that runs `agentel-command', such as a container runner.
The agent is started as this list followed by `agentel-command' and
`agentel-command-args'.  When nil the agent is started directly.

A function is called with the directory of the session and returns
the list, or nil to start the agent directly, so the prefix can
depend on the project:

  (lambda (cwd)
    (list \"docker\" \"run\" \"--rm\" \"-i\"
          \"-v\" (format \"%s:%s\" cwd cwd) \"-w\" cwd \"image\"))"
  :type '(choice (repeat string) function)
  :group 'agentel)

(defcustom agentel-environment nil
  "Extra environment of `agentel-command', as \"VAR=value\" strings.
With `agentel-command-prefix' the variables are given to the first
program of the prefix, which may not pass them on to the agent."
  :type '(repeat string)
  :group 'agentel)

(defun agentel--command-line (cwd)
  "Return the program and arguments that start the agent in CWD."
  (append (if (functionp agentel-command-prefix)
              (funcall agentel-command-prefix cwd)
            agentel-command-prefix)
          (list agentel-command)
          agentel-command-args))

(defvar agentel-session-started-functions nil
  "Abnormal hook run once the agent created or loaded a session.
Each function is called with the session, the result of
`session/new' or `session/load', and the keyword arguments given to
`agentel-start'.")

(defun agentel--fail (session message)
  "End SESSION because it could not start, showing MESSAGE."
  (agentel-chat-notice session message 'error)
  (agentel-session-set-ended session 'failed))

(defun agentel--open-session (session options)
  "Create or load the ACP session for SESSION as OPTIONS say."
  (let* ((connection (agentel-session-connection session))
         (session-id (plist-get options :session-id))
         (params `((cwd . ,(agentel-session-cwd session)) (mcpServers . []))))
    (when session-id
      ;; The agent replays the history before it answers, so the session
      ;; must be found by id before the request is sent.
      (agentel-session-register session session-id)
      (agentel-session-set-busy session t)
      (push (cons 'sessionId session-id) params))
    (agentel-connection-request
     connection (if session-id "session/load" "session/new") params
     :on-success
     (lambda (result)
       (if session-id
           (progn
             (agentel-session-set-busy session nil)
             ;; The replayed history ends without a turn ending it.
             (with-current-buffer (agentel-session-buffer session)
               (agentel-chat-finish-message)))
         (agentel-session-register session (alist-get 'sessionId result)))
       (run-hook-with-args 'agentel-session-started-functions
                           session result options))
     :on-failure
     (lambda (error)
       (agentel--fail session (format "Could not start the session: %s"
                                      (alist-get 'message error)))))))

(defun agentel--report-exit (connection)
  "Tell the sessions of CONNECTION that their agent exited, and why."
  (let ((stderr (agentel-connection-stderr connection)))
    (dolist (session (agentel-session-roots))
      (when (eq (agentel-session-connection session) connection)
        (agentel-chat-notice
         session
         (concat "Agent exited"
                 (if stderr
                     (concat ":\n" (mapconcat (lambda (l) (concat "  " l))
                                              (last stderr 5) "\n"))
                   ""))
         'error)))))

(add-hook 'agentel-connection-exit-functions #'agentel--report-exit)

(defun agentel--kill-sessions ()
  "Stop the agent of the session in the buffer being killed."
  (when-let* ((session agentel-chat--session)
              ((not (agentel-session-parent session)))
              (connection (agentel-session-connection session)))
    (agentel-connection-shutdown connection)
    (dolist (other (agentel-session-list))
      (when (eq (agentel-session-connection other) connection)
        (agentel-session-remove other)
        (let ((buffer (agentel-session-buffer other)))
          (when (and (buffer-live-p buffer) (not (eq buffer (current-buffer))))
            (kill-buffer buffer)))))))

;;;###autoload
(cl-defun agentel-start (&rest options &key cwd (display t) &allow-other-keys)
  "Start an agent in CWD and open a buffer for a new session.
Return the session.

OPTIONS are keyword arguments.  CWD defaults to `default-directory'.
When DISPLAY is nil the buffer is not shown.  :session-id loads that
earlier session instead of creating one.  Other keywords, such as
:model, :effort and :mode, are read by the features that handle them.

  (agentel-start :cwd \"~/src/project/\" :model \"opus\" :mode \"plan\")"
  (let* ((cwd (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (session (agentel-session-create :cwd cwd))
         (buffer (agentel-chat-open session :input t))
         (command-line (agentel--command-line cwd)))
    (with-current-buffer buffer
      (setq default-directory cwd)
      (add-hook 'kill-buffer-hook #'agentel--kill-sessions nil t))
    (setf (agentel-session-connection session)
          (agentel-connection-start
           :command (car command-line) :args (cdr command-line)
           :env agentel-environment :cwd cwd
           :on-ready (lambda (_connection) (agentel--open-session session options))
           :on-failure (lambda (error)
                         (agentel--fail session
                                        (format "Could not start the agent: %s"
                                                (alist-get 'message error))))))
    (when display (pop-to-buffer buffer))
    session))

;;;###autoload
(defun agentel (&optional directory)
  "Start an agent session in the current project.
With a prefix argument, ask for the DIRECTORY instead."
  (interactive
   (list (if current-prefix-arg
             (read-directory-name "Start agent in: ")
           (if-let* ((project (project-current)))
               (project-root project)
             default-directory))))
  (agentel-start :cwd directory))

(provide 'agentel)
;;; agentel.el ends here
