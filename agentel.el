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
;; start options without this file knowing them.  In return,
;; `agentel-session-restart-options-functions' asks the features for
;; the options that start a session again as it is now.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-unprompted)
(require 'agentel-chat)
(require 'agentel-title)
(require 'agentel-permission)
(require 'agentel-elicitation)
(require 'agentel-commands)
(require 'agentel-files)
(require 'agentel-resume)
(require 'agentel-clear)
(require 'agentel-config)
(require 'agentel-usage)
(require 'agentel-subagent)
(require 'agentel-copilot-subagent)
(require 'agentel-async-task)
(require 'agentel-terminal)
(require 'agentel-background)
(require 'agentel-list)
(require 'agentel-pending)
(require 'agentel-summary)
(require 'agentel-markdown)
(require 'agentel-focus)
(require 'agentel-notify)
(require 'agentel-sleep)

(defcustom agentel-agents
  '((claude "claude-agent-acp")
    (copilot "copilot" "--acp"))
  "Agents that speak ACP on stdio, as (NAME PROGRAM ARG...).
`agentel-start' runs the one its :agent names."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'agentel)

(defcustom agentel-acp-v2-agents nil
  "Names of the agents in `agentel-agents' spoken to with ACP version 2.
The other agents are spoken to with version 1.  An agent that answers
another version fails to start."
  :type '(repeat symbol)
  :group 'agentel)

(defun agentel-agent-protocol-version (agent)
  "Return the ACP version to speak to AGENT."
  (if (memq agent agentel-acp-v2-agents) 2 1))

(defcustom agentel-default-agent 'claude
  "Name of the agent in `agentel-agents' a session runs by default."
  :type 'symbol
  :group 'agentel)

(defcustom agentel-command-prefix nil
  "Command line that runs the agent, such as a container runner.
The agent is started as this list followed by its program and
arguments in `agentel-agents'.  When nil the agent is started directly.

A function is called with the directory of the session and returns
the list, or nil to start the agent directly, so the prefix can
depend on the project:

  (lambda (cwd)
    (list \"docker\" \"run\" \"--rm\" \"-i\"
          \"-v\" (format \"%s:%s\" cwd cwd) \"-w\" cwd \"image\"))"
  :type '(choice (repeat string) function)
  :group 'agentel)

(defcustom agentel-environment nil
  "Extra environment of the agent, as \"VAR=value\" strings.
With `agentel-command-prefix' the variables are given to the first
program of the prefix, which may not pass them on to the agent.

A function is called with the directory of the session and returns
the list, so the variables can depend on the project:

  (lambda (cwd)
    (when-let* ((project (project-current nil cwd)))
      (list (concat \"CLAUDE_CODE_SESSION_NAME=\" (project-name project)))))"
  :type '(choice (repeat string) function)
  :group 'agentel)

(defun agentel-agent-environment (cwd)
  "Return the extra environment of an agent started in CWD."
  (if (functionp agentel-environment)
      (funcall agentel-environment cwd)
    agentel-environment))

(defun agentel-agent-command-line (agent cwd)
  "Return the program and arguments that start AGENT in CWD."
  (append (if (functionp agentel-command-prefix)
              (funcall agentel-command-prefix cwd)
            agentel-command-prefix)
          (or (alist-get agent agentel-agents)
              (user-error "No agent named %s in `agentel-agents'" agent))))

(defvar agentel-session-started-functions nil
  "Abnormal hook run once the agent created or loaded a session.
Each function is called with the session, the result of
`session/new', `session/load' or `session/resume', and the keyword
arguments given to `agentel-start'.")

(defvar agentel-session-restart-options-functions nil
  "Functions returning start options that recreate a session's settings.
Each function is called with the session and returns keyword
arguments of `agentel-start', such as (:model \"opus\"), so a new
session can start as the current one is now.")

(defun agentel-session-restart-options (session)
  "Return the keyword arguments of `agentel-start' that recreate SESSION.
The new session starts empty, in the directory and with the settings
of SESSION."
  (apply #'append
         (list :cwd (agentel-session-cwd session)
               :agent (agentel-session-agent session))
         (mapcar (lambda (function) (funcall function session))
                 agentel-session-restart-options-functions)))

(defun agentel--fail (session message)
  "End SESSION because it could not start, showing MESSAGE."
  (agentel-conversation-note session message 'error)
  (agentel-session-send session '(end failed)))

(defun agentel--open-session (session options)
  "Create or load the ACP session for SESSION as OPTIONS say."
  (let* ((connection (agentel-session-connection session))
         (session-id (plist-get options :session-id))
         (params `((cwd . ,(agentel-session-cwd session)) (mcpServers . []))))
    (when session-id
      ;; The agent replays the history before it answers, so the session
      ;; must be found by id before the request is sent.
      (agentel-session-register session session-id)
      (agentel-session-send session '(start-loading))
      (push (cons 'sessionId session-id) params)
      ;; ACP version 2 resumes a session, and replays its history only
      ;; when asked to.
      (when (eql (agentel-connection-version connection) 2)
        (push '(replayFrom . ((type . "start"))) params)))
    (agentel-connection-request
     connection (cond ((not session-id) "session/new")
                      ((eql (agentel-connection-version connection) 2) "session/resume")
                      (t "session/load"))
     params
     :on-success
     (lambda (result)
       (if session-id
           (progn
             (agentel-session-send session '(finish-loading))
             ;; The replayed history ends without a turn ending it.
             (agentel-conversation-finish-message session))
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
        (agentel-conversation-note
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
  (when-let* ((session (agentel-session-current))
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
(cl-defun agentel-start (&rest options &key cwd (agent agentel-default-agent)
                                (display t) &allow-other-keys)
  "Start AGENT in CWD and open a buffer for a new session.
Return the session.

OPTIONS are keyword arguments.  AGENT names an entry of
`agentel-agents' and defaults to `agentel-default-agent'.  CWD
defaults to `default-directory'.  When DISPLAY is nil the buffer is
not shown.  :session-id loads that earlier session instead of creating
one.  Other keywords, such as :model, :effort and :mode, are read by
the features that handle them.

  (agentel-start :cwd \"~/src/project/\" :model \"opus\" :mode \"plan\")"
  (let* ((cwd (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (command-line (agentel-agent-command-line agent cwd))
         (session (agentel-session-create
                   :cwd cwd
                   :agent agent
                   :project (when-let* ((project (project-current nil cwd)))
                              (project-name project))))
         (buffer (agentel-chat-open session :input t)))
    (with-current-buffer buffer
      (setq default-directory cwd)
      (add-hook 'kill-buffer-hook #'agentel--kill-sessions nil t))
    (setf (agentel-session-connection session)
          (agentel-connection-start
           :command (car command-line) :args (cdr command-line)
           :env (agentel-agent-environment cwd) :cwd cwd
           :protocol-version (agentel-agent-protocol-version agent)
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
