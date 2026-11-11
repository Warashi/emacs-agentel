;;; agentel-summary.el --- Sum up sessions that need the user  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A session the user comes back to among many is summed up for them:
;; what it is for, where it stands, what it asks of them and what they
;; could answer.  The user still decides.
;;
;; `agentel-summary-run' hands an excerpt of the conversation to an agent
;; of its own, started for that one summary and stopped once it
;; answered, as a session is stopped by its process rather than closed.
;; The agent runs with the command prefix and environment of the
;; sessions.  Its updates go to the connection, not to the session
;; registry, so the conversation is neither shown, listed nor notified,
;; and its permission requests find no session and are cancelled.
;;
;; The sessions summed up are those `agentel-list-pending' puts first,
;; one at a time in its order, so the session the user reaches first is
;; summed up first and agents do not pile up.  A summary belongs to the
;; state and the conversation it read: once either changes, as when the
;; user answers, it is forgotten, or cancelled while it is being made.
;; It stays when the session is only shown, so a session reached
;; through the list or `agentel-pending-next' keeps it.  The summary is
;; a model of the session store pinned above the prompt.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-store)
(require 'agentel-ui)
(require 'agentel-list)

(declare-function agentel-agent-command-line "agentel")
(declare-function agentel-agent-environment "agentel")
(declare-function agentel-agent-protocol-version "agentel")

(defcustom agentel-summary-agent nil
  "Agent and model that sum up the sessions that need the user.
Nil turns summaries off.  Otherwise a plist with the :agent, a name in
`agentel-agents', and the :model, a value that agent accepts, such as

  (:agent claude :model \"haiku\")"
  :type '(choice (const :tag "Off" nil)
                 (list (const :agent) symbol (const :model) string))
  :group 'agentel)

(defcustom agentel-summary-excerpt-items 10
  "Number of the last items of a conversation a summary reads."
  :type 'natnum
  :group 'agentel)

(defcustom agentel-summary-item-length 2000
  "Number of characters of an item a summary reads at most."
  :type 'natnum
  :group 'agentel)

(defvar agentel-summary--instructions
  "Sum up this agent session for its user, who comes back to it among \
many others and decides what to do next.  Do not use any tool.  Answer \
with four lines and nothing else, each led by its label: the goal of the \
session, where it stands, what the user is asked to answer or do now, and \
what you would answer and why, in one sentence.  Write the labels and the \
lines in the language of the conversation.

The session:

"
  "Instructions sent before the excerpt of a session.")

;;;; Excerpt

(defun agentel-summary--cut (text)
  "Return TEXT cut to `agentel-summary-item-length' characters."
  (truncate-string-to-width (string-trim text) agentel-summary-item-length nil nil "…"))

(defun agentel-summary--item (item)
  "Return the text of the conversation ITEM a summary reads, or nil."
  (let-alist (agentel-store-model-data item)
    (when-let* ((text
                 (pcase (agentel-store-model-type item)
                   ('user (concat "User: " .text))
                   ('agent (concat "Agent: " .text))
                   ('tool (format "Tool call: %s%s"
                                  (if .why (concat .why " — ") "")
                                  (or .title "")))
                   ((or 'permission 'elicitation)
                    (concat (if .waiting "Waiting for the user: " "Asked: ")
                            .question
                            (if .details (concat "\n" .details) "")))
                   ((or 'error 'stop 'notice) (concat "Note: " .text)))))
      (agentel-summary--cut text))))

(defun agentel-summary--waiting (session)
  "Return the items the subagents of SESSION wait on, at any depth."
  (mapcan (lambda (child)
            (append (seq-filter (lambda (item) (agentel-store-get item 'waiting))
                                (agentel-conversation-items child))
                    (agentel-summary--waiting child)))
          (agentel-session-children session)))

(defun agentel-summary-excerpt (session)
  "Return the excerpt of the conversation of SESSION a summary reads.
It has the title, the first prompt, the last
`agentel-summary-excerpt-items' items and what the subagents wait on."
  (let* ((items (agentel-conversation-items session))
         (first (seq-find (lambda (item) (eq (agentel-store-model-type item) 'user))
                          items))
         (last (last items agentel-summary-excerpt-items)))
    (string-join
     (delq nil
           (append
            (list (when-let* ((title (agentel-session-title session)))
                    (concat "Title: " title))
                  (when (and first (not (memq first last)))
                    (agentel-summary--item first))
                  (when (and first (not (memq first last))
                             (< (length last) (1- (length (memq first items)))))
                    "…"))
            (mapcar #'agentel-summary--item last)
            (mapcar #'agentel-summary--item (agentel-summary--waiting session))))
     "\n\n")))

;;;; Running the summarizer

(defun agentel-summary--model-option (result)
  "Return the id of the model option in the RESULT of `session/new'."
  (alist-get 'id (seq-find (lambda (option)
                             (equal (alist-get 'category option) "model"))
                           (alist-get 'configOptions result))))

(defun agentel-summary-run (cwd excerpt callback)
  "Sum up EXCERPT with `agentel-summary-agent' started in CWD.
CALLBACK is called with the summary, or nil when there is none.  It is
not called once the returned connection was shut down."
  (let* ((agent (plist-get agentel-summary-agent :agent))
         (model (plist-get agentel-summary-agent :model))
         (command-line (if (and agent model)
                           (agentel-agent-command-line agent cwd)
                         (user-error "`agentel-summary-agent' needs an :agent and a :model")))
         (session-id nil)
         (chunks nil)
         (connection nil)
         (finished nil)
         ;; An agent that cannot start fails before its connection is
         ;; returned, with no process to stop.
         (finish (lambda (text)
                   (unless (or finished
                               (and connection (agentel-connection-stopping connection)))
                     (setq finished t)
                     (funcall callback text)
                     (when connection (agentel-connection-shutdown connection)))))
         (fail (lambda (_error) (funcall finish nil)))
         (prompt
          (lambda ()
            (agentel-connection-request
             connection "session/prompt"
             `((sessionId . ,session-id)
               (prompt . [((type . "text")
                           (text . ,(concat agentel-summary--instructions excerpt)))]))
             :on-success (lambda (_result)
                           (funcall finish (when chunks
                                             (string-trim
                                              (apply #'concat (nreverse chunks))))))
             :on-failure fail)))
         (open
          (lambda (_connection)
            (agentel-connection-request
             connection "session/new" `((cwd . ,cwd) (mcpServers . []))
             :on-success
             (lambda (result)
               (setq session-id (alist-get 'sessionId result))
               (if-let* ((option (agentel-summary--model-option result)))
                   (agentel-connection-request
                    connection "session/set_config_option"
                    `((sessionId . ,session-id) (configId . ,option) (value . ,model))
                    :on-success (lambda (_result) (funcall prompt))
                    :on-failure fail)
                 (funcall finish nil)))
             :on-failure fail))))
    (setq connection
          (agentel-connection-start
           :command (car command-line) :args (cdr command-line)
           :env (agentel-agent-environment cwd) :cwd cwd
           :protocol-version (agentel-agent-protocol-version agent)
           :on-ready open
           :on-failure fail
           :on-update
           (lambda (params)
             (let-alist params
               (when (and (equal .sessionId session-id)
                          (equal .update.sessionUpdate "agent_message_chunk"))
                 (when-let* ((text .update.content.text))
                   (push text chunks)))))))
    connection))

;;;; Summing up the sessions that need the user

(defface agentel-summary-face
  '((t :inherit font-lock-doc-face))
  "Face of the summary of a session."
  :group 'agentel)

(defun agentel-summary--update (message _data)
  "Return the data of the summary of a session changed by MESSAGE.
It has the `key' of what the summary read and its `text', nil while
it is made or when there is none.  MESSAGE is (start KEY), (show KEY
TEXT) or (forget)."
  (pcase message
    (`(start ,key) `((key . ,key)))
    (`(show ,key ,text) `((key . ,key) (text . ,text)))
    ('(forget) nil)
    (_ (agentel-store-reject message))))

(defun agentel-summary--view (model _options)
  "Return the pinned text of the summary MODEL, or nil without one."
  (when-let* ((text (agentel-store-get model 'text)))
    (propertize text 'face 'agentel-summary-face)))

(agentel-store-define 'agentel-summary #'agentel-summary--update)
(agentel-ui-define-view 'agentel-summary #'agentel-summary--view :pin 5)

(defvar agentel-summary--start-function
  (lambda (cwd excerpt callback)
    (let ((connection (agentel-summary-run cwd excerpt callback)))
      (lambda () (agentel-connection-shutdown connection))))
  "Function that starts a summary and returns a function cancelling it.
It is called with the directory, the excerpt and the callback of
`agentel-summary-run'.")

(defvar agentel-summary--job nil
  "Summary being made, as (SESSION KEY CANCEL), or nil.")

(defvar agentel-summary--timer nil
  "Timer that looks for summaries to make or forget after changes.")

(defun agentel-summary--send (session message)
  "Change the summary of SESSION by MESSAGE."
  (agentel-store-dispatch (agentel-session-store session) 'summary 'agentel-summary
                          message))

(defun agentel-summary--get (session field)
  "Return the value FIELD of the summary of SESSION, or nil."
  (when-let* ((model (agentel-store-find (agentel-session-store session) 'summary)))
    (agentel-store-get model field)))

(defun agentel-summary-text (session)
  "Return the summary of SESSION, or nil without one."
  (agentel-summary--get session 'text))

(defun agentel-summary--item-count (session)
  "Return the number of items of SESSION and its subagents."
  (apply #'+ (length (agentel-conversation-items session))
         (mapcar #'agentel-summary--item-count (agentel-session-children session))))

(defun agentel-summary--key (session)
  "Return what a summary of SESSION read: its state and how much was said."
  (cons (agentel-session-state session) (agentel-summary--item-count session)))

(defun agentel-summary--job-p (session key)
  "Return non-nil if the summary being made is of SESSION reading KEY."
  (and (eq (car agentel-summary--job) session)
       (equal (cadr agentel-summary--job) key)))

(defun agentel-summary--finish (session key text)
  "Show TEXT as the summary of SESSION when it still reads KEY."
  (when (agentel-summary--job-p session key)
    (setq agentel-summary--job nil))
  (when (and (memq session (agentel-session-list))
             (equal (agentel-summary--get session 'key) key))
    (agentel-summary--send session `(show ,key ,text)))
  (agentel-summary--reconcile))

(defun agentel-summary--start (session)
  "Start summing up SESSION."
  (let ((key (agentel-summary--key session)))
    (agentel-summary--send session `(start ,key))
    ;; The job is set before the summary starts, as a summary failing at
    ;; once finishes it and starts the next one before this returns.
    (setq agentel-summary--job (list session key nil))
    (condition-case err
        (let ((cancel (funcall agentel-summary--start-function
                               (agentel-session-cwd session)
                               (agentel-summary-excerpt session)
                               (lambda (text)
                                 (agentel-summary--finish session key text)))))
          (when (agentel-summary--job-p session key)
            (setf (nth 2 agentel-summary--job) cancel)))
      (error
       (when (agentel-summary--job-p session key)
         (setq agentel-summary--job nil))
       (message "agentel-summary: %s" (error-message-string err))
       (agentel-summary--send session `(show ,key nil))))))

(defun agentel-summary--reconcile ()
  "Forget the summaries that are out of date and start the next one."
  (setq agentel-summary--timer nil)
  (pcase-let ((`(,session ,key ,cancel) agentel-summary--job))
    (when (and session
               (not (and (memq session (agentel-session-list))
                         (equal (agentel-summary--key session) key))))
      (setq agentel-summary--job nil)
      (when cancel (funcall cancel))))
  (dolist (session (agentel-session-roots))
    (when (and (agentel-summary--get session 'key)
               (not (equal (agentel-summary--get session 'key)
                           (agentel-summary--key session))))
      (agentel-summary--send session '(forget))))
  (when (and agentel-summary-agent (not agentel-summary--job))
    (when-let* ((next (seq-find (lambda (session)
                                  (not (agentel-summary--get session 'key)))
                                (agentel-list-pending))))
      (agentel-summary--start next))))

(defun agentel-summary--schedule (&rest _)
  "Look for summaries to make or forget soon, once for many changes."
  (unless agentel-summary--timer
    (setq agentel-summary--timer (run-with-timer 0 nil #'agentel-summary--reconcile))))

(add-hook 'agentel-session-changed-functions #'agentel-summary--schedule)

(provide 'agentel-summary)
;;; agentel-summary.el ends here
