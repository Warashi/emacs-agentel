;;; agentel-session.el --- Session registry for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A session is one ACP session: a top-level conversation or a subagent
;; spawned by one.  This file keeps every live session in a registry,
;; routes `session/update' notifications to them by session id, and
;; derives the state shown to the user.
;;
;; Features keep their own per-session values in `agentel-session-data'
;; and react through `agentel-session-update-functions' and
;; `agentel-session-changed-functions', so each feature can be removed
;; without touching this file.  `agentel-session-withhold-functions'
;; lets a feature keep an update from the others,
;; `agentel-session-waiting-functions' tells whether the user owes a
;; session an answer, and `agentel-session-running-functions' whether a
;; feature is at work in it.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (agentel-session (:constructor agentel-session--make)
                               (:copier nil))
  "One ACP session."
  id connection parent cwd title buffer busy ended
  (project nil :documentation "Name of the project the session works in.")
  (agent nil :documentation "Name of the agent in `agentel-agents' that runs it.")
  (alist nil :documentation "Per-feature values, see `agentel-session-data'."))

(defvar agentel-session--registry nil
  "Live sessions, oldest first.")

(defvar agentel-session-update-functions nil
  "Abnormal hook run for each `session/update' of a registered session.
Each function is called with the session and the update alist.")

(defvar agentel-session-withhold-functions nil
  "Abnormal hook asked whether to withhold a `session/update'.
Each function is called with the session and the update alist.  When
one returns non-nil, `agentel-session-update-functions' is not run for
the update.")

(defvar agentel-session-changed-functions nil
  "Abnormal hook run with a session whenever its visible state changes.")

(defvar agentel-session-waiting-functions nil
  "Abnormal hook asked whether a session waits for the user to answer.
Each function is called with the session.  When one returns non-nil,
the session waits.  Whoever answers it calls
`agentel-session-waiting-changed' when that changes.")

(defvar agentel-session-running-functions nil
  "Abnormal hook asked whether a feature is at work in a session.
Each function is called with the session.  When one returns non-nil,
the session runs.  Whoever works calls `agentel-session-changed' when
that changes.")

(cl-defun agentel-session-create (&key connection parent cwd project agent)
  "Create a session on CONNECTION and add it to the registry.
PARENT is the session that spawned this one as a subagent.  CWD is
the working directory, PROJECT the name of its project and AGENT the
name of the agent that runs it.  The session has no id until
`agentel-session-register' gives it one."
  (let ((session (agentel-session--make :connection connection
                                        :parent parent
                                        :cwd cwd
                                        :project project
                                        :agent agent)))
    (setq agentel-session--registry
          (append agentel-session--registry (list session)))
    (agentel-session-changed session)
    session))

(defun agentel-session-register (session id)
  "Give SESSION the id ID assigned by the agent."
  (setf (agentel-session-id session) id)
  (agentel-session-changed session))

(defun agentel-session-remove (session)
  "Remove SESSION from the registry."
  (setq agentel-session--registry (delq session agentel-session--registry))
  (agentel-session-changed session))

(defun agentel-session-get (id &optional connection)
  "Return the session whose id is ID, or nil.
With CONNECTION, only a session of that connection matches: ids are
chosen by each agent, so two agents may use the same one."
  (and id
       (seq-find (lambda (s) (and (equal (agentel-session-id s) id)
                                  (or (not connection)
                                      (eq (agentel-session-connection s) connection))))
                 agentel-session--registry)))

(defun agentel-session-list ()
  "Return all sessions, oldest first."
  agentel-session--registry)

(defun agentel-session-roots ()
  "Return the sessions that are not subagents, oldest first."
  (seq-remove #'agentel-session-parent agentel-session--registry))

(defun agentel-session-children (session)
  "Return the subagent sessions spawned by SESSION, oldest first."
  (seq-filter (lambda (s) (eq (agentel-session-parent s) session))
              agentel-session--registry))

(defun agentel-session-changed (session)
  "Tell listeners that SESSION changed."
  (run-hook-with-args 'agentel-session-changed-functions session))

(defun agentel-session-data (session key)
  "Return the value a feature stored in SESSION under KEY."
  (alist-get key (agentel-session-alist session)))

(gv-define-setter agentel-session-data (value session key)
  `(prog1 (setf (alist-get ,key (agentel-session-alist ,session)) ,value)
     (agentel-session-changed ,session)))

(defun agentel-session-set-busy (session busy)
  "Record whether SESSION is working on a turn, according to BUSY."
  (setf (agentel-session-busy session) busy)
  (agentel-session-changed session))

(defun agentel-session-set-ended (session reason)
  "Record that SESSION ended for REASON, a symbol shown as its state."
  (setf (agentel-session-ended session) reason
        (agentel-session-busy session) nil)
  (agentel-session-changed session))

(defun agentel-session-waiting-p (session)
  "Return non-nil if the user owes an answer to SESSION or to its subagents.
Whether a session waits is asked of `agentel-session-waiting-functions'."
  (or (run-hook-with-args-until-success 'agentel-session-waiting-functions session)
      (seq-some #'agentel-session-waiting-p (agentel-session-children session))))

(defun agentel-session-waiting-changed (session)
  "Tell listeners that whether SESSION waits for the user changed.
The sessions above it are told too, as they wait while it does."
  (while session
    (agentel-session-changed session)
    (setq session (agentel-session-parent session))))

(defun agentel-session-running-p (session)
  "Return non-nil if SESSION is at work and has not ended.
It is at work during a turn or while a feature of
`agentel-session-running-functions' says so."
  (and (not (agentel-session-ended session))
       (or (agentel-session-busy session)
           (run-hook-with-args-until-success 'agentel-session-running-functions
                                             session))))

(defun agentel-session-state (session)
  "Return the state of SESSION as a symbol.
It is `starting' before the agent assigns an id, the end reason once
ended, `waiting' while the user owes an answer to it or to one of its
subagents, `running' while it is at work, and `idle' otherwise."
  (cond ((agentel-session-ended session))
        ((not (agentel-session-id session)) 'starting)
        ((agentel-session-waiting-p session) 'waiting)
        ((agentel-session-running-p session) 'running)
        (t 'idle)))

(defun agentel-session-name (session)
  "Return a short human readable name for SESSION."
  (or (when-let* ((title (agentel-session-title session)))
        (string-trim (replace-regexp-in-string "[\n\t ]+" " " title)))
      (and (agentel-session-cwd session)
           (file-name-nondirectory
            (directory-file-name (agentel-session-cwd session))))
      "agent"))

(defun agentel-session-project-name (session)
  "Return the name of the project SESSION works in.
A subagent works in the project of its top-level session.  Outside of
a project, this is the name of the working directory."
  (while (agentel-session-parent session)
    (setq session (agentel-session-parent session)))
  (or (agentel-session-project session)
      (when-let* ((cwd (agentel-session-cwd session)))
        (file-name-nondirectory (directory-file-name cwd)))
      "agent"))

(defun agentel-session-dispatch (params &optional connection)
  "Route the `session/update' notification PARAMS to its session.
CONNECTION is the connection it arrived on."
  (let-alist params
    (when-let* ((session (agentel-session-get .sessionId connection)))
      (when (equal (alist-get 'sessionUpdate .update) "session_info_update")
        (when-let* ((title (alist-get 'title .update)))
          (setf (agentel-session-title session) title)
          (agentel-session-changed session)))
      (unless (run-hook-with-args-until-success
               'agentel-session-withhold-functions session .update)
        (run-hook-with-args 'agentel-session-update-functions
                            session .update)))))

(provide 'agentel-session)
;;; agentel-session.el ends here
