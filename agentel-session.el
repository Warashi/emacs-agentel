;;; agentel-session.el --- Session registry for agentel  -*- lexical-binding: t; -*-

;;; Commentary:

;; A session is one ACP session: a top-level conversation or a subagent
;; spawned by one.  This file keeps every live session in a registry,
;; routes `session/update' notifications to them by session id, and
;; derives the state shown to the user.
;;
;; Features keep their own per-session values in `agentel-session-data'
;; and react through `agentel-session-update-functions' and
;; `agentel-session-changed-functions', so each feature can be removed
;; without touching this file.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (agentel-session (:constructor agentel-session--make)
                               (:copier nil))
  "One ACP session."
  id connection parent cwd title buffer busy ended pending
  (alist nil :documentation "Per-feature values, see `agentel-session-data'."))

(defvar agentel-session--registry nil
  "Live sessions, oldest first.")

(defvar agentel-session-update-functions nil
  "Abnormal hook run for each `session/update' of a registered session.
Each function is called with the session and the update alist.")

(defvar agentel-session-changed-functions nil
  "Abnormal hook run with a session whenever its visible state changes.")

(cl-defun agentel-session-create (&key connection parent cwd)
  "Create a session on CONNECTION and add it to the registry.
PARENT is the session that spawned this one as a subagent.  CWD is
the working directory.  The session has no id until
`agentel-session-register' gives it one."
  (let ((session (agentel-session--make :connection connection
                                        :parent parent
                                        :cwd cwd)))
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

(defun agentel-session-get (id)
  "Return the session whose id is ID, or nil."
  (and id
       (seq-find (lambda (s) (equal (agentel-session-id s) id))
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

(defun agentel-session-add-pending (session item)
  "Record that SESSION waits for the user to answer ITEM."
  (setf (agentel-session-pending session)
        (append (agentel-session-pending session) (list item)))
  (agentel-session-changed session))

(defun agentel-session-remove-pending (session item)
  "Record that the user answered ITEM of SESSION."
  (setf (agentel-session-pending session)
        (delq item (agentel-session-pending session)))
  (agentel-session-changed session))

(defun agentel-session-state (session)
  "Return the state of SESSION as a symbol.
It is `starting' before the agent assigns an id, the end reason once
ended, `waiting' while the user owes an answer, `running' during a
turn, and `idle' otherwise."
  (cond ((agentel-session-ended session))
        ((not (agentel-session-id session)) 'starting)
        ((agentel-session-pending session) 'waiting)
        ((agentel-session-busy session) 'running)
        (t 'idle)))

(defun agentel-session-name (session)
  "Return a short human readable name for SESSION."
  (or (when-let* ((title (agentel-session-title session)))
        (string-trim (replace-regexp-in-string "[\n\t ]+" " " title)))
      (and (agentel-session-cwd session)
           (file-name-nondirectory
            (directory-file-name (agentel-session-cwd session))))
      "agent"))

(defun agentel-session-dispatch (params)
  "Route the `session/update' notification PARAMS to its session."
  (let-alist params
    (when-let* ((session (agentel-session-get .sessionId)))
      (when (equal (alist-get 'sessionUpdate .update) "session_info_update")
        (when-let* ((title (alist-get 'title .update)))
          (setf (agentel-session-title session) title)
          (agentel-session-changed session)))
      (run-hook-with-args 'agentel-session-update-functions
                          session .update))))

(provide 'agentel-session)
;;; agentel-session.el ends here
