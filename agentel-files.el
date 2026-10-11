;;; agentel-files.el --- File path completion for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Completes the path of a file after an @ in the input, as in
;; "look at @src/foo.el".  The candidates are the files under the
;; directory of the session.  The text stays as typed: the agent reads
;; the file itself.
;;
;; Listing the files of a large repository takes long, and completion
;; may run on every key press, so a process lists them in the
;; background and completion answers from the last list.  The first
;; list is made when a session starts; a later completion makes a new
;; one when the last is older than `agentel-files-refresh-interval'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-chat)

(defvar agentel-session-started-functions)

(defcustom agentel-files-refresh-interval 5
  "Seconds after which completing a path lists the files again."
  :type 'number
  :group 'agentel)

(cl-defstruct (agentel-files--cache (:constructor agentel-files--cache-make))
  "The files under a directory, and the process listing them again."
  files time process)

(defvar agentel-files--caches (make-hash-table :test #'equal)
  "The `agentel-files--cache' of each directory.
A subagent has the directory of its parent, so they share one.")

(defun agentel-files--cache (dir)
  "Return the cache of DIR, making it if there is none."
  (or (gethash dir agentel-files--caches)
      (puthash dir (agentel-files--cache-make) agentel-files--caches)))

(defconst agentel-files--git-command
  '("git" "--no-optional-locks" "ls-files" "-z" "--cached" "--others"
    "--exclude-standard")
  "The command listing the files git tracks or does not ignore.
Git must not take the optional locks, such as refreshing the index:
completion runs while the agent runs git in the same directory, and
a lock held here would fail its command.")

(defconst agentel-files--find-command
  '("find" "." "(" "-type" "d" "-name" ".?*" "-prune" ")" "-o" "-type" "f"
    "-print0")
  "The command listing the files outside a repository but the hidden ones.")

(defun agentel-files--run (dir command on-exit)
  "Run COMMAND in DIR in the background and call ON-EXIT when it exits.
ON-EXIT is called with the exit status and the file names COMMAND
printed, separated by NUL.  When COMMAND cannot start, ON-EXIT is
called with 127 once Emacs is idle.  Return the process."
  (let ((default-directory dir)
        (output (generate-new-buffer " *agentel-files*"))
        (errors (generate-new-buffer " *agentel-files-errors*")))
    (condition-case nil
        (make-process
         :name "agentel-files" :buffer output :stderr errors :command command
         :connection-type 'pipe :noquery t
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((files (with-current-buffer output
                            (split-string (buffer-string) "\0" t))))
               (kill-buffer output)
               (kill-buffer errors)
               (funcall on-exit (process-exit-status process) files)))))
      (file-missing
       (kill-buffer output)
       (kill-buffer errors)
       (run-at-time 0 nil on-exit 127 nil)
       nil))))

(defun agentel-files--refresh (dir)
  "List the files under DIR in the background, unless that is under way."
  (let ((cache (agentel-files--cache dir)))
    (unless (process-live-p (agentel-files--cache-process cache))
      (cl-flet ((done (files)
                  (setf (agentel-files--cache-files cache) files
                        (agentel-files--cache-time cache) (float-time)
                        (agentel-files--cache-process cache) nil)))
        (setf (agentel-files--cache-process cache)
              (agentel-files--run
               dir agentel-files--git-command
               (lambda (status files)
                 (if (zerop status)
                     (done files)
                   ;; Not a repository, or no git.
                   (setf (agentel-files--cache-process cache)
                         (agentel-files--run
                          dir agentel-files--find-command
                          (lambda (_status files)
                            (done (mapcar (lambda (file)
                                            (string-remove-prefix "./" file))
                                          files)))))))))))))

(defun agentel-files--candidates (dir)
  "Return the files under DIR as last listed, listing them again if stale."
  (let ((cache (agentel-files--cache dir)))
    (when (or (null (agentel-files--cache-time cache))
              (> (- (float-time) (agentel-files--cache-time cache))
                 agentel-files-refresh-interval))
      (agentel-files--refresh dir))
    (agentel-files--cache-files cache)))

(defun agentel-files-completion-at-point ()
  "Complete the path of a file after an @ in the input."
  (when-let* ((session (agentel-session-current))
              (start (agentel-chat-input-start))
              (cwd (agentel-session-cwd session))
              ((>= (point) start))
              (at (save-excursion
                    (skip-chars-backward "^ \t\n" start)
                    (and (eq (char-after) ?@) (point)))))
    (list (1+ at) (point)
          (completion-table-dynamic (lambda (_) (agentel-files--candidates cwd)))
          :exclusive 'no)))

(defun agentel-files--on-started (session _result _options)
  "List the files under the directory of SESSION once it started."
  (when-let* ((cwd (agentel-session-cwd session)))
    (agentel-files--refresh cwd)))

(defun agentel-files--setup ()
  "Enable file path completion in the current chat buffer."
  (add-hook 'completion-at-point-functions
            #'agentel-files-completion-at-point nil t))

(add-hook 'agentel-session-started-functions #'agentel-files--on-started)
(add-hook 'agentel-chat-mode-hook #'agentel-files--setup)

(provide 'agentel-files)
;;; agentel-files.el ends here
