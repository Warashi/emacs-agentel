;;; agentel-files.el --- File path completion for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Completes the path of a file after an @ in the input, as in
;; "look at @src/foo.el".  The candidates are the files under the
;; directory of the session.  The text stays as typed: the agent reads
;; the file itself.

;;; Code:

(require 'subr-x)
(require 'agentel-session)
(require 'agentel-chat)

(defun agentel-files--git-list (dir)
  "Return the files under DIR that git tracks or does not ignore, or nil.
Git must not take the optional locks, such as refreshing the index:
completion runs while the agent runs git in the same directory, and
a lock held here would fail its command."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (zerop (call-process "git" nil '(t nil) nil "--no-optional-locks"
                                 "ls-files" "-z" "--cached" "--others"
                                 "--exclude-standard"))
        (split-string (buffer-string) "\0" t)))))

(defun agentel-files--list (dir)
  "Return the files under DIR, relative to it."
  (or (agentel-files--git-list dir)
      (mapcar (lambda (file) (file-relative-name file dir))
              (directory-files-recursively
               dir "" nil
               (lambda (subdir)
                 (not (string-prefix-p "." (file-name-nondirectory subdir))))))))

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
          (completion-table-dynamic (lambda (_) (agentel-files--list cwd)))
          :exclusive 'no)))

(defun agentel-files--setup ()
  "Enable file path completion in the current chat buffer."
  (add-hook 'completion-at-point-functions
            #'agentel-files-completion-at-point nil t))

(add-hook 'agentel-chat-mode-hook #'agentel-files--setup)

(provide 'agentel-files)
;;; agentel-files.el ends here
