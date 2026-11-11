;;; agentel-diff.el --- Diffs of file changes for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A tool call of ACP version 1 tells a file change as the old and the
;; new text, of the whole file or of a snippet.  This file shows the
;; change as a unified diff made by `diff-command', with the faces of
;; `diff-mode'.  Version 2 tells the change as a git patch, shown with the
;; same faces.

;;; Code:

(require 'diff-mode)

(defun agentel-diff--file (text)
  "Return a temporary file holding TEXT, ended by a newline."
  (make-temp-file "agentel-diff" nil nil
                  (if (or (string-empty-p text) (string-suffix-p "\n" text))
                      text
                    (concat text "\n"))))

(defun agentel-diff--face (line)
  "Return the `diff-mode' face of the diff LINE."
  (cond ((string-match-p "\\`\\(---\\|\\+\\+\\+\\) " line) 'diff-file-header)
        ((string-match-p "\\`\\(diff \\|new file\\|deleted file\\|index \\|old mode\\|new mode\\)"
                         line)
         'diff-header)
        ((string-prefix-p "@@" line) 'diff-hunk-header)
        ((string-prefix-p "-" line) 'diff-removed)
        ((string-prefix-p "+" line) 'diff-added)))

(defun agentel-diff-patch (patch)
  "Return the unified diff PATCH with the faces of `diff-mode'."
  (mapconcat (lambda (line)
               (if-let* ((face (agentel-diff--face line)))
                   (propertize line 'face face)
                 line))
             (split-string (string-trim-right patch "\n") "\n")
             "\n"))

(defun agentel-diff (path old new)
  "Return the unified diff of the file PATH changed from OLD to NEW.
OLD is nil for a file that is created.  The diff is nil when nothing
changes."
  (let ((old-file (agentel-diff--file (or old "")))
        (new-file (agentel-diff--file (or new ""))))
    (unwind-protect
        (with-temp-buffer
          (call-process diff-command nil t nil "-u" "--label" path "--label" path
                        old-file new-file)
          (unless (zerop (buffer-size))
            (agentel-diff-patch (buffer-string))))
      (delete-file old-file)
      (delete-file new-file))))

(provide 'agentel-diff)
;;; agentel-diff.el ends here
