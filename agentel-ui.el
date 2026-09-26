;;; agentel-ui.el --- Display components shared by agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Components that decide how things look regardless of what they show,
;; such as how wide a summary line may be.  Features build their lines
;; with them, so the look changes in one place.

;;; Code:

(require 'subr-x)

(defcustom agentel-ui-max-line-width 80
  "Largest width of a summary line, including its prefix."
  :type 'natnum
  :group 'agentel)

(defun agentel-ui-line-width ()
  "Return the width of a summary line in this buffer.
It is the narrowest window showing the buffer, up to
`agentel-ui-max-line-width'."
  (apply #'min agentel-ui-max-line-width
         (mapcar #'window-max-chars-per-line
                 (get-buffer-window-list nil nil t))))

(defun agentel-ui-one-line (prefix text)
  "Return PREFIX followed by the first line of TEXT as one summary line.
The line fits in `agentel-ui-line-width' and ends in … when part of
TEXT is left out."
  (let* ((width (agentel-ui-line-width))
         (lines (split-string (string-trim (or text "")) "\n"))
         (line (concat prefix (car lines))))
    (if (cdr lines)
        (concat (truncate-string-to-width line (- width (string-width "…")))
                "…")
      (truncate-string-to-width line width nil nil "…"))))

(provide 'agentel-ui)
;;; agentel-ui.el ends here
