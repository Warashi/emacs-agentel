;;; agentel-ui.el --- Display components shared by agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Components that decide how things look regardless of what they show,
;; such as how wide a summary line may be or how a state is shown.  Features build their lines
;; with them, so the look changes in one place.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom agentel-ui-max-line-width 80
  "Largest width of a summary line, including its prefix."
  :type 'natnum
  :group 'agentel)

(defun agentel-ui-line-width ()
  "Return the width of a summary line in this buffer.
It is the narrowest window showing the buffer, up to
`agentel-ui-max-line-width'."
  ;; `window-max-chars-per-line' selects the window, which moves point
  ;; to the point of that window.
  (save-excursion
    (apply #'min agentel-ui-max-line-width
           (mapcar #'window-max-chars-per-line
                   (get-buffer-window-list nil nil t)))))

(defun agentel-ui-one-line (prefix text)
  "Return PREFIX followed by the first line of TEXT as one summary line.
The line fits in `agentel-ui-line-width' and ends in … when part of
TEXT is left out.  It carries the `agentel-ui-fits-width' property, so
text that has to be made again when the width changes can be found."
  (let* ((width (agentel-ui-line-width))
         (lines (split-string (string-trim (or text "")) "\n"))
         (line (concat prefix (car lines))))
    (propertize
     (if (cdr lines)
         (concat (truncate-string-to-width line (- width (string-width "…")))
                 "…")
       (truncate-string-to-width line width nil nil "…"))
     'agentel-ui-fits-width t)))

(defvar agentel-ui--followers nil
  "Buffers told when their line width changes.")

(defvar-local agentel-ui--width nil
  "Line width of this buffer when it was last checked.")

(defvar-local agentel-ui--on-width-change nil
  "Function called with no arguments when the line width changes.")

(defun agentel-ui-follow-width (function)
  "Call FUNCTION in this buffer whenever its line width changes.
FUNCTION makes the text depending on `agentel-ui-line-width' again."
  (setq agentel-ui--width (agentel-ui-line-width))
  (setq agentel-ui--on-width-change function)
  (cl-pushnew (current-buffer) agentel-ui--followers)
  ;; Window hooks local to a buffer miss a window that stops showing it.
  (add-hook 'window-size-change-functions #'agentel-ui--check-widths)
  (add-hook 'window-buffer-change-functions #'agentel-ui--check-widths))

(defun agentel-ui--check-widths (&rest _)
  "Tell the followers whose line width changed."
  (setq agentel-ui--followers
        (seq-filter (lambda (buffer)
                      ;; Changing the major mode kills the local function.
                      (and (buffer-live-p buffer)
                           (buffer-local-value 'agentel-ui--on-width-change buffer)))
                    agentel-ui--followers))
  (dolist (buffer agentel-ui--followers)
    (with-current-buffer buffer
      (let ((width (agentel-ui-line-width)))
        (unless (eql width agentel-ui--width)
          (setq agentel-ui--width width)
          (funcall agentel-ui--on-width-change))))))

(defcustom agentel-ui-state-icons
  '((starting . "⏳") (running . "🏃") (waiting . "🙋") (idle . "💤")
    (paused . "💤") (completed . "✅") (failed . "❌") (cancelled . "🚫")
    (exited . "🔌"))
  "Icons shown for the states of sessions and background tasks.
Each icon should take the same width in every terminal, so an emoji
that needs a variation selector to be wide does not fit."
  :type '(alist :key-type symbol :value-type string)
  :group 'agentel)

(defun agentel-ui-state (state)
  "Return how the symbol STATE is shown.
It is the icon of STATE in `agentel-ui-state-icons', naming STATE on
hover, or STATE in brackets when it has no icon."
  (if-let* ((icon (alist-get state agentel-ui-state-icons)))
      (propertize icon 'help-echo (symbol-name state))
    (format "[%s]" state)))

(provide 'agentel-ui)
;;; agentel-ui.el ends here
