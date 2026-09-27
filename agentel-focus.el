;;; agentel-focus.el --- Show only what the next input needs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `agentel-focus-mode' hides the transcript except what the user
;; answers next: the last prompt, the questions still waiting for an
;; answer, errors and why the turn ended, the last agent message, and
;; the latest thought or tool call when it came after that message.  A
;; subagent is shown while it waits for an answer; while it runs it is
;; pinned above the prompt instead.
;;
;; Hidden entries are covered by overlays, so the transcript text and
;; its rendering stay as they are, and turning the mode off shows
;; everything again.
;;
;; Whether an entry is shown depends on the entry itself and on the
;; entries that came after it, so a changed entry and a new entry are
;; the only ones looked at again; the cost does not grow with the
;; session or the turn.  Everything before the last prompt is covered by a
;; single overlay.  An overlay of a hidden entry follows text inserted
;; in front of it and not behind it, so it keeps covering its entry when
;; the neighbours are rendered again.

;;; Code:

(require 'subr-x)
(require 'agentel-chat)

(defvar-local agentel-focus--overlays nil
  "Hash table from each entry of the current turn to its overlay.
The overlay is nil while the entry is shown.")

(defvar-local agentel-focus--turn-start nil
  "Start marker of the last prompt, or nil without one.")

(defvar-local agentel-focus--before nil
  "Overlay hiding the transcript before the current turn.")

(defvar-local agentel-focus--last-message nil
  "Newest agent message of the current turn.")

(defvar-local agentel-focus--last-activity nil
  "Newest thought or tool call of the current turn after its last message.")

(defvar-local agentel-focus--latest nil
  "Entries shown as the last message and the current activity of the turn.")

(defun agentel-focus--current-latest ()
  "Return the entries that show the state of the turn."
  (delq nil (list agentel-focus--last-message agentel-focus--last-activity)))

(defun agentel-focus--hidden-p (entry)
  "Return non-nil if ENTRY of the current turn is hidden."
  (not (or (memq entry agentel-focus--latest)
           (memq (agentel-store-model-type entry) '(user error stop))
           (agentel-store-get entry 'waiting))))

(defun agentel-focus--entry-end (entry)
  "Return the position after the text of ENTRY."
  (next-single-property-change (agentel-chat-entry-start entry)
                               'agentel-chat-entry nil
                               (marker-position agentel-chat--transcript-end)))

(defun agentel-focus--fix (entry &optional moved)
  "Show or hide ENTRY of the current turn.
MOVED means its text changed, so a hidden entry is covered again."
  (let ((overlay (gethash entry agentel-focus--overlays)))
    (cond ((not (agentel-focus--hidden-p entry))
           (when overlay
             (delete-overlay overlay)
             (puthash entry nil agentel-focus--overlays)))
          ((not overlay)
           (let ((overlay (make-overlay (agentel-chat-entry-start entry)
                                        (agentel-focus--entry-end entry)
                                        nil t nil)))
             (overlay-put overlay 'invisible 'agentel-focus)
             (puthash entry overlay agentel-focus--overlays)))
          (moved
           (move-overlay overlay (agentel-chat-entry-start entry)
                         (agentel-focus--entry-end entry))))))

(defun agentel-focus--add (entry)
  "Record ENTRY as the newest of the current turn."
  (puthash entry nil agentel-focus--overlays)
  (pcase (agentel-store-model-type entry)
    ('agent (setq agentel-focus--last-message entry
                  agentel-focus--last-activity nil))
    ((or 'thought 'tool) (setq agentel-focus--last-activity entry))))

(defun agentel-focus--refresh ()
  "Show or hide the entries that a new entry can take the place of."
  (let ((previous agentel-focus--latest))
    (setq agentel-focus--latest (agentel-focus--current-latest))
    (mapc #'agentel-focus--fix previous)
    (mapc #'agentel-focus--fix agentel-focus--latest)))

(defun agentel-focus--clear ()
  "Remove the overlays of this buffer."
  (when agentel-focus--overlays
    (maphash (lambda (_ overlay) (when overlay (delete-overlay overlay)))
             agentel-focus--overlays))
  (when agentel-focus--before
    (delete-overlay agentel-focus--before))
  (setq agentel-focus--overlays (make-hash-table :test 'eq)
        agentel-focus--before nil
        agentel-focus--turn-start nil
        agentel-focus--last-message nil
        agentel-focus--last-activity nil
        agentel-focus--latest nil))

(defun agentel-focus--start-turn (entries)
  "Make ENTRIES, oldest first, the current turn.
The first of them is the last prompt, unless there is none."
  (agentel-focus--clear)
  (mapc #'agentel-focus--add entries)
  (when-let* ((prompt (car entries))
              ((eq (agentel-store-model-type prompt) 'user)))
    (setq agentel-focus--turn-start (agentel-chat-entry-start prompt))
    (when (> agentel-focus--turn-start (point-min))
      ;; The blank lines in front of the prompt are hidden too.
      (setq agentel-focus--before
            (make-overlay (point-min) (+ agentel-focus--turn-start 2)))
      (overlay-put agentel-focus--before 'invisible 'agentel-focus)))
  (setq agentel-focus--latest (agentel-focus--current-latest))
  (mapc #'agentel-focus--fix entries))

(defun agentel-focus--last-turn ()
  "Return the entries from the last prompt on, oldest first."
  (let ((pos (marker-position agentel-chat--transcript-end))
        entries)
    (while (> pos (point-min))
      (setq pos (previous-single-property-change pos 'agentel-chat-entry nil
                                                 (point-min)))
      (when-let* ((entry (get-text-property pos 'agentel-chat-entry)))
        (push entry entries)
        (when (eq (agentel-store-model-type entry) 'user)
          (setq pos (point-min)))))
    entries))

(defun agentel-focus--on-entry-changed (entry)
  "Show or hide ENTRY, which was added or changed."
  (cond ((not (eq (gethash entry agentel-focus--overlays 'absent) 'absent))
         (agentel-focus--fix entry t))
        ((eq (agentel-store-model-type entry) 'user)
         (agentel-focus--start-turn (list entry)))
        ((and agentel-focus--turn-start
              (< (agentel-chat-entry-start entry) agentel-focus--turn-start)))
        (t
         (agentel-focus--add entry)
         (agentel-focus--refresh)
         (agentel-focus--fix entry))))

;;;###autoload
(define-minor-mode agentel-focus-mode
  "Show only what the next input to the session needs.
The last prompt stays visible, with the questions waiting for an
answer, errors and why the turn ended, the last agent message, and
the latest thought or tool call when it came after that message."
  :lighter " Focus"
  (if agentel-focus-mode
      (progn
        (add-to-invisibility-spec 'agentel-focus)
        (add-hook 'agentel-chat-entry-changed-functions #'agentel-focus--on-entry-changed nil t)
        (agentel-focus--start-turn (agentel-focus--last-turn)))
    (remove-from-invisibility-spec 'agentel-focus)
    (remove-hook 'agentel-chat-entry-changed-functions #'agentel-focus--on-entry-changed t)
    (agentel-focus--clear)))

(keymap-set agentel-chat-mode-map "C-c C-f" #'agentel-focus-mode)

(provide 'agentel-focus)
;;; agentel-focus.el ends here
