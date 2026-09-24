;;; agentel-focus.el --- Show only what the next input needs  -*- lexical-binding: t; -*-

;;; Commentary:

;; `agentel-focus-mode' hides the transcript except what the user
;; answers next: the last prompt, the questions still waiting for an
;; answer and errors of the turn, and then the last agent message once
;; the turn ended, or the latest tool call or subagent while it runs.
;; A subagent is also shown while it waits for an answer.
;;
;; Hidden entries are covered by overlays, so the transcript text and
;; its rendering stay as they are, and turning the mode off shows
;; everything again.
;;
;; A streamed chunk changes one entry, so only that entry is looked at
;; again; the cost does not grow with the session.  Everything before
;; the last prompt is covered by a single overlay, and the entries of
;; the current turn are kept in a list.  An overlay of a hidden entry
;; follows text inserted in front of it and not behind it, so it keeps
;; covering its entry when the neighbours are rendered again.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-chat)

(defvar-local agentel-focus--turn nil
  "Entries of the current turn, newest first.")

(defvar-local agentel-focus--overlays nil
  "Hash table from each entry of the current turn to its overlay.
The overlay is nil while the entry is shown.")

(defvar-local agentel-focus--turn-start nil
  "Start marker of the last prompt, or nil without one.")

(defvar-local agentel-focus--before nil
  "Overlay hiding the transcript before the current turn.")

(defvar-local agentel-focus--latest nil
  "Entry shown as the answer or the current activity of the turn.")

(defun agentel-focus--activity ()
  "Return the entry types that show the state of the turn."
  (let ((session agentel-chat--session))
    (if (and session (agentel-session-busy session)) '(tool subagent) '(agent))))

(defun agentel-focus--waits-p (entry)
  "Return non-nil if ENTRY shows something owing an answer."
  (let ((item (agentel-chat-entry-get entry 'item))
        (child (agentel-chat-entry-get entry 'child)))
    (or (and item agentel-chat--session
             (memq item (agentel-session-pending agentel-chat--session)))
        (and child (agentel-session-pending-items child)))))

(defun agentel-focus--hidden-p (entry)
  "Return non-nil if ENTRY of the current turn is hidden."
  (not (or (eq entry agentel-focus--latest)
           (memq (agentel-chat-entry-type entry) '(user error))
           (agentel-focus--waits-p entry))))

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

(defun agentel-focus--refresh ()
  "Show or hide every entry of the current turn."
  (let ((activity (agentel-focus--activity)))
    (setq agentel-focus--latest
          (seq-find (lambda (e) (memq (agentel-chat-entry-type e) activity))
                    agentel-focus--turn)))
  (dolist (entry agentel-focus--turn)
    (agentel-focus--fix entry)))

(defun agentel-focus--clear ()
  "Remove the overlays of this buffer."
  (when agentel-focus--overlays
    (maphash (lambda (_ overlay) (when overlay (delete-overlay overlay)))
             agentel-focus--overlays))
  (when agentel-focus--before
    (delete-overlay agentel-focus--before))
  (setq agentel-focus--overlays (make-hash-table :test 'eq)
        agentel-focus--before nil
        agentel-focus--turn nil
        agentel-focus--turn-start nil
        agentel-focus--latest nil))

(defun agentel-focus--start-turn (entries)
  "Make ENTRIES, newest first, the current turn.
The oldest of them is the last prompt, unless there is none."
  (agentel-focus--clear)
  (setq agentel-focus--turn entries)
  (dolist (entry entries)
    (puthash entry nil agentel-focus--overlays))
  (when-let* ((prompt (car (last entries)))
              ((eq (agentel-chat-entry-type prompt) 'user)))
    (setq agentel-focus--turn-start (agentel-chat-entry-start prompt))
    (when (> agentel-focus--turn-start (point-min))
      ;; The blank lines in front of the prompt are hidden too.
      (setq agentel-focus--before
            (make-overlay (point-min) (+ agentel-focus--turn-start 2)))
      (overlay-put agentel-focus--before 'invisible 'agentel-focus)))
  (agentel-focus--refresh))

(defun agentel-focus--last-turn ()
  "Return the entries from the last prompt on, newest first."
  (let ((pos (marker-position agentel-chat--transcript-end))
        entries)
    (while (> pos (point-min))
      (setq pos (previous-single-property-change pos 'agentel-chat-entry nil
                                                 (point-min)))
      (when-let* ((entry (get-text-property pos 'agentel-chat-entry)))
        (push entry entries)
        (when (eq (agentel-chat-entry-type entry) 'user)
          (setq pos (point-min)))))
    (nreverse entries)))

(defun agentel-focus--on-entry-changed (entry)
  "Show or hide ENTRY, which was added or changed."
  (cond ((not (eq (gethash entry agentel-focus--overlays 'absent) 'absent))
         (agentel-focus--fix entry t))
        ((eq (agentel-chat-entry-type entry) 'user)
         (agentel-focus--start-turn (list entry)))
        ((and agentel-focus--turn-start
              (< (agentel-chat-entry-start entry) agentel-focus--turn-start)))
        (t
         (push entry agentel-focus--turn)
         (puthash entry nil agentel-focus--overlays)
         (when (memq (agentel-chat-entry-type entry) (agentel-focus--activity))
           (let ((previous agentel-focus--latest))
             (setq agentel-focus--latest entry)
             (when previous (agentel-focus--fix previous))))
         (agentel-focus--fix entry))))

;;;###autoload
(define-minor-mode agentel-focus-mode
  "Show only what the next input to the session needs.
The last prompt stays visible, with the questions waiting for an
answer, errors of the turn, and the last agent message once the turn
ended or the latest tool call or subagent while it runs."
  :lighter " Focus"
  (if agentel-focus-mode
      (progn
        (add-to-invisibility-spec 'agentel-focus)
        (add-hook 'agentel-chat-entry-changed-functions #'agentel-focus--on-entry-changed nil t)
        (agentel-focus--start-turn (agentel-focus--last-turn)))
    (remove-from-invisibility-spec 'agentel-focus)
    (remove-hook 'agentel-chat-entry-changed-functions #'agentel-focus--on-entry-changed t)
    (agentel-focus--clear)))

(defun agentel-focus--on-changed (session)
  "Follow the state of SESSION in its buffer."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when agentel-focus-mode
        (agentel-focus--refresh)))))

(add-hook 'agentel-session-changed-functions #'agentel-focus--on-changed)
(keymap-set agentel-chat-mode-map "C-c C-f" #'agentel-focus-mode)

(provide 'agentel-focus)
;;; agentel-focus.el ends here
