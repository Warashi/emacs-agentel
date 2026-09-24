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

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-chat)

(defun agentel-focus--entries ()
  "Return the entries of this buffer in order, as (ENTRY START END)."
  (let ((end (marker-position agentel-chat--transcript-end))
        (pos (point-min))
        entries)
    (while (< pos end)
      (let ((next (next-single-property-change pos 'agentel-chat-entry nil end)))
        (when-let* ((entry (get-text-property pos 'agentel-chat-entry)))
          (push (list entry pos next) entries))
        (setq pos next)))
    (nreverse entries)))

(defun agentel-focus--waits-p (session entry)
  "Return non-nil if ENTRY of SESSION shows something owing an answer."
  (let ((item (agentel-chat-entry-get entry 'item))
        (child (agentel-chat-entry-get entry 'child)))
    (or (and item (memq item (agentel-session-pending session)))
        (and child (agentel-session-pending-items child)))))

(defun agentel-focus--shown (session entries)
  "Return the entries among ENTRIES of SESSION that stay visible."
  (let* ((turn (or (car (last (cl-loop for tail on entries
                                       when (eq (agentel-chat-entry-type (car tail)) 'user)
                                       collect tail)))
                   entries))
         (activity (if (agentel-session-busy session) '(tool subagent) '(agent)))
         (latest (car (last (seq-filter
                             (lambda (e) (memq (agentel-chat-entry-type e) activity))
                             turn)))))
    (seq-filter (lambda (entry)
                  (or (eq entry latest)
                      (memq (agentel-chat-entry-type entry) '(user error))
                      (agentel-focus--waits-p session entry)))
                turn)))

(defun agentel-focus--hide (start end)
  "Hide the text from START to END."
  (overlay-put (make-overlay start end) 'invisible 'agentel-focus))

(defun agentel-focus--update ()
  "Hide what the next input does not need in this buffer."
  (remove-overlays (point-min) (point-max) 'invisible 'agentel-focus)
  (when-let* ((session agentel-chat--session))
    (let* ((entries (agentel-focus--entries))
           (shown (agentel-focus--shown session (mapcar #'car entries)))
           (first t))
      (pcase-dolist (`(,entry ,start ,end) entries)
        (cond ((not (memq entry shown)) (agentel-focus--hide start end))
              (first
               (setq first nil)
               ;; The blank lines separating it from hidden entries above.
               (when (> start (point-min))
                 (agentel-focus--hide start (+ start 2)))))))))

(defun agentel-focus--on-entry-changed (_entry)
  "Follow a change of the transcript of this buffer."
  (agentel-focus--update))

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
        (agentel-focus--update))
    (remove-from-invisibility-spec 'agentel-focus)
    (remove-hook 'agentel-chat-entry-changed-functions #'agentel-focus--on-entry-changed t)
    (remove-overlays (point-min) (point-max) 'invisible 'agentel-focus)))

(defun agentel-focus--on-changed (session)
  "Follow the state of SESSION in its buffer."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when agentel-focus-mode
        (agentel-focus--update)))))

(add-hook 'agentel-session-changed-functions #'agentel-focus--on-changed)
(keymap-set agentel-chat-mode-map "C-c C-f" #'agentel-focus-mode)

(provide 'agentel-focus)
;;; agentel-focus.el ends here
