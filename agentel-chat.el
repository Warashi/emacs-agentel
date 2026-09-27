;;; agentel-chat.el --- Session buffers for agentel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Shinnosuke Sawada-Dazai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Each session is shown in its own buffer: a read-only transcript
;; followed by an input area where the prompt is edited like any other
;; text.
;;
;; The transcript shows the conversation of the session, whose items
;; (messages, thoughts, tool calls, ...) are its entries; see
;; `agentel-conversation'.  The conversation outlives the buffer showing
;; it, and the buffer subscribes to it.  An entry owns the text carrying
;; its item in the `agentel-chat-entry' property, starting at its start
;; marker.  An entry is changed by rendering it again and replacing
;; exactly that text, and new entries are always inserted at the end of
;; the transcript, so updating an older entry never moves text that
;; belongs to another one.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agentel-session)
(require 'agentel-connection)
(require 'agentel-conversation)
(require 'agentel-store)
(require 'agentel-ui)

(defgroup agentel nil
  "Agent Client Protocol client."
  :group 'tools
  :prefix "agentel-")

(defface agentel-chat-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face of messages written by the user.")

(defface agentel-chat-thought-face
  '((t :inherit shadow :slant italic))
  "Face of the agent's thoughts.")

(defface agentel-chat-tool-face
  '((t :inherit font-lock-function-name-face))
  "Face of tool call titles.")

(defface agentel-chat-tool-body-face
  '((t :inherit shadow))
  "Face of tool call output.")

(defface agentel-chat-notice-face
  '((t :inherit font-lock-comment-face))
  "Face of notices from agentel.")

(defface agentel-chat-error-face
  '((t :inherit error))
  "Face of errors.")

(defface agentel-chat-prompt-face
  '((t :inherit minibuffer-prompt))
  "Face of the prompt in front of the input area.")

(defvar agentel-chat-header-functions nil
  "Functions returning segments of the header line.
Each is called with the session and returns a string or nil.")

(defvar agentel-chat-pin-functions nil
  "Functions returning lines pinned above the prompt.
Each is called with the session and returns a list of strings.  The
lines are shown again whenever the session changes, or when
`agentel-chat-refresh-pin' is called.")

(defvar agentel-chat-send-functions nil
  "Abnormal hook run with the session and the input when it is sent.
If a function returns non-nil, it handled the input and it is not
sent to the agent as a prompt.")

(defvar agentel-chat-format-message-function nil
  "Function returning a finished agent message text formatted for display.
It is called with the text once the message is complete, so it never
sees half of a construct, such as a code block, that spans chunks.")

(defvar agentel-chat-entry-changed-functions nil
  "Abnormal hook run in a session buffer after the text of an entry changed.
Each function is called with the entry, once it was added, rendered
again or extended.")

(defvar agentel-chat-prompt-string "❯ "
  "String in front of the input area.")

(defvar-local agentel-chat--session nil
  "Session shown in this buffer.")

(defvar-local agentel-chat--starts nil
  "Hash table of the start markers of the entries of this buffer.")

(defvar-local agentel-chat--collapsed nil
  "Hash table of the entries folded in this buffer.")

(defvar-local agentel-chat--transcript-end nil
  "Marker at the end of the transcript.")

(defvar-local agentel-chat--input-start nil
  "Marker at the start of the input area, or nil without one.")

(defvar-local agentel-chat--pin nil
  "Overlay on the prompt that shows the pinned lines, or nil without one.")

(defvar-local agentel-chat--history nil
  "Inputs sent from this buffer, newest first.")

(defvar-local agentel-chat--history-index nil
  "Position in `agentel-chat--history' while browsing it.")

;;;; Keymaps

(defvar-keymap agentel-chat-entry-map
  :doc "Keymap on foldable transcript entries."
  "TAB" #'agentel-chat-toggle
  "RET" #'agentel-chat-toggle
  "<mouse-1>" #'agentel-chat-toggle)

(defvar-keymap agentel-chat-mode-map
  :doc "Keymap of `agentel-chat-mode'."
  "TAB" #'completion-at-point
  "C-c C-c" #'agentel-chat-send
  "C-c C-k" #'agentel-chat-cancel
  "C-c C-a" #'agentel-chat-answer
  "C-c C-i" #'agentel-chat-goto-input
  "M-p" #'agentel-chat-previous-input
  "M-n" #'agentel-chat-next-input)

;;;; Entries

(defmacro agentel-chat--with-transcript (&rest body)
  "Run BODY, which changes the read-only transcript.
The change is not recorded for undo.  Undo records positions, and
recorded input edits would point at the wrong text after the
transcript grows above them, so the undo history is dropped."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     (prog1 (let ((buffer-undo-list t)) ,@body)
       (unless (eq buffer-undo-list t)
         (setq buffer-undo-list nil)))))

;;;; Transcript

(defun agentel-chat-transcript (session)
  "Return the store of the conversation of SESSION."
  (agentel-conversation--store session))

(defun agentel-chat-finish-message (session)
  "Mark the agent message at the end of the conversation of SESSION as complete."
  (agentel-conversation-finish-message session))

(defun agentel-chat-notice (session text &optional type)
  "Add the notice TEXT to the conversation of SESSION.
TYPE is `error' for errors and `stop' for why a turn ended early."
  (agentel-conversation-note session text type))

;;;; Entries

(defun agentel-chat-entry-start (entry)
  "Return the marker at the start of the text of ENTRY in this buffer."
  (gethash entry agentel-chat--starts))

(defun agentel-chat--entry-end (entry)
  "Return the position after the text of ENTRY."
  (next-single-property-change (agentel-chat-entry-start entry)
                               'agentel-chat-entry nil
                               (marker-position agentel-chat--transcript-end)))

(defun agentel-chat--propertize (entry string)
  "Return STRING marked as text of ENTRY."
  (let ((string (copy-sequence string)))
    (add-text-properties 0 (length string)
                         (list 'agentel-chat-entry entry
                               'read-only t
                               'rear-nonsticky t
                               'front-sticky '(read-only))
                         string)
    string))

(defun agentel-chat--entry-string (entry at)
  "Return the text of ENTRY when it starts at position AT."
  (agentel-chat--propertize
   entry
   (concat (if (> at (point-min)) "\n\n" "")
           (agentel-ui-view entry
                            :collapsed (gethash entry agentel-chat--collapsed)
                            :width (agentel-ui-line-width)))))

(defun agentel-chat--insert (entry)
  "Append the text of ENTRY to the transcript."
  (when (agentel-ui-view-property (agentel-store-model-type entry) :collapsed)
    (puthash entry t agentel-chat--collapsed))
  (agentel-chat--with-transcript
    (save-excursion
      (goto-char agentel-chat--transcript-end)
      ;; The gap in front of the prompt is hidden while nothing was said.
      (when (and agentel-chat--input-start (= (point) (point-min)))
        (remove-text-properties (point) (+ (point) 2) '(invisible nil)))
      (puthash entry (copy-marker (point)) agentel-chat--starts)
      (insert (agentel-chat--entry-string entry (point)))))
  (run-hook-with-args 'agentel-chat-entry-changed-functions entry))

(defun agentel-chat--refresh (entry)
  "Render ENTRY again in place."
  (let* ((start (marker-position (agentel-chat-entry-start entry)))
         (end (agentel-chat--entry-end entry))
         (offset (and (<= start (point)) (< (point) end) (- (point) start))))
    (agentel-chat--with-transcript
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert (agentel-chat--entry-string entry start))
        ;; The start of the next entry was left in front of the new text.
        (when-let* ((next (and (< (point) agentel-chat--transcript-end)
                               (agentel-chat-entry-at (point)))))
          (set-marker (agentel-chat-entry-start next) (point)))))
    (run-hook-with-args 'agentel-chat-entry-changed-functions entry)
    (when offset
      (goto-char (min (+ start offset) (agentel-chat--entry-end entry))))))

(defun agentel-chat--show (session)
  "Show the conversation of SESSION in this buffer and follow its changes."
  (mapc #'agentel-chat--insert (agentel-conversation-items session))
  (let* ((buffer (current-buffer))
         (subscriber (lambda (entry added)
                       (with-current-buffer buffer
                         (if added
                             (agentel-chat--insert entry)
                           (agentel-chat--refresh entry)))))
         (unsubscribe (lambda () (agentel-conversation-unsubscribe session subscriber))))
    (agentel-conversation-subscribe session subscriber)
    ;; Changing the major mode forgets the entries of the buffer.
    (add-hook 'change-major-mode-hook unsubscribe nil t)
    (add-hook 'kill-buffer-hook unsubscribe nil t)))

(defun agentel-chat-entry-at (&optional pos)
  "Return the entry at POS, which defaults to point."
  (get-text-property (or pos (point)) 'agentel-chat-entry))

(defun agentel-chat-toggle ()
  "Fold or unfold the entry at point."
  (interactive)
  (when-let* ((entry (agentel-chat-entry-at)))
    (puthash entry (not (gethash entry agentel-chat--collapsed))
             agentel-chat--collapsed)
    (agentel-chat--refresh entry)))

;;;; Rendering

(defun agentel-chat--render-text (entry _options)
  "Render the text message ENTRY."
  (let ((text (agentel-store-get entry 'text)))
    (pcase (agentel-store-model-type entry)
      ('user (propertize (concat agentel-chat-prompt-string text)
                         'face 'agentel-chat-user-face))
      ('agent (if (and (agentel-store-get entry 'finished)
                       agentel-chat-format-message-function)
                  (funcall agentel-chat-format-message-function text)
                text)))))

(agentel-ui-define-view 'user #'agentel-chat--render-text)

(agentel-ui-define-view 'agent #'agentel-chat--render-text)

(defun agentel-chat--render-thought (entry options)
  "Render the thought ENTRY, folded to its first line when OPTIONS say so.
The folded line fits in the :width of OPTIONS."
  (let ((text (agentel-store-get entry 'text)))
    (propertize
     (if (plist-get options :collapsed)
         (agentel-ui-one-line "▸ Thinking: " text (plist-get options :width))
       (concat "▾ Thinking\n" text))
     'face 'agentel-chat-thought-face
     'keymap agentel-chat-entry-map)))

(agentel-ui-define-view 'thought #'agentel-chat--render-thought :collapsed t)

(defun agentel-chat--render-notice (entry _options)
  "Render the notice ENTRY."
  (propertize (agentel-store-get entry 'text)
              'face (if (eq (agentel-store-model-type entry) 'error)
                        'agentel-chat-error-face
                      'agentel-chat-notice-face)))

(dolist (type '(notice error stop))
  (agentel-ui-define-view type #'agentel-chat--render-notice))

(defconst agentel-chat--status-icons
  '(("pending" . "…") ("in_progress" . "⟳") ("completed" . "✓") ("failed" . "✗"))
  "Icons of tool call statuses.")

(defun agentel-chat--content-text (content)
  "Return the displayable text of the tool call CONTENT list."
  (string-join
   (delq nil
         (mapcar
          (lambda (item)
            (pcase (alist-get 'type item)
              ("content" (alist-get 'text (alist-get 'content item)))
              ("diff" (format "%s %s" (if (alist-get 'oldText item) "Edit" "Write")
                              (alist-get 'path item)))
              ("terminal" nil)))
          content))
   "\n"))

(defun agentel-chat--indent (text)
  "Return TEXT with every line indented."
  (replace-regexp-in-string "^" "    " text))

(defun agentel-chat--render-tool (entry options)
  "Render the tool call ENTRY, folded when OPTIONS say so.
The header is one line in the :width of OPTIONS, led by why the tool
is called when the agent tells it; a title cut short there shows in
full on top of the output."
  (let* ((status (agentel-store-get entry 'status))
         (collapsed (plist-get options :collapsed))
         (icon (or (cdr (assoc status agentel-chat--status-icons)) "…"))
         (title (propertize (or (agentel-store-get entry 'title) "Tool")
                            'face 'agentel-chat-tool-face))
         (why (alist-get 'description (agentel-store-get entry 'rawInput)))
         (summary (if (and (stringp why) (not (equal why title)))
                      (concat (propertize (concat why " — ") 'face 'agentel-chat-tool-face)
                              title)
                    title))
         (line (lambda (marker)
                 (agentel-ui-one-line (concat marker icon " ") summary
                                      (plist-get options :width))))
         (cut (not (equal (funcall line "  ")
                          (concat "  " icon " " (string-trim summary)))))
         (output (agentel-chat--content-text (agentel-store-get entry 'content)))
         (body (string-join (delete "" (list (if cut title "") output)) "\n"))
         (foldable (not (string-empty-p body)))
         (header (funcall line (cond ((not foldable) "  ")
                                     (collapsed "▸ ")
                                     (t "▾ ")))))
    (propertize
     (if (or (not foldable) collapsed)
         header
       (concat header "\n"
               (propertize (agentel-chat--indent (string-trim-right body))
                           'face 'agentel-chat-tool-body-face)))
     'keymap agentel-chat-entry-map)))

(agentel-ui-define-view 'tool #'agentel-chat--render-tool :collapsed t)

(defconst agentel-chat--plan-marks
  '(("completed" . "[x]") ("in_progress" . "[-]") ("pending" . "[ ]"))
  "Check boxes of plan entry statuses.")

(defun agentel-chat--render-plan (entry _options)
  "Render the plan ENTRY."
  (concat
   (propertize "Plan" 'face 'bold)
   (mapconcat (lambda (item)
                (format "\n  %s %s"
                        (or (cdr (assoc (alist-get 'status item)
                                        agentel-chat--plan-marks))
                            "[ ]")
                        (alist-get 'content item)))
              (agentel-store-get entry 'entries) "")))

(agentel-ui-define-view 'plan #'agentel-chat--render-plan)

;;;; Input

(defun agentel-chat-input ()
  "Return the text of the input area."
  (if agentel-chat--input-start
      (buffer-substring-no-properties agentel-chat--input-start (point-max))
    ""))

(defun agentel-chat--set-input (text)
  "Replace the input area with TEXT."
  (delete-region agentel-chat--input-start (point-max))
  (goto-char (point-max))
  (insert text))

(defun agentel-chat--insert-prompt ()
  "Insert the prompt and the input area after the transcript."
  (agentel-chat--with-transcript
    (goto-char (point-max))
    (let ((end (point)))
      (insert (propertize (concat (propertize "\n\n" 'invisible (= end (point-min)))
                                  agentel-chat-prompt-string)
                          'face 'agentel-chat-prompt-face
                          'read-only t
                          'front-sticky '(read-only)
                          'rear-nonsticky t
                          'field 'agentel-chat-prompt))
      ;; The transcript grows in front of the prompt, not after it.
      (set-marker agentel-chat--transcript-end end))
    ;; Nothing is ever inserted into the prompt, so an overlay on it
    ;; stays between the transcript and the input.
    (setq agentel-chat--pin
          (make-overlay (- (point) (length agentel-chat-prompt-string)) (point)))
    (setq agentel-chat--input-start (point-marker))))

(defun agentel-chat--finish-turn (session)
  "Record that the turn of SESSION ended."
  (agentel-session-set-busy session nil)
  (agentel-conversation-finish-message session))

(defun agentel-chat--prompt (session text)
  "Send TEXT to the agent as a prompt of SESSION."
  (agentel-session-set-busy session t)
  (agentel-connection-request
   (agentel-session-connection session) "session/prompt"
   `((sessionId . ,(agentel-session-id session))
     (prompt . [((type . "text") (text . ,text))]))
   :on-success
   (lambda (result)
     (agentel-chat--finish-turn session)
     (let ((reason (alist-get 'stopReason result)))
       (unless (member reason '("end_turn" nil))
         (agentel-conversation-note session (format "Turn ended: %s" reason) 'stop))))
   :on-failure
   (lambda (error)
     (agentel-chat--finish-turn session)
     (agentel-conversation-note session
                                (format "Prompt failed: %s" (alist-get 'message error))
                                'error))))

(defun agentel-chat-send ()
  "Send the input to the agent."
  (interactive)
  (let ((session agentel-chat--session)
        (text (string-trim (agentel-chat-input))))
    (unless agentel-chat--input-start
      (user-error "This session does not take input"))
    (when (agentel-session-ended session)
      (user-error "This session has ended"))
    (unless (string-empty-p text)
      (push text agentel-chat--history)
      (setq agentel-chat--history-index nil)
      (agentel-chat--set-input "")
      (unless (run-hook-with-args-until-success 'agentel-chat-send-functions
                                                session text)
        (agentel-conversation-prompt session text)
        (agentel-chat--prompt session text)))))

(defun agentel-chat-cancel ()
  "Ask the agent to stop the current turn."
  (interactive)
  (let ((session agentel-chat--session))
    (agentel-connection-notify (agentel-session-connection session)
                               "session/cancel"
                               `((sessionId . ,(agentel-session-id session))))))

(defun agentel-chat-previous-input (n)
  "Replace the input with the Nth previous sent input."
  (interactive "p")
  (when agentel-chat--history
    (setq agentel-chat--history-index
          (max 0 (min (1- (length agentel-chat--history))
                      (+ (or agentel-chat--history-index -1) n))))
    (agentel-chat--set-input (nth agentel-chat--history-index
                                  agentel-chat--history))))

(defun agentel-chat-next-input (n)
  "Replace the input with the Nth next sent input."
  (interactive "p")
  (if (and agentel-chat--history-index
           (>= (- agentel-chat--history-index n) 0))
      (agentel-chat-previous-input (- n))
    (setq agentel-chat--history-index nil)
    (agentel-chat--set-input "")))

(defun agentel-chat-ordered-completion (candidates)
  "Return a completion table of CANDIDATES that keeps their order."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string predicate))))

(defun agentel-chat-answer ()
  "Answer the oldest question of this session or of its subagents.
Questions are the pending items of sessions, plists whose :answer is a
command that asks the user and replies to the agent."
  (interactive)
  (let ((pending (agentel-session-pending-items agentel-chat--session)))
    (unless pending
      (user-error "No question is waiting for an answer"))
    (funcall (plist-get (cdar pending) :answer))))

(defun agentel-chat-goto-input ()
  "Move point to the end of the input area."
  (interactive)
  (goto-char (point-max)))

;;;; Mode

(defun agentel-chat--header-line ()
  "Return the header line of this buffer."
  (let ((session agentel-chat--session))
    (string-join
     (delq nil (cons (format "%s %s  │  %s"
                             (agentel-ui-state (agentel-session-state session))
                             (agentel-session-project-name session)
                             (agentel-session-name session))
                     (mapcar (lambda (f) (funcall f session))
                             agentel-chat-header-functions)))
     "  │  ")))

(define-derived-mode agentel-chat-mode text-mode "agentel"
  "Major mode of agentel session buffers.
The transcript is read-only; type the prompt at the end of the buffer
and send it with \\[agentel-chat-send]."
  (setq-local agentel-chat--starts (make-hash-table :test 'eq))
  (setq-local agentel-chat--collapsed (make-hash-table :test 'eq))
  (setq-local agentel-chat--transcript-end (point-min-marker))
  (set-marker-insertion-type agentel-chat--transcript-end t)
  ;; The result of :eval is itself a mode line format, where % is special.
  (setq-local header-line-format
              '(:eval (string-replace "%" "%%" (agentel-chat--header-line))))
  (agentel-ui-follow-width #'agentel-chat--fit-width))

(defun agentel-chat--fit-width ()
  "Render the entries and pinned lines that fit the line width again."
  (let ((pos (point-min)) entries)
    (while (< pos agentel-chat--transcript-end)
      (let* ((entry (agentel-chat-entry-at pos))
             (end (agentel-chat--entry-end entry)))
        (when (text-property-any pos end 'agentel-ui-fits-width t)
          (push entry entries))
        (setq pos end)))
    (mapc #'agentel-chat--refresh entries))
  (when agentel-chat--session
    (agentel-chat-refresh-pin agentel-chat--session)))

(defun agentel-chat-refresh-pin (session)
  "Show the pinned lines of SESSION above its prompt again."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when agentel-chat--pin
        (let* ((lines (mapcan (lambda (f) (copy-sequence (funcall f session)))
                              agentel-chat-pin-functions))
               (string (and lines (concat (string-join lines "\n") "\n"))))
          ;; Setting an equal string still makes the window redisplay.
          (unless (equal string (overlay-get agentel-chat--pin 'before-string))
            (overlay-put agentel-chat--pin 'before-string string)))))))

(defun agentel-chat--on-changed (session)
  "Redisplay the header line and the pinned lines of SESSION's buffer."
  (when-let* ((buffer (agentel-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (force-mode-line-update))
    (agentel-chat-refresh-pin session)))

(add-hook 'agentel-session-changed-functions #'agentel-chat--on-changed)

(defun agentel-chat--buffer-name (session)
  "Return a buffer name for SESSION, from its project if it has one."
  (generate-new-buffer-name
   (format "*agentel: %s*" (or (agentel-session-project session)
                               (agentel-session-name session)))))

(cl-defun agentel-chat-open (session &key input)
  "Create and return the buffer of SESSION.
With INPUT, the buffer has an input area for prompts."
  (let ((buffer (get-buffer-create (agentel-chat--buffer-name session))))
    (with-current-buffer buffer
      (agentel-chat-mode)
      (setq agentel-chat--session session)
      (when input (agentel-chat--insert-prompt))
      (agentel-chat--show session)
      (unless input (setq buffer-read-only t)))
    (setf (agentel-session-buffer session) buffer)
    buffer))

(defun agentel-chat-buffer (session)
  "Return the live buffer of SESSION, making a new one if it was killed.
Only a subagent outlives its buffer, so the new one has no input area."
  (let ((buffer (agentel-session-buffer session)))
    (if (buffer-live-p buffer)
        buffer
      (agentel-chat-open session :input (not (agentel-session-parent session))))))

(provide 'agentel-chat)
;;; agentel-chat.el ends here
